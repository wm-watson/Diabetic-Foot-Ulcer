# =============================================================================
# 10_pcp_hotspot.R
#
# Spatial hotspot analysis of common primary-care (E&M) visit rates. Tests
# whether the Delta cold-spot in DFU prevalence (script 09) reflects a
# general access-to-care problem or a DFU-specific pattern.
#
#   If PCP-visit cold-spots overlap the DFU cold-spot  ->  access-to-care artifact
#   If DFU cold-spot persists after adjusting for PCP  ->  DFU-specific signal
#
# Design mirrors R/09_payer_stratified_spatial.R:
#   - Same 12 half-year bins (2017-H1 .. 2022-H2)
#   - Same ZCTA-2020 vintage, adaptive KNN k=8, local EB, EHSA, Gi*/LISA
#   - Same suppression rule (n_dm_person_halfyears < DEN_THRESH -> NA)
#   - Same payer strata via payer_strata.csv (MEDICARE, MEDICAID, COMMERCIAL,
#     MIXED). Default primary comparison is COMMERCIAL and MEDICAID (rev-4
#     methodology; Medicare stratum available as sensitivity).
#
# Two denominators, controlled by DENOM env var:
#   DENOM=all  (Cohort A)  ->  whole APCD-enrolled population.
#                              Rate = distinct persons with >=1 PCP visit /
#                                     enrolled person-halfyears.
#   DENOM=dm   (Cohort B)  ->  diabetes cohort only.
#                              Rate = distinct DM persons with >=1 PCP visit /
#                                     DM person-halfyears.
#
# Inputs
# ------
#   step7 outputs (D:\WPWatson synced to ENROLL_DIR):
#     pcp_visits_bin.csv     long: apcd_unique_id, bin_id, has_pcp_visit, n_pcp_visits
#     all_enrollment.csv     wide: apcd_unique_id, gender, m_h1_YYYY, m_h2_YYYY, ...
#     all_zip_lookup.csv     apcd_unique_id, ar_zip
#   step3d output:
#     payer_strata.csv       apcd_unique_id, primary_payer_category
#   step3c output (for DENOM=dm intersect check):
#     cohort_continuous.csv  DM-restricted enrollment
#   step5 output:
#     dm_dfu_analytic.csv    used to derive DM apcd_unique_ids
#
# Outputs (per stratum x denominator, in outputs/pcp_spatial/)
#   zcta_sf_lisa_pcp_<stratum>_<denom>.rds
#   map_rate_pcp_<stratum>_<denom>.png
#   map_gi_pcp_<stratum>_<denom>.png
#
# Env vars
#   DFU_PAYER_STRATUM  MEDICAID | COMMERCIAL | MEDICARE | MIXED  (default COMMERCIAL)
#   PCP_DENOM          all | dm                                   (default dm)
#   DFU_COHORT         continuous | fractional                    (default continuous)
#   PCP_DIR            step7 output folder (defaults to Dropbox/DFU/)
#   DFU_ENROLL_DIR     existing analytical datasets (payer_strata.csv, cohort_continuous.csv)
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(sf)
    library(spdep)
    library(tigris)
    library(ggplot2)
    library(fs)
    library(viridis)
})

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ---- Paths ------------------------------------------------------------------
DROPBOX_DIR <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU")
ENROLL_DIR  <- Sys.getenv(
    "DFU_ENROLL_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/APCD/Analytical Datasets")
# Step 7 outputs (pcp_visits_bin.csv, all_zip_lookup.csv, and eventually
# all_enrollment.csv) live in a separate DFU folder unless overridden.
PCP_DIR <- Sys.getenv(
    "PCP_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/DFU")
ANALYTIC <- file.path(DROPBOX_DIR, "analytic")
OUT_DIR  <- file.path(DROPBOX_DIR, "outputs", "pcp_spatial")
dir_create(OUT_DIR)

STRATUM <- toupper(Sys.getenv("DFU_PAYER_STRATUM", "COMMERCIAL"))
# Default DENOM to "dm" until the all-population enrollment file is rebuilt
# (the initial step7 run exhausted D:\WPWatson mid-passthrough).
DENOM   <- tolower(Sys.getenv("PCP_DENOM", "dm"))
COHORT  <- Sys.getenv("DFU_COHORT", "continuous")
stopifnot(STRATUM %in% c("MEDICARE", "MEDICAID", "COMMERCIAL", "MIXED"))
stopifnot(DENOM   %in% c("all", "dm"))
stopifnot(COHORT  %in% c("continuous", "fractional"))
message("Stratum:  ", STRATUM)
message("Denom:    ", DENOM)
message("Cohort:   ", COHORT)

# ---- Parameters -------------------------------------------------------------
STUDY_YEARS     <- 2017:2022
KNN_K           <- 8L
NPERM           <- 999L
FDR_ALPHA       <- 0.05
DEN_THRESH      <- 20L
BIN_MONTH_FLOOR <- 3L
BIN_COLS <- c("m_h1_2017","m_h2_2017","m_h1_2018","m_h2_2018",
              "m_h1_2019","m_h2_2019","m_h1_2020","m_h2_2020",
              "m_h1_2021","m_h2_2021","m_h1_2022","m_h2_2022")
HALF_BIN_IDS <- c("2017-H1","2017-H2","2018-H1","2018-H2",
                  "2019-H1","2019-H2","2020-H1","2020-H2",
                  "2021-H1","2021-H2","2022-H1","2022-H2")

# ---- Load payer strata ------------------------------------------------------
strata <- fread(file.path(ENROLL_DIR, "payer_strata.csv"),
                colClasses = list(character = "apcd_unique_id"))
strata_ids <- strata[primary_payer_category == STRATUM, apcd_unique_id]
message("Persons in payer stratum ", STRATUM, ": ", length(strata_ids))

# ---- Load enrollment cohort (denominator base) ------------------------------
# For DENOM=all use step7's all_enrollment.csv (whole APCD population).
# For DENOM=dm use step3c's cohort_continuous / cohort_fractional (DM only).
if (DENOM == "all") {
    all_enroll_path <- file.path(PCP_DIR, "all_enrollment.csv")
    if (!file_exists(all_enroll_path)) {
        stop("all_enrollment.csv not found at ", all_enroll_path,
             "\nStep 7 Part D needs to be rerun (D:\\WPWatson space fix).",
             "\nRun with PCP_DENOM=dm for the diabetes-cohort analysis.")
    }
    enroll <- fread(all_enroll_path,
                    colClasses = list(character = "apcd_unique_id"))
    if (COHORT == "continuous") {
        enroll <- enroll[continuous_enrolled == 1L]
    } else {
        enroll <- enroll[total_months_enrolled >= 1L]
    }
} else {
    cohort_csv <- switch(COHORT,
        "continuous" = "cohort_continuous.csv",
        "fractional" = "cohort_fractional.csv")
    enroll <- fread(file.path(ENROLL_DIR, cohort_csv),
                    colClasses = list(character = "apcd_unique_id"))
}
message("Enrollment base persons: ", nrow(enroll))

# Restrict to payer stratum
enroll <- enroll[apcd_unique_id %in% strata_ids]
message("Stratum x cohort intersection: ", nrow(enroll))

# ---- Load ZIP lookup --------------------------------------------------------
zip_lookup <- fread(file.path(PCP_DIR, "all_zip_lookup.csv"),
                    colClasses = list(character = c("apcd_unique_id", "ar_zip")))
zip_lookup <- zip_lookup[nchar(ar_zip) == 5L]

enroll <- merge(enroll, zip_lookup[, .(apcd_unique_id, ar_zip)],
                by = "apcd_unique_id", all.x = FALSE)
message("With AR ZIP: ", nrow(enroll))

# ---- ZIP -> ZCTA (2020 crosswalk; same convention as scripts 04/09) --------
# Uses tigris' ZCTA relationship table implicitly via ZIP == ZCTA5CE20 for
# the vast majority of AR ZIPs. Any un-crosswalked ZIPs fall out at the
# merge with the ZCTA polygons below (they'd be non-AR or PO-box-only).
enroll[, zcta := ar_zip]

# ---- Load PCP visits (long) ------------------------------------------------
pcp <- fread(file.path(PCP_DIR, "pcp_visits_bin.csv"),
             colClasses = list(character = c("apcd_unique_id", "bin_id")))
pcp <- pcp[apcd_unique_id %in% enroll$apcd_unique_id]
message("PCP-visit rows (stratum): ", nrow(pcp))

# ---- Build denominator (person-halfyears by zcta x bin) --------------------
long <- melt(enroll[, c("apcd_unique_id", "zcta", BIN_COLS), with = FALSE],
             id.vars      = c("apcd_unique_id", "zcta"),
             measure.vars = BIN_COLS,
             variable.name = "bin_col", value.name = "months")
long[, months := as.integer(months)]
long[is.na(months), months := 0L]
long[, bin_id := sub("^m_h([12])_(\\d{4})$", "\\2-H\\1", as.character(bin_col))]

if (COHORT == "continuous") {
    long[, contrib := fifelse(months >= 1L, 1.0, 0.0)]
} else {
    long[, contrib := fifelse(months >= BIN_MONTH_FLOOR, months / 6.0, 0.0)]
}
den <- long[, .(person_halfyears = sum(contrib)), by = .(zcta, bin_id)]

# ---- Build numerator (distinct persons with PCP visit) ---------------------
# Merge PCP flags to enrolled persons + their ZCTA, keep only bins where they
# had >=1 visit AND were enrolled (>= 1 month, or >= 3 for fractional).
pcp_enrolled <- merge(pcp, long[, .(apcd_unique_id, zcta, bin_id, months)],
                      by = c("apcd_unique_id", "bin_id"), all.x = FALSE)
if (COHORT == "continuous") {
    pcp_enrolled <- pcp_enrolled[months >= 1L]
} else {
    pcp_enrolled <- pcp_enrolled[months >= BIN_MONTH_FLOOR]
}
num <- unique(pcp_enrolled[, .(apcd_unique_id, zcta, bin_id)])[
    , .(num_count = .N), by = .(zcta, bin_id)]

# ---- Pool across bins -------------------------------------------------------
grid <- CJ(zcta = unique(c(den$zcta, num$zcta)), bin_id = HALF_BIN_IDS)
panel <- merge(grid, den, by = c("zcta", "bin_id"), all.x = TRUE)
panel <- merge(panel, num, by = c("zcta", "bin_id"), all.x = TRUE)
panel[is.na(person_halfyears), person_halfyears := 0]
panel[is.na(num_count),        num_count        := 0L]

pooled <- panel[, .(person_halfyears = sum(person_halfyears),
                    pcp_person_halfyears = sum(num_count)),
                by = zcta]
pooled[, rate_per_1000 := 1000 * pcp_person_halfyears /
                          pmax(person_halfyears, 1L)]
pooled[, suppressed := person_halfyears < DEN_THRESH]
pooled_active <- pooled[!(suppressed)]

message("ZCTAs modeled (", STRATUM, "/", DENOM, "): ", nrow(pooled_active))
if (nrow(pooled_active) < 30) {
    message("WARNING: <30 modeled ZCTAs; spatial analysis may be unstable.")
}

# ---- Spatial pipeline (mirror of R/09) --------------------------------------
ar_state <- states(progress_bar = FALSE)
ar_state <- ar_state[ar_state$STUSPS == "AR", ]
ar_state <- st_transform(ar_state, 5070)
ar_zctas <- zctas(year = 2020, progress_bar = FALSE)
ar_zctas$zcta <- ar_zctas$ZCTA5CE20
ar_zctas <- st_transform(ar_zctas, 5070)
ar_cent  <- st_centroid(ar_zctas)
inside   <- st_intersects(ar_cent, ar_state, sparse = FALSE)[, 1]
ar_zctas <- ar_zctas[inside, ]
zcta_sf  <- merge(ar_zctas, pooled_active, by = "zcta", all.x = FALSE)

if (nrow(zcta_sf) < 30) {
    message("Aborting: insufficient ZCTAs after AR clip.")
    quit(status = 0)
}

coords <- st_centroid(zcta_sf) |> st_coordinates()
nb     <- knn2nb(knearneigh(coords, k = KNN_K), sym = FALSE)
lw     <- nb2listw(nb, style = "W", zero.policy = TRUE)

eb_local <- EBlocal(zcta_sf$pcp_person_halfyears,
                    zcta_sf$person_halfyears, nb)
zcta_sf$rate_raw      <- zcta_sf$rate_per_1000
zcta_sf$rate_per_1000 <- eb_local$est * 1000
n_na <- sum(is.na(zcta_sf$rate_per_1000))
if (n_na > 0) {
    message("EBlocal produced NA in ", n_na,
            " ZCTAs (sparse stratum); falling back to raw rate.")
    zcta_sf$rate_per_1000[is.na(zcta_sf$rate_per_1000)] <-
        zcta_sf$rate_raw[is.na(zcta_sf$rate_per_1000)]
}

set.seed(20260804)
moran_global <- moran.mc(zcta_sf$rate_per_1000, lw,
                         nsim = NPERM, zero.policy = TRUE)
print(moran_global)

set.seed(20260804)
gi_perm <- localG_perm(zcta_sf$rate_per_1000, lw,
                       nsim = NPERM, zero.policy = TRUE)
zcta_sf$gi_z     <- as.numeric(gi_perm)
zcta_sf$gi_p     <- attr(gi_perm, "internals")[, "Pr(z != E(Gi))"]
zcta_sf$gi_p_fdr <- p.adjust(zcta_sf$gi_p, method = "BH")
zcta_sf$gi_bin <- with(as.data.frame(zcta_sf), fcase(
    gi_z >=  2.58, "Hot 99%",
    gi_z >=  1.96 & gi_z < 2.58, "Hot 95%",
    gi_z >=  1.65 & gi_z < 1.96, "Hot 90%",
    gi_z <= -2.58, "Cold 99%",
    gi_z <= -1.96 & gi_z > -2.58, "Cold 95%",
    gi_z <= -1.65 & gi_z > -1.96, "Cold 90%",
    default = "Not significant"))
zcta_sf$gi_bin <- factor(zcta_sf$gi_bin, levels = c(
    "Cold 99%","Cold 95%","Cold 90%","Not significant",
    "Hot 90%","Hot 95%","Hot 99%"))

set.seed(20260804)
lisa <- localmoran_perm(zcta_sf$rate_per_1000, lw,
                        nsim = NPERM, zero.policy = TRUE)
zcta_sf$lisa_p <- lisa[, "Pr(z != E(Ii))"]
mean_rate <- mean(zcta_sf$rate_per_1000, na.rm = TRUE)
lagged    <- lag.listw(lw, zcta_sf$rate_per_1000, zero.policy = TRUE)
zcta_sf$lisa_quad <- with(as.data.frame(zcta_sf), fcase(
    lisa_p >= 0.05, "NS",
    rate_per_1000 >= mean_rate & lagged >= mean_rate, "High-High",
    rate_per_1000 <  mean_rate & lagged <  mean_rate, "Low-Low",
    rate_per_1000 >= mean_rate & lagged <  mean_rate, "High-Low",
    rate_per_1000 <  mean_rate & lagged >= mean_rate, "Low-High"))

stem <- sprintf("pcp_%s_%s_%s", tolower(STRATUM), DENOM, COHORT)
saveRDS(zcta_sf, file.path(OUT_DIR, sprintf("zcta_sf_lisa_%s.rds", stem)))

# ---- Maps -------------------------------------------------------------------
theme_map <- theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey30"),
          plot.caption  = element_text(color = "grey40", size = 9),
          legend.position = "right")

denom_label <- if (DENOM == "all") "whole APCD-enrolled" else "diabetes cohort only"
n_label <- format(nrow(enroll), big.mark = ",")

p_rate <- ggplot() +
    geom_sf(data = ar_zctas, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = rate_per_1000),
            color = "grey40", linewidth = 0.15) +
    scale_fill_viridis(option = "C",
                       name = "PCP visits\nper 1,000 person-\nhalfyears (EB)") +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = sprintf("Common primary-care visits — %s stratum",
                            STRATUM),
         subtitle = sprintf("Denominator: %s | Cohort: %s | N = %s",
                            denom_label, COHORT, n_label),
         caption  = "Local EB | KNN k=8 | Pooled 2017-2022 (excluding 2025)") +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_rate_%s.png", stem)),
       p_rate, width = 9, height = 7, dpi = 300)

gi_palette <- c(
    "Cold 99%" = "#053061","Cold 95%" = "#2166ac","Cold 90%" = "#4393c3",
    "Not significant" = "grey88",
    "Hot 90%" = "#f4a582","Hot 95%" = "#d6604d","Hot 99%" = "#b2182b")
p_gi <- ggplot() +
    geom_sf(data = ar_zctas, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = gi_bin),
            color = "grey40", linewidth = 0.15) +
    scale_fill_manual(values = gi_palette, name = "Gi* Cluster",
                      drop = FALSE) +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = sprintf("Gi* — PCP visits, %s stratum", STRATUM),
         subtitle = sprintf("Denominator: %s | N = %s", denom_label, n_label),
         caption  = sprintf("Local EB | KNN k=8 | 999 perms | Moran's I = %.3f, p = %.3f",
                            moran_global$statistic, moran_global$p.value)) +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_gi_%s.png", stem)),
       p_gi, width = 9, height = 7, dpi = 300)

# ---- DFU cold-spot overlay (comparison) -------------------------------------
# If the payer-stratified DFU Gi* result exists, overlay its cold-spot ZCTAs
# on the PCP Gi* map so we can eyeball whether they coincide.
dfu_stem <- sprintf("dfu_%s_%s", tolower(STRATUM), COHORT)
dfu_rds  <- file.path(dirname(OUT_DIR), "static_spatial",
                      sprintf("zcta_sf_lisa_pooled_%s.rds", dfu_stem))
if (file_exists(dfu_rds)) {
    dfu_sf <- readRDS(dfu_rds)
    dfu_cold <- dfu_sf[dfu_sf$gi_bin %in% c("Cold 90%","Cold 95%","Cold 99%"), ]
    p_overlay <- ggplot() +
        geom_sf(data = ar_zctas, fill = "grey93", color = "grey85",
                linewidth = 0.08) +
        geom_sf(data = zcta_sf, aes(fill = gi_bin),
                color = "grey40", linewidth = 0.15) +
        scale_fill_manual(values = gi_palette, name = "PCP Gi*",
                          drop = FALSE) +
        geom_sf(data = dfu_cold, fill = NA,
                color = "black", linewidth = 0.9, linetype = "solid") +
        geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
        labs(title    = sprintf("PCP Gi* with DFU cold-spot overlay — %s",
                                STRATUM),
             subtitle = sprintf("Black outlines = DFU Cold 90/95/99%% ZCTAs (script 09)"),
             caption  = "Overlap => cold-spot likely an access-to-care artifact") +
        theme_map
    ggsave(file.path(OUT_DIR, sprintf("map_gi_overlay_%s.png", stem)),
           p_overlay, width = 9, height = 7, dpi = 300)
    message("Overlay saved: map_gi_overlay_", stem, ".png")

    # Quick numeric overlap table
    dfu_ids <- dfu_sf$zcta[dfu_sf$gi_bin %in% c("Cold 90%","Cold 95%","Cold 99%")]
    pcp_ids <- zcta_sf$zcta[zcta_sf$gi_bin %in% c("Cold 90%","Cold 95%","Cold 99%")]
    overlap <- intersect(dfu_ids, pcp_ids)
    message(sprintf("DFU cold-spot ZCTAs: %d | PCP cold-spot ZCTAs: %d | Overlap: %d",
                    length(dfu_ids), length(pcp_ids), length(overlap)))
} else {
    message("Skipping overlay: DFU Gi* file not found at ", dfu_rds)
}

# ---- Summary ---------------------------------------------------------------
message("\n10 complete: ", stem)
message(sprintf("  ZCTAs modeled:   %d", nrow(zcta_sf)))
message(sprintf("  Moran's I:       %.3f (p = %.3f)",
                moran_global$statistic, moran_global$p.value))
print(table(zcta_sf$gi_bin))
