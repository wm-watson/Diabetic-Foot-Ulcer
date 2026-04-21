# =============================================================================
# 07_amputation_spatial.R
#
# Amputation-based static spatial analysis. Complements 05_static_spatial.R
# (which measures DFU coding prevalence) by measuring a more severe, harder-
# to-miss outcome: lower-extremity amputation among DM patients.
#
# Rationale (see README and 2026-04-21 session notes):
#   DFU prevalence maps capture care utilization + coding intensity. They
#   may show Ozark retirees as "hot" (chronic mild ulcers with repeat
#   coding) and Delta populations as "cold" (under-coded, later presentation,
#   payer leakage to Memphis). Amputation is a forced event -- you cannot
#   fail to code a below-knee amputation. Mapping amputation incidence
#   therefore captures disease severity and access failure directly,
#   producing the spatial pattern public health intervention targeting
#   actually needs.
#
# Numerator: patients whose FIRST amputation (first_amp_date) falls in the
#   bin, among DM patients. This is an incidence measure -- each patient
#   counted once, in the bin of their first event.
#
# Denominator: same enrollment-weighted person-halfyears used in 04 --
#   Sigma(enrolled_months/6) per ZCTA-bin, restricted to the active cohort.
#
# Cohort selection via DFU_COHORT env var (default = continuous).
# Outputs are suffixed _amp so they do not overwrite the DFU maps.
#
# Assumptions: R/_assumptions.md sections 3.2, 6.1, 8.2.
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
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
ANALYTIC    <- file.path(DROPBOX_DIR, "analytic")
PANELS      <- file.path(DROPBOX_DIR, "outputs", "panels")
OUT_DIR     <- file.path(DROPBOX_DIR, "outputs", "static_spatial")
dir_create(OUT_DIR)

COHORT <- Sys.getenv("DFU_COHORT", "continuous")
message("Using cohort: ", COHORT)

# ---- Parameters -------------------------------------------------------------
STUDY_YEARS <- 2017:2022
KNN_K       <- 8L
NPERM       <- 999L
FDR_ALPHA   <- 0.05
DEN_THRESH  <- 20L

HALF_BIN_IDS <- c("2017-H1", "2017-H2", "2018-H1", "2018-H2",
                  "2019-H1", "2019-H2", "2020-H1", "2020-H2",
                  "2021-H1", "2021-H2", "2022-H1", "2022-H2")

# ---- Load existing DFU panel to inherit enrollment-weighted denominators ----
panel_file <- file.path(PANELS, sprintf("panel_halfyear_%s.rds", COHORT))
stopifnot(file.exists(panel_file))
panel <- as.data.table(readRDS(panel_file))

# ---- Build amputation numerator from tiered patient data --------------------
pt <- readRDS(file.path(ANALYTIC, "02_tiered.rds"))
pt <- pt[dm_primary == 1L]
pt[, first_amp_date := as.Date(first_amp_date)]

# Restrict to amputations occurring within the study window
pt_amp <- pt[!is.na(first_amp_date) &
             first_amp_date >= as.Date("2017-01-01") &
             first_amp_date <= as.Date("2022-12-31") &
             !is.na(zcta)]

# Map each amputation to its half-year bin
pt_amp[, bin_year := as.integer(format(first_amp_date, "%Y"))]
pt_amp[, half     := ifelse(as.integer(format(first_amp_date, "%m")) <= 6, 1L, 2L)]
pt_amp[, bin_id   := sprintf("%d-H%d", bin_year, half)]

# Apply the same cohort filter used in script 04
cohort_csv <- switch(COHORT,
    "continuous" = "cohort_continuous.csv",
    "fractional" = "cohort_fractional.csv",
    stop("Unknown cohort: ", COHORT)
)
ENROLL_DIR <- Sys.getenv(
    "DFU_ENROLL_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/APCD/Analytical Datasets"
)
cohort_dt <- fread(file.path(ENROLL_DIR, cohort_csv),
                   colClasses = list(character = "apcd_unique_id"))
cohort_ids <- unique(cohort_dt$apcd_unique_id)

# Recover apcd_unique_id from study_id on the patient frame
pt_amp[, apcd_unique_id := fifelse(
    nchar(study_id) >= 2,
    substr(study_id, 1, nchar(study_id) - 1L),
    NA_character_
)]
pt_amp <- pt_amp[apcd_unique_id %in% cohort_ids]

# Count first amputations per ZCTA per bin (each patient counted once)
amp_num <- pt_amp[, .(amp_num = .N), by = .(zcta, bin_id)]

# ---- Replace dfu_num with amp_num in the panel ------------------------------
panel[, dfu_num := NULL]
panel <- merge(panel, amp_num, by = c("zcta", "bin_id"), all.x = TRUE)
panel[is.na(amp_num), amp_num := 0L]
setnames(panel, "amp_num", "dfu_num")   # reuse downstream code

# ---- Pool across 12 bins for static spatial analysis ------------------------
pooled <- panel[, .(dm_person_halfyears  = sum(dm_denom),
                    amp_person_halfyears = sum(dfu_num)),
                by = zcta]
pooled[, rate_per_1000 := 1000 * amp_person_halfyears /
                          pmax(dm_person_halfyears, 1L)]
# Analytic threshold: denominator-only (internal use; adjust for publication)
pooled[, suppressed := dm_person_halfyears < DEN_THRESH]
pooled_active <- pooled[!(suppressed)]

message("ZCTAs modeled (amp outcome): ", nrow(pooled_active))
message("Pooled amp rate range (raw): ",
        round(min(pooled_active$rate_per_1000), 2), " - ",
        round(max(pooled_active$rate_per_1000), 2),
        " per 1,000 person-halfyears")

# ---- ZCTA polygons, spatial clip to AR --------------------------------------
ar_state <- states(progress_bar = FALSE)
ar_state <- ar_state[ar_state$STUSPS == "AR", ]
ar_state <- st_transform(ar_state, 5070)

ar_zctas <- zctas(year = 2020, progress_bar = FALSE)
ar_zctas$zcta <- ar_zctas$ZCTA5CE20
ar_zctas <- st_transform(ar_zctas, 5070)
ar_cent  <- st_centroid(ar_zctas)
inside   <- st_intersects(ar_cent, ar_state, sparse = FALSE)[, 1]
ar_zctas <- ar_zctas[inside, ]

zcta_sf <- merge(ar_zctas, pooled_active, by = "zcta", all.x = FALSE)

# ---- Spatial weights + local EB ---------------------------------------------
coords <- st_centroid(zcta_sf) |> st_coordinates()
nb     <- knn2nb(knearneigh(coords, k = KNN_K), sym = FALSE)
lw     <- nb2listw(nb, style = "W", zero.policy = TRUE)

eb_local <- EBlocal(zcta_sf$amp_person_halfyears,
                    zcta_sf$dm_person_halfyears, nb)
zcta_sf$rate_raw      <- zcta_sf$rate_per_1000
zcta_sf$rate_per_1000 <- eb_local$est * 1000

# ---- Global Moran's I -------------------------------------------------------
set.seed(20260421)
moran_global <- moran.mc(zcta_sf$rate_per_1000, lw,
                         nsim = NPERM, zero.policy = TRUE)
print(moran_global)
saveRDS(moran_global, file.path(OUT_DIR,
        sprintf("global_morans_I_amp_%s.rds", COHORT)))

# ---- Gi* --------------------------------------------------------------------
set.seed(20260421)
gi_perm <- localG_perm(zcta_sf$rate_per_1000, lw,
                       nsim = NPERM, zero.policy = TRUE)
zcta_sf$gi_z     <- as.numeric(gi_perm)
zcta_sf$gi_p     <- attr(gi_perm, "internals")[, "Pr(z != E(Gi))"]
zcta_sf$gi_p_fdr <- p.adjust(zcta_sf$gi_p, method = "BH")

zcta_sf$gi_bin <- with(as.data.frame(zcta_sf), fcase(
    gi_z >=  2.58,                   "Hot 99%",
    gi_z >=  1.96 & gi_z < 2.58,     "Hot 95%",
    gi_z >=  1.65 & gi_z < 1.96,     "Hot 90%",
    gi_z <= -2.58,                   "Cold 99%",
    gi_z <= -1.96 & gi_z > -2.58,    "Cold 95%",
    gi_z <= -1.65 & gi_z > -1.96,    "Cold 90%",
    default = "Not significant"
))
zcta_sf$gi_bin <- factor(zcta_sf$gi_bin, levels = c(
    "Cold 99%", "Cold 95%", "Cold 90%", "Not significant",
    "Hot 90%", "Hot 95%", "Hot 99%"))
zcta_sf$gi_fdr_robust <- zcta_sf$gi_p_fdr < FDR_ALPHA

# ---- LISA -------------------------------------------------------------------
set.seed(20260421)
lisa <- localmoran_perm(zcta_sf$rate_per_1000, lw,
                        nsim = NPERM, zero.policy = TRUE)
zcta_sf$lisa_I     <- lisa[, "Ii"]
zcta_sf$lisa_z     <- lisa[, "Z.Ii"]
zcta_sf$lisa_p     <- lisa[, "Pr(z != E(Ii))"]
zcta_sf$lisa_p_fdr <- p.adjust(zcta_sf$lisa_p, method = "BH")

mean_rate <- mean(zcta_sf$rate_per_1000, na.rm = TRUE)
lagged    <- lag.listw(lw, zcta_sf$rate_per_1000, zero.policy = TRUE)
zcta_sf$lisa_quad <- with(as.data.frame(zcta_sf), fcase(
    lisa_p >= 0.05, "NS",
    rate_per_1000 >= mean_rate & lagged >= mean_rate, "High-High",
    rate_per_1000 <  mean_rate & lagged <  mean_rate, "Low-Low",
    rate_per_1000 >= mean_rate & lagged <  mean_rate, "High-Low",
    rate_per_1000 <  mean_rate & lagged >= mean_rate, "Low-High"
))
zcta_sf$lisa_fdr_robust <- zcta_sf$lisa_p_fdr < FDR_ALPHA

saveRDS(zcta_sf, file.path(OUT_DIR,
        sprintf("zcta_sf_gi_pooled_amp_%s.rds", COHORT)))
saveRDS(zcta_sf, file.path(OUT_DIR,
        sprintf("zcta_sf_lisa_pooled_amp_%s.rds", COHORT)))

# ---- Maps -------------------------------------------------------------------
# Full AR ZCTA layer for grey background
all_zctas_bg <- ar_zctas

theme_map <- theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey30"),
          plot.caption  = element_text(color = "grey40", size = 9),
          legend.position = "right")

# 1. Rate choropleth (EB-smoothed)
p_rate <- ggplot() +
    geom_sf(data = all_zctas_bg, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = rate_per_1000),
            color = "grey40", linewidth = 0.15) +
    scale_fill_viridis(option = "C",
                       name = "Amputations per\n1,000 person-halfyears\n(EB-smoothed)") +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = "Lower-Extremity Amputation Incidence \u2014 AR ZCTAs",
         subtitle = sprintf("Cohort: %s | Pooled 2017\u20132022", COHORT),
         caption  = sprintf("Local EB | N patients = %d | N amputations = %d",
                            uniqueN(pt_amp$apcd_unique_id),
                            sum(pt_amp$apcd_unique_id %in% cohort_ids))) +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_rate_amp_%s.png", COHORT)),
       p_rate, width = 9, height = 7, dpi = 300)

# 2. Gi* map
gi_palette <- c(
    "Cold 99%" = "#053061", "Cold 95%" = "#2166ac", "Cold 90%" = "#4393c3",
    "Not significant" = "grey88",
    "Hot 90%"  = "#f4a582", "Hot 95%"  = "#d6604d", "Hot 99%"  = "#b2182b"
)
p_gi <- ggplot() +
    geom_sf(data = all_zctas_bg, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = gi_bin),
            color = "grey40", linewidth = 0.15) +
    scale_fill_manual(values = gi_palette,
                      name = "Gi* Cluster",
                      drop = FALSE) +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = "Getis-Ord Gi* Hot Spot Analysis \u2014 Amputation",
         subtitle = sprintf("Cohort: %s | Single-test z-bands", COHORT),
         caption  = sprintf("Local EB | KNN k=8 | 999 permutations | Global Moran\u2019s I = %.3f, p = %.3f",
                            moran_global$statistic, moran_global$p.value)) +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_gi_amp_%s.png", COHORT)),
       p_gi, width = 9, height = 7, dpi = 300)

# 3. LISA map
lisa_palette <- c("High-High" = "#b2182b", "Low-Low" = "#2166ac",
                  "High-Low" = "#f4a582", "Low-High" = "#92c5de",
                  "NS" = "grey88")
p_lisa <- ggplot() +
    geom_sf(data = all_zctas_bg, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = lisa_quad),
            color = "grey40", linewidth = 0.15) +
    scale_fill_manual(values = lisa_palette,
                      name = "LISA Quadrant",
                      drop = FALSE) +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = "LISA Cluster Map \u2014 Amputation",
         subtitle = sprintf("Cohort: %s | Single-test p < 0.05", COHORT),
         caption  = "Local EB | KNN k=8 | 999 permutations") +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_lisa_amp_%s.png", COHORT)),
       p_lisa, width = 9, height = 7, dpi = 300)

# ---- Summary ----------------------------------------------------------------
message("\n07_amputation_spatial.R complete.")
message(sprintf("ZCTAs modeled:    %d", nrow(zcta_sf)))
message(sprintf("Global Moran's I: %.3f (p = %.3f)",
                moran_global$statistic, moran_global$p.value))
message("Gi* bins: "); print(table(zcta_sf$gi_bin))
message("LISA quadrants: "); print(table(zcta_sf$lisa_quad))
