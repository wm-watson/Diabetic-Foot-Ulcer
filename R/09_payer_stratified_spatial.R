# =============================================================================
# 09_payer_stratified_spatial.R
#
# Payer-stratified static spatial analysis. Reruns the DFU prevalence and
# amputation incidence analyses (scripts 05 and 07) for each of the four
# payer strata produced by sas/step3d_payer_strata.sas:
#
#   - MEDICARE   (>=80% Medicare-MA + Medicare-FFS combined)
#   - MEDICAID   (>=80% MCD/MCD_QHP/HCIP/PASSE)
#   - COMMERCIAL (>=80% COM/QHP-non-MCD/EBD)
#   - MIXED      (no single payer >=80%)
#
# Why this matters:
#   The Delta cold-spot pattern in the pooled analysis may reflect
#   payer-specific data-capture differences, not disease distribution:
#     - Medicare FFS has near-100% capture (CMS-reported)
#     - AR Medicaid has near-100% capture (state-reported)
#     - TN-licensed commercial insurance has 0% capture (payer not in
#       APCD) -- this is the Memphis edge mechanism
#   If the Delta cold spot disappears in the Medicare-only and
#   Medicaid-only strata but persists in Commercial, the cold spot is
#   driven by Memphis-employed AR residents with TN commercial insurance
#   rather than real disease patterns.
#
# Inputs:
#   payer_strata.csv            (from step3d_payer_strata.sas)
#   cohort_continuous.csv       (from step3c_enrollment.sas)
#   analytic/02_tiered.rds      (R pipeline)
#   bin_activity_*.csv          (from step3b_bin_activity.sas)
#
# Outputs (per stratum, in outputs/static_spatial/):
#   zcta_sf_lisa_pooled_<outcome>_<stratum>_<cohort>.rds
#   map_rate_<outcome>_<stratum>_<cohort>.png
#   map_gi_<outcome>_<stratum>_<cohort>.png
#
# Where:
#   <outcome> in {dfu, amp}        (DFU prevalence, amputation incidence)
#   <stratum> in {medicare, medicaid, commercial, mixed}
#   <cohort>  in {continuous, fractional}  (default = continuous)
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
ENROLL_DIR  <- Sys.getenv(
    "DFU_ENROLL_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/APCD/Analytical Datasets"
)
ANALYTIC <- file.path(DROPBOX_DIR, "analytic")
OUT_DIR  <- file.path(DROPBOX_DIR, "outputs", "static_spatial")
dir_create(OUT_DIR)

COHORT  <- Sys.getenv("DFU_COHORT", "continuous")
STRATUM <- toupper(Sys.getenv("DFU_PAYER_STRATUM", "MEDICARE"))
OUTCOME <- tolower(Sys.getenv("DFU_OUTCOME", "dfu"))   # "dfu" or "amp"

stopifnot(STRATUM %in% c("MEDICARE", "MEDICAID", "COMMERCIAL", "MIXED"))
stopifnot(OUTCOME %in% c("dfu", "amp"))
message("Cohort:   ", COHORT)
message("Stratum:  ", STRATUM)
message("Outcome:  ", OUTCOME)

# ---- Parameters -------------------------------------------------------------
STUDY_YEARS <- 2017:2022
KNN_K       <- 8L
NPERM       <- 999L
FDR_ALPHA   <- 0.05
DEN_THRESH  <- 20L
BIN_MONTH_FLOOR <- 3L
BIN_COLS <- c("m_h1_2017","m_h2_2017","m_h1_2018","m_h2_2018",
              "m_h1_2019","m_h2_2019","m_h1_2020","m_h2_2020",
              "m_h1_2021","m_h2_2021","m_h1_2022","m_h2_2022")
HALF_BIN_IDS <- c("2017-H1","2017-H2","2018-H1","2018-H2",
                  "2019-H1","2019-H2","2020-H1","2020-H2",
                  "2021-H1","2021-H2","2022-H1","2022-H2")

# ---- Load payer strata + cohort ---------------------------------------------
strata <- fread(file.path(ENROLL_DIR, "payer_strata.csv"),
                colClasses = list(character = "apcd_unique_id"))
strata_ids <- strata[primary_payer_category == STRATUM, apcd_unique_id]
message("Patients in stratum ", STRATUM, ": ", length(strata_ids))

cohort_csv <- switch(COHORT,
    "continuous" = "cohort_continuous.csv",
    "fractional" = "cohort_fractional.csv",
    stop("Unknown cohort: ", COHORT))
cohort_dt <- fread(file.path(ENROLL_DIR, cohort_csv),
                   colClasses = list(character = "apcd_unique_id"))

# Restrict cohort to this stratum
cohort_strat <- cohort_dt[apcd_unique_id %in% strata_ids]
message("Stratum + cohort intersection: ", nrow(cohort_strat))

# ---- Patient-level data with ZCTA + tier flags ------------------------------
pt <- readRDS(file.path(ANALYTIC, "02_tiered.rds"))
pt <- pt[dm_primary == 1L & !is.na(zcta)]
pt[, apcd_unique_id := fifelse(
    nchar(study_id) >= 2,
    substr(study_id, 1, nchar(study_id) - 1L),
    NA_character_)]
pt[, first_amp_date := as.Date(first_amp_date)]
pt[, first_dfu_date := as.Date(first_dfu_date)]

pt_strat <- pt[apcd_unique_id %in% strata_ids]

# ---- Build denominator (DM person-halfyears, this stratum + cohort) --------
zcta_lookup <- unique(pt_strat[, .(
    apcd_unique_id, zcta,
    amp_year = as.integer(format(first_amp_date, "%Y")),
    dfu_year = as.integer(format(first_dfu_date, "%Y")))])
zcta_lookup[, censor_year := fifelse(
    !is.na(amp_year) & (is.na(dfu_year) | amp_year >= dfu_year),
    amp_year, NA_integer_)]

coh <- merge(cohort_strat[, c("apcd_unique_id", BIN_COLS), with = FALSE],
             zcta_lookup, by = "apcd_unique_id", all.x = FALSE)

long <- melt(coh, id.vars = c("apcd_unique_id", "zcta", "censor_year"),
             measure.vars = BIN_COLS,
             variable.name = "bin_col", value.name = "months")
long[, months := as.integer(months)]
long[is.na(months), months := 0L]
long[, bin_id := sub("^m_h([12])_(\\d{4})$", "\\2-H\\1", as.character(bin_col))]
long[, bin_year := as.integer(sub("^(\\d{4}).*$", "\\1", bin_id))]
long[!is.na(censor_year) & bin_year > censor_year, months := 0L]
if (COHORT == "continuous") {
    long[, contrib := fifelse(months >= 1L, 1.0, 0.0)]
} else {
    long[, contrib := fifelse(months >= BIN_MONTH_FLOOR, months / 6.0, 0.0)]
}
den <- long[, .(dm_person_halfyears = sum(contrib)), by = .(zcta, bin_id)]

# ---- Build numerator --------------------------------------------------------
if (OUTCOME == "amp") {
    pt_event <- pt_strat[!is.na(first_amp_date) &
                         first_amp_date >= as.Date("2017-01-01") &
                         first_amp_date <= as.Date("2022-12-31")]
    pt_event[, bin_year := as.integer(format(first_amp_date, "%Y"))]
    pt_event[, half     := ifelse(as.integer(format(first_amp_date, "%m")) <= 6, 1L, 2L)]
    pt_event[, bin_id   := sprintf("%d-H%d", bin_year, half)]
    num <- pt_event[, .(num_count = .N), by = .(zcta, bin_id)]
} else {
    # DFU prevalence: count Tier 2 patients with first DFU in bin
    pt_event <- pt_strat[tier2 == 1L &
                         !is.na(first_dfu_date) &
                         first_dfu_date >= as.Date("2017-01-01") &
                         first_dfu_date <= as.Date("2022-12-31")]
    pt_event[, bin_year := as.integer(format(first_dfu_date, "%Y"))]
    pt_event[, half     := ifelse(as.integer(format(first_dfu_date, "%m")) <= 6, 1L, 2L)]
    pt_event[, bin_id   := sprintf("%d-H%d", bin_year, half)]
    num <- pt_event[, .(num_count = .N), by = .(zcta, bin_id)]
}

# ---- Pool, build sf, run Gi*/LISA ------------------------------------------
grid <- CJ(zcta = unique(c(den$zcta, num$zcta)), bin_id = HALF_BIN_IDS)
panel <- merge(grid, den, by = c("zcta", "bin_id"), all.x = TRUE)
panel <- merge(panel, num, by = c("zcta", "bin_id"), all.x = TRUE)
panel[is.na(dm_person_halfyears), dm_person_halfyears := 0]
panel[is.na(num_count),           num_count           := 0L]

pooled <- panel[, .(dm_person_halfyears = sum(dm_person_halfyears),
                    event_person_halfyears = sum(num_count)),
                by = zcta]
pooled[, rate_per_1000 := 1000 * event_person_halfyears /
                          pmax(dm_person_halfyears, 1L)]
pooled[, suppressed := dm_person_halfyears < DEN_THRESH]
pooled_active <- pooled[!(suppressed)]

message("ZCTAs modeled (", STRATUM, "/", OUTCOME, "): ", nrow(pooled_active))
if (nrow(pooled_active) < 30) {
    message("WARNING: <30 modeled ZCTAs in this stratum; spatial analysis may be unstable.")
}

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

eb_local <- EBlocal(zcta_sf$event_person_halfyears,
                    zcta_sf$dm_person_halfyears, nb)
zcta_sf$rate_raw      <- zcta_sf$rate_per_1000
zcta_sf$rate_per_1000 <- eb_local$est * 1000

set.seed(20260428)
moran_global <- moran.mc(zcta_sf$rate_per_1000, lw,
                         nsim = NPERM, zero.policy = TRUE)
print(moran_global)

set.seed(20260428)
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

set.seed(20260428)
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

stem <- sprintf("%s_%s_%s", OUTCOME, tolower(STRATUM), COHORT)
saveRDS(zcta_sf, file.path(OUT_DIR, sprintf("zcta_sf_lisa_pooled_%s.rds", stem)))

# ---- Maps -------------------------------------------------------------------
theme_map <- theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey30"),
          plot.caption  = element_text(color = "grey40", size = 9),
          legend.position = "right")

outcome_label <- ifelse(OUTCOME == "amp",
                        "Amputation incidence",
                        "DFU prevalence")
n_label <- format(length(strata_ids), big.mark = ",")

p_rate <- ggplot() +
    geom_sf(data = ar_zctas, fill = "grey93", color = "grey85",
            linewidth = 0.08) +
    geom_sf(data = zcta_sf, aes(fill = rate_per_1000),
            color = "grey40", linewidth = 0.15) +
    scale_fill_viridis(option = "C",
                       name = sprintf("%s\nper 1,000\n(EB-smoothed)",
                                      outcome_label)) +
    geom_sf(data = ar_state, fill = NA, color = "black", linewidth = 0.6) +
    labs(title    = sprintf("%s — %s stratum",
                            outcome_label, STRATUM),
         subtitle = sprintf("Cohort: %s | N patients = %s",
                            COHORT, n_label),
         caption  = sprintf("Local EB | KNN k=8 | Pooled 2017–2022")) +
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
    labs(title    = sprintf("Gi* — %s, %s stratum",
                            outcome_label, STRATUM),
         subtitle = sprintf("Cohort: %s | N = %s",
                            COHORT, n_label),
         caption  = sprintf("Local EB | KNN k=8 | 999 perms | Moran’s I = %.3f, p = %.3f",
                            moran_global$statistic, moran_global$p.value)) +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_gi_%s.png", stem)),
       p_gi, width = 9, height = 7, dpi = 300)

# ---- Summary ----------------------------------------------------------------
message("\n09 complete: ", stem)
message(sprintf("  ZCTAs modeled:   %d", nrow(zcta_sf)))
message(sprintf("  Moran's I:       %.3f (p = %.3f)",
                moran_global$statistic, moran_global$p.value))
print(table(zcta_sf$gi_bin))
