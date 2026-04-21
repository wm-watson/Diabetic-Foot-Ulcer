# =============================================================================
# 04_zcta_aggregation.R
#
# Build the ZCTA x time-bin panel used by static spatial analysis (script 05)
# and EHSA space-time cube construction (script 06).
#
# Bin structures (Option C window = 2017-2022, see assumptions §5.5):
#   - H1/H2 half-year bins  -> 12 slices   (PRIMARY for EHSA)
#   - Meteorological seasonal bins -> 24 slices (SENSITIVITY)
#   - Calendar year bins    -> 6 slices    (DESCRIPTIVE only)
#
# *** Two-cohort design (assumptions §6.1, rev 3, 2026-04-21) ***
# Denominator construction is enrollment-based, not claims-observed:
#
#   COHORT 1 (PRIMARY)  -- continuously enrolled all 72 months
#     Source: cohort_continuous.csv from step3c_enrollment.sas
#     Each patient contributes 1.0 person-halfyear to EVERY bin for which
#     their ZCTA assignment is valid -- they are at risk throughout the
#     study window by definition.
#
#   COHORT 2 (SENSITIVITY)  -- fractional person-time
#     Source: cohort_fractional.csv from step3c_enrollment.sas
#     Each patient contributes enrolled_months / 6 person-halfyears per bin.
#     Per-bin floor of >=3 of 6 months (50%) applied here; bins below the
#     floor contribute 0 for that patient (rationale: "more unobserved
#     than observed"). Patient still contributes to other bins where the
#     floor is met.
#
# DFU-aware amputation censoring rule (assumptions §5.3):
#   Censor patient from both numerator and denominator in bins at or after
#   first amputation ONLY if first amputation occurred on or after first DFU.
#
# Small-cell suppression (assumptions §6.4):
#   Cell suppressed if numerator < NUM_THRESH (default 11) OR
#                     denominator < DEN_THRESH (default 20).
#   Suppression is applied for DISPLAY ONLY; EHSA input (script 06) is
#   built on unsuppressed panels.
#
# Outputs (per cohort):
#   panel_halfyear_<cohort>.rds     -- PRIMARY EHSA input
#   panel_seasonal_<cohort>.rds     -- seasonal sensitivity
#   panel_annual_<cohort>.rds       -- descriptive
#   + matching .csv copies
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(fs)
    library(lubridate)
})

# ---- Paths ------------------------------------------------------------------
DROPBOX_DIR <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
ANALYTIC    <- file.path(DROPBOX_DIR, "analytic")
BIN_COMM    <- file.path(DROPBOX_DIR, "bin_activity_commercial.csv")
BIN_MCR     <- file.path(DROPBOX_DIR, "bin_activity_medicare.csv")
OUT_DIR     <- file.path(DROPBOX_DIR, "outputs", "panels")
dir_create(OUT_DIR)

TIER_RDS         <- file.path(ANALYTIC, "02_tiered.rds")
ENROLL_CONT_CSV  <- file.path(DROPBOX_DIR, "cohort_continuous.csv")
ENROLL_FRAC_CSV  <- file.path(DROPBOX_DIR, "cohort_fractional.csv")

# ---- Parameters -------------------------------------------------------------
STUDY_YEARS  <- 2017:2022
NUM_THRESH   <- 11L     # suppress numerator < 11
DEN_THRESH   <- 20L     # suppress denominator < 20
BIN_MONTH_FLOOR <- 3L   # >=3 of 6 months required for a bin to contribute
                         # under the fractional cohort (assumptions §6.1)

BIN_COLS <- c("m_h1_2017", "m_h2_2017",
              "m_h1_2018", "m_h2_2018",
              "m_h1_2019", "m_h2_2019",
              "m_h1_2020", "m_h2_2020",
              "m_h1_2021", "m_h2_2021",
              "m_h1_2022", "m_h2_2022")

HALF_BIN_IDS <- c("2017-H1", "2017-H2", "2018-H1", "2018-H2",
                  "2019-H1", "2019-H2", "2020-H1", "2020-H2",
                  "2021-H1", "2021-H2", "2022-H1", "2022-H2")

# ---- Load tiered patient-level data -----------------------------------------
stopifnot(file_exists(TIER_RDS), file_exists(BIN_COMM), file_exists(BIN_MCR))
pt <- readRDS(TIER_RDS)
pt <- pt[dm_primary == 1L]  # PRIMARY population (T1D + T2D, exclude AMBIGUOUS)

# Attach zcta + tier flags we need. study_id = apcd_unique_id || gender,
# so we recover apcd_unique_id by stripping the trailing M/F/U character.
pt[, apcd_unique_id := fifelse(
        nchar(study_id) >= 2,
        substr(study_id, 1, nchar(study_id) - 1L),
        NA_character_
)]
pt_keys <- pt[, .(data_source, patient_id, study_id, apcd_unique_id, zcta,
                  tier2, first_dfu_date, first_amp_date,
                  first_dm_year, last_dm_year)]

# ---- Load bin-activity claim-level flags ------------------------------------
ba_comm <- fread(BIN_COMM, colClasses = list(character = c("submitter",
                                                           "group_policy",
                                                           "person_code")))
ba_mcr  <- fread(BIN_MCR, colClasses = list(character = "bene_id"))
ba_comm[, data_source := "COMMERCIAL"]
ba_mcr[,  data_source := "MEDICARE"]

# Reconstruct the same patient_id key that step5 builds (LENGTH=80 SAS
# silent truncation; see assumptions §1 and prior session notes).
ba_comm[, patient_id := substr(paste(submitter, group_policy, person_code,
                                     sep = "|"), 1, 80)]
ba_mcr[,  patient_id := substr(as.character(bene_id), 1, 80)]

keep_cols <- c("data_source", "patient_id", "bin_year", "half",
               "season", "season_year",
               "had_dm", "had_l97", "had_combo", "had_dfu")
ba <- rbindlist(list(ba_comm[, ..keep_cols], ba_mcr[, ..keep_cols]),
                use.names = TRUE)

# Primary window filter
ba <- ba[bin_year %in% STUDY_YEARS]

# Merge in zcta + tier/censor info
ba <- merge(ba, pt_keys, by = c("data_source", "patient_id"),
            all.x = FALSE, all.y = FALSE)

# ---- Build bin labelings ----------------------------------------------------
ba[, half_bin := paste0(bin_year, "-H", half)]
ba[, season_bin := paste0(season_year, "-", season)]
ba <- ba[season_year >= min(STUDY_YEARS) & season_year <= max(STUDY_YEARS)]
ba[, year_bin := as.character(bin_year)]

# ---- DFU-aware amputation censoring -----------------------------------------
ba[, amp_year := as.integer(format(first_amp_date, "%Y"))]
ba[, dfu_year := as.integer(format(first_dfu_date, "%Y"))]
ba[, censor_year := fifelse(
    !is.na(amp_year) & (is.na(dfu_year) | amp_year >= dfu_year),
    amp_year,
    NA_integer_
)]
ba[, censored := !is.na(censor_year) & bin_year > censor_year]
ba_active <- ba[censored == FALSE]

# ---- Load cohort enrollment files -------------------------------------------
# Both CSVs are keyed on apcd_unique_id and carry the 12 monthly-count
# columns m_h1_2017 ... m_h2_2022 (see step3c_enrollment.sas).
load_cohort <- function(path, label) {
    if (!file_exists(path)) {
        warning("Cohort file not found: ", path,
                " -- skipping '", label, "' cohort.")
        return(NULL)
    }
    dt <- fread(path, colClasses = list(character = "apcd_unique_id"))
    # Expect apcd_unique_id plus the 12 m_* columns
    missing_cols <- setdiff(c("apcd_unique_id", BIN_COLS), names(dt))
    if (length(missing_cols) > 0) {
        stop("Cohort ", label, " missing columns: ",
             paste(missing_cols, collapse = ", "))
    }
    dt
}

cohort_cont <- load_cohort(ENROLL_CONT_CSV, "continuous")
cohort_frac <- load_cohort(ENROLL_FRAC_CSV, "fractional")

# ---- Compute numerator per (zcta, bin) --------------------------------------
# Numerator is cohort-independent in the sense that the service date proves
# enrollment at that moment. But we restrict to patients IN the cohort --
# a patient excluded from the continuous cohort cannot contribute to its
# numerator. We apply the cohort filter to ba_active before counting.

numerator_panel <- function(ba_active_filtered, bin_col) {
    d <- ba_active_filtered[, .(
        in_dfu_num = as.integer(tier2[1] == 1L &
                                any(had_l97 == 1L | had_combo == 1L))
    ), by = c("zcta", bin_col, "patient_id")]
    out <- d[, .(dfu_num = sum(in_dfu_num)), by = c("zcta", bin_col)]
    setnames(out, bin_col, "bin_id")
    out
}

# ---- Compute enrollment-based denominator per (zcta, bin) -------------------
# For each patient in the cohort with a known ZCTA (from pt_keys), the
# per-bin contribution is:
#   continuous cohort: 1.0 in every bin (they're enrolled all 72 months)
#   fractional cohort: (m_hX_YYYY / 6) when m_hX_YYYY >= BIN_MONTH_FLOOR
# We also respect DFU-aware amputation censoring by zeroing out bins at
# bin_year > censor_year for that patient.

denominator_panel <- function(cohort_dt, mode) {
    stopifnot(mode %in% c("continuous", "fractional"))

    # ZCTA and censor info per patient from pt_keys (unique per study person)
    zcta_lookup <- unique(pt_keys[!is.na(zcta), .(
        apcd_unique_id,
        zcta,
        amp_year    = as.integer(format(first_amp_date, "%Y")),
        dfu_year    = as.integer(format(first_dfu_date, "%Y"))
    )])
    zcta_lookup[, censor_year := fifelse(
        !is.na(amp_year) & (is.na(dfu_year) | amp_year >= dfu_year),
        amp_year, NA_integer_
    )]

    # Join cohort members to their ZCTA (inner join -- drop cohort patients
    # we cannot geographically assign, e.g., non-AR ZIP excluded upstream)
    coh <- merge(cohort_dt[, c("apcd_unique_id", BIN_COLS), with = FALSE],
                 zcta_lookup, by = "apcd_unique_id", all.x = FALSE)

    if (nrow(coh) == 0L) return(data.table())

    # Reshape wide -> long on the 12 bin columns
    long <- melt(coh,
                 id.vars = c("apcd_unique_id", "zcta", "censor_year"),
                 measure.vars = BIN_COLS,
                 variable.name = "bin_col", value.name = "months")
    long[, months := as.integer(months)]
    long[is.na(months), months := 0L]

    # Map m_h1_2017 -> "2017-H1" etc.
    long[, bin_id := sub("^m_h([12])_(\\d{4})$", "\\2-H\\1",
                         as.character(bin_col))]
    long[, bin_year := as.integer(sub("^(\\d{4}).*$", "\\1", bin_id))]

    # Apply DFU-aware amputation censoring: zero out bins beyond censor year
    long[!is.na(censor_year) & bin_year > censor_year, months := 0L]

    # Compute per-patient per-bin person-halfyear contribution
    if (mode == "continuous") {
        # Continuous cohort: full 1.0 contribution regardless of 'months'
        # value (they're by definition 6/6 in every bin). We still zero
        # out post-censoring bins.
        long[, contrib := fifelse(months >= 1L, 1.0, 0.0)]
    } else {
        # Fractional: contribute months/6, but only if >= BIN_MONTH_FLOOR
        long[, contrib := fifelse(months >= BIN_MONTH_FLOOR,
                                  months / 6.0, 0.0)]
    }

    # Aggregate to ZCTA x bin person-halfyears
    panel <- long[, .(dm_denom = sum(contrib)),
                  by = .(zcta, bin_id)]
    panel
}

# ---- Assemble, rectangularize, save -----------------------------------------
rectangularize_and_decorate <- function(num, den, all_bins, bin_type_label) {
    # Inner join num <-> den would drop ZCTAs that have denom but no num
    # (common in rural areas). We outer-join and fill missing num with 0,
    # and drop cells with exactly zero denominator (no one at risk there).
    all_zctas <- sort(unique(c(num$zcta, den$zcta)))
    grid <- CJ(zcta = all_zctas, bin_id = all_bins)
    out  <- merge(grid, num, by = c("zcta", "bin_id"), all.x = TRUE)
    out  <- merge(out,  den, by = c("zcta", "bin_id"), all.x = TRUE)
    out[is.na(dm_denom), dm_denom := 0]
    out[is.na(dfu_num),  dfu_num  := 0L]
    out[, bin_type := bin_type_label]
    out[, rate_per_1000 := 1000 * dfu_num / pmax(dm_denom, 1)]
    out[, suppressed := dfu_num < NUM_THRESH | dm_denom < DEN_THRESH]
    out[]
}

half_dates <- data.table(
    bin_id    = HALF_BIN_IDS,
    bin_start = as.Date(c(sapply(STUDY_YEARS,
                    function(y) c(sprintf("%d-01-01", y),
                                  sprintf("%d-07-01", y))))),
    bin_end   = as.Date(c(sapply(STUDY_YEARS,
                    function(y) c(sprintf("%d-06-30", y),
                                  sprintf("%d-12-31", y)))))
)

season_dates <- rbindlist(lapply(STUDY_YEARS, function(y) {
    data.table(
        bin_id    = c(paste0(y, "-W"), paste0(y, "-S"),
                      paste0(y, "-U"), paste0(y, "-A")),
        bin_start = as.Date(c(sprintf("%d-12-01", y - 1),
                              sprintf("%d-03-01", y),
                              sprintf("%d-06-01", y),
                              sprintf("%d-09-01", y))),
        bin_end   = as.Date(c(sprintf("%d-02-28", y),
                              sprintf("%d-05-31", y),
                              sprintf("%d-08-31", y),
                              sprintf("%d-11-30", y)))
    )
}))

build_cohort_panels <- function(cohort_dt, cohort_label, mode) {
    message("\n=== Building ", cohort_label, " cohort panels ===")

    # Filter bin-activity to patients in the cohort (for numerator)
    cohort_ids <- unique(cohort_dt$apcd_unique_id)
    # pt_keys carries apcd_unique_id extracted from study_id; rebuild the
    # filter via patient_id -> apcd_unique_id through pt_keys
    pt_in_cohort <- pt_keys[apcd_unique_id %in% cohort_ids,
                            unique(patient_id)]
    ba_cohort    <- ba_active[patient_id %in% pt_in_cohort]

    message("  Cohort size:       ", uniqueN(cohort_ids))
    message("  Bin-activity rows: ", nrow(ba_cohort))

    # Numerator panels
    num_half  <- numerator_panel(ba_cohort, "half_bin")
    num_seas  <- numerator_panel(ba_cohort, "season_bin")
    num_year  <- numerator_panel(ba_cohort, "year_bin")

    # Denominator panels (half-year only for cohort enrollment;
    # seasonal/annual derived by proportional allocation)
    den_half  <- denominator_panel(cohort_dt, mode)

    # Seasonal: allocate each half-year's enrollment proportionally into
    # the 2 seasonal bins that fall within it. This is a reasonable
    # approximation -- the MEST doesn't give us sub-month granularity.
    # Mapping (assumes contiguous enrollment within bin):
    #   H1 (Jan-Jun)  -> winter(Dec-Feb, 2mo of 3) + spring(3mo) + summer(3mo, but only Jun=1mo)
    # Simpler: take the corresponding half's denom / 2 per season bin.
    # This is reported as a sensitivity and doesn't need to be exact.
    den_seas <- den_half[, .(
        s1 = sprintf("%s-W", sub("-H.*", "", bin_id)),
        s2 = sprintf("%s-S", sub("-H.*", "", bin_id)),
        s3 = sprintf("%s-U", sub("-H.*", "", bin_id)),
        s4 = sprintf("%s-A", sub("-H.*", "", bin_id)),
        dm_half = dm_denom,
        zcta = zcta,
        half = sub(".*-(H[12])$", "\\1", bin_id)
    )]
    den_seas_long <- rbindlist(list(
        den_seas[half == "H1", .(zcta, bin_id = s1, dm_denom = dm_half / 2)],
        den_seas[half == "H1", .(zcta, bin_id = s2, dm_denom = dm_half / 2)],
        den_seas[half == "H2", .(zcta, bin_id = s3, dm_denom = dm_half / 2)],
        den_seas[half == "H2", .(zcta, bin_id = s4, dm_denom = dm_half / 2)]
    ))[, .(dm_denom = sum(dm_denom)), by = .(zcta, bin_id)]

    # Annual: sum the two halves
    den_year <- den_half[, .(
        dm_denom = sum(dm_denom)
    ), by = .(zcta, year = sub("-H.*", "", bin_id))]
    setnames(den_year, "year", "bin_id")

    # Rectangularize + decorate
    bins_half    <- HALF_BIN_IDS
    bins_season  <- season_dates$bin_id
    bins_annual  <- as.character(STUDY_YEARS)

    panel_half    <- rectangularize_and_decorate(num_half, den_half,
                                                  bins_half, "halfyear")
    panel_season  <- rectangularize_and_decorate(num_seas, den_seas_long,
                                                  bins_season, "seasonal")
    panel_annual  <- rectangularize_and_decorate(num_year, den_year,
                                                  bins_annual, "annual")

    panel_half    <- merge(panel_half,   half_dates,   by = "bin_id", all.x = TRUE)
    panel_season  <- merge(panel_season, season_dates, by = "bin_id", all.x = TRUE)

    # Save
    suffix <- paste0("_", cohort_label)
    saveRDS(panel_half,   file.path(OUT_DIR,
            sprintf("panel_halfyear%s.rds", suffix)))
    saveRDS(panel_season, file.path(OUT_DIR,
            sprintf("panel_seasonal%s.rds", suffix)))
    saveRDS(panel_annual, file.path(OUT_DIR,
            sprintf("panel_annual%s.rds", suffix)))
    fwrite(panel_half,    file.path(OUT_DIR,
            sprintf("panel_halfyear%s.csv", suffix)))
    fwrite(panel_season,  file.path(OUT_DIR,
            sprintf("panel_seasonal%s.csv", suffix)))
    fwrite(panel_annual,  file.path(OUT_DIR,
            sprintf("panel_annual%s.csv", suffix)))

    message("  Half-year cells: ", nrow(panel_half),
            " (", sum(panel_half$suppressed), " suppressed)")
    message("  Seasonal cells:  ", nrow(panel_season),
            " (", sum(panel_season$suppressed), " suppressed)")
    message("  Annual cells:    ", nrow(panel_annual),
            " (", sum(panel_annual$suppressed), " suppressed)")

    invisible(list(half = panel_half, seasonal = panel_season,
                   annual = panel_annual))
}

# ---- Run both cohorts -------------------------------------------------------
results <- list()
if (!is.null(cohort_cont)) {
    results$continuous <- build_cohort_panels(cohort_cont,
                                              "continuous", "continuous")
}
if (!is.null(cohort_frac)) {
    results$fractional <- build_cohort_panels(cohort_frac,
                                              "fractional", "fractional")
}

# ---- Suppression summary across cohorts -------------------------------------
supp_report <- rbindlist(lapply(names(results), function(lab) {
    data.table(
        cohort    = lab,
        bin_type  = "halfyear",
        n_cells   = nrow(results[[lab]]$half),
        n_supp    = sum(results[[lab]]$half$suppressed),
        pct_supp  = round(100 * mean(results[[lab]]$half$suppressed), 1)
    )
}))
print(supp_report)
fwrite(supp_report, file.path(OUT_DIR, "suppression_report.csv"))

message("\n04_zcta_aggregation.R complete.")
message("Panels written to: ", OUT_DIR)
message("Primary EHSA input: panel_halfyear_continuous.rds")
message("Sensitivity input:  panel_halfyear_fractional.rds")
