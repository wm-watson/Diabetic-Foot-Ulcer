# =============================================================================
# 02_tier_assignment.R
#
# Apply the three-tier DFU case definition and create analytic cohort flags.
#
# Tier 1 (broadest):  any L97 OR combo code
# Tier 2 (primary):   (L97 AND debridement) OR combo code
# Tier 3 (strictest): combo code only
#
# Also creates:
#   - dm_primary           : T1D + T2D only (ambiguous excluded)
#   - dm_with_ambiguous    : T1D + T2D + ambiguous (sensitivity)
#   - tier2_temporal_flag  : pass-through of SAS-computed claim-level
#                            forward 30-day L97 -> debridement match
#                            (Barshes-style); computed in step3_cohort.sas
#                            Part T. This is the authoritative temporal
#                            Tier 2. tier2b (365-day, patient-level) is
#                            retained as a coarse sensitivity only.
#
# Assumptions documented in R/_assumptions.md §3.
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(fs)
})

DROPBOX_DIR  <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
OUT_DIR      <- file.path(DROPBOX_DIR, "analytic")
CLEAN_RDS    <- file.path(OUT_DIR, "01_clean.rds")
TIERED_RDS   <- file.path(OUT_DIR, "02_tiered.rds")

stopifnot(file_exists(CLEAN_RDS))

dat <- readRDS(CLEAN_RDS)

# ---- Primary diabetes cohort flags -----------------------------------------
dat[, dm_primary        := diabetes_type %in% c("T1D", "T2D")]
dat[, dm_with_ambiguous := diabetes_type %in% c("T1D", "T2D", "AMBIGUOUS")]

# ---- DFU tier flags ---------------------------------------------------------
# ever_l97 and ever_dm_combo already derived in step3 per-patient
# has_debridement already derived in step3 per-patient (from ALL claims,
# restricted to DM patients via P3 join)

dat[, tier1 := as.integer(ever_l97 == 1L | ever_dm_combo == 1L)]

dat[, tier2 := as.integer(
    (ever_l97 == 1L & has_debridement == 1L) | ever_dm_combo == 1L
)]

dat[, tier3 := as.integer(ever_dm_combo == 1L)]

# ---- Tier 2 temporal (SAS claim-level 30-day forward match) -----------------
# step3_cohort.sas Part T computes tier2_temporal: a patient qualifies if they
# have at least one L97 claim followed within 0-30 days by a debridement
# claim (forward-only, Barshes-style). Combo-code patients always qualify.
# If the column is missing (older CSV), fall back to tier2b.
if ("tier2_temporal" %in% names(dat)) {
    dat[, tier2_temporal := as.integer(tier2_temporal)]
} else {
    warning("tier2_temporal not found in input; falling back to tier2b as proxy")
    dat[, tier2_temporal := NA_integer_]
}

# ---- Tier 2b (stricter, date-proximity) -------------------------------------
# Approximation of Barshes-style temporal match: first debridement must occur
# between first DFU claim date and 365 days after. Patients with combo code
# always qualify.
dat[, days_dfu_to_debride := as.integer(first_debride_date - first_dfu_date)]

dat[, tier2b := as.integer(
    ever_dm_combo == 1L |
    (ever_l97 == 1L
     & has_debridement == 1L
     & !is.na(days_dfu_to_debride)
     & days_dfu_to_debride >= 0L
     & days_dfu_to_debride <= 365L)
)]

# ---- Sanity checks: tier ordering must hold ---------------------------------
# Every Tier 3 must be Tier 2; every Tier 2 must be Tier 1; every Tier 2b must
# be Tier 2.
stopifnot(dat[tier3 == 1 & tier2 == 0, .N] == 0L)
stopifnot(dat[tier2 == 1 & tier1 == 0, .N] == 0L)
stopifnot(dat[tier2b == 1 & tier2 == 0, .N] == 0L)
if (!all(is.na(dat$tier2_temporal))) {
    stopifnot(dat[tier2_temporal == 1 & tier2 == 0, .N] == 0L)
}

# ---- Apply amputation censoring flags ---------------------------------------
# A patient is censored from year Y+ if first_amp_date falls in year Y.
# The actual year-level filtering happens in 04_zcta_aggregation.R; here we
# just ensure first_amp_year is available.
dat[, first_amp_year := year(first_amp_date)]

# ---- Summary tables ---------------------------------------------------------
cat("\n=== DFU tier counts by data source (T1D + T2D, primary) ===\n")
print(
    dat[dm_primary == TRUE,
        .(n          = .N,
          tier1      = sum(tier1),
          tier2          = sum(tier2),
          tier2_temporal = sum(tier2_temporal, na.rm = TRUE),
          tier2b         = sum(tier2b),
          tier3          = sum(tier3)),
        by = data_source]
)

cat("\n=== DFU tier counts with AMBIGUOUS included (sensitivity) ===\n")
print(
    dat[dm_with_ambiguous == TRUE,
        .(n          = .N,
          tier1      = sum(tier1),
          tier2          = sum(tier2),
          tier2_temporal = sum(tier2_temporal, na.rm = TRUE),
          tier2b         = sum(tier2b),
          tier3          = sum(tier3)),
        by = data_source]
)

cat("\n=== Tier 2 vs Tier 2b discordance (Tier 2 only, T1/T2) ===\n")
print(
    dat[dm_primary == TRUE & tier2 == 1,
        .(tier2_only_no_tier2b = sum(tier2 == 1 & tier2b == 0),
          both_tier2_and_2b    = sum(tier2 == 1 & tier2b == 1)),
        by = data_source]
)

cat("\n=== Distribution of days from first DFU to first debridement ===\n")
print(
    dat[dm_primary == TRUE & ever_l97 == 1 & has_debridement == 1,
        .(n        = .N,
          p05      = as.numeric(quantile(days_dfu_to_debride, 0.05, na.rm = TRUE)),
          median   = as.numeric(median(days_dfu_to_debride, na.rm = TRUE)),
          p95      = as.numeric(quantile(days_dfu_to_debride, 0.95, na.rm = TRUE)),
          n_before = sum(days_dfu_to_debride < 0, na.rm = TRUE),
          n_after  = sum(days_dfu_to_debride >= 0, na.rm = TRUE),
          n_na     = sum(is.na(days_dfu_to_debride))),
        by = data_source]
)

saveRDS(dat, TIERED_RDS)
message("\nSaved tiered data: ", TIERED_RDS)
