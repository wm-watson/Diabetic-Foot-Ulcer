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
# Source of bin membership: step3b_bin_activity.sas outputs
#   bin_activity_commercial.csv and bin_activity_medicare.csv, which
#   provide one row per (patient x calendar year x half x season x season_year)
#   with had_dm / had_l97 / had_debridement / had_amputation / had_combo flags.
#
# DFU-aware amputation censoring rule (assumptions §5.3):
#   Censor patient from both numerator and denominator in bins at or after
#   first amputation ONLY if first amputation occurred on or after first DFU.
#   Amputations that precede the first DFU claim are not treated as censoring
#   events (those are historical and unrelated to the incident ulcer).
#
# Small-cell suppression (assumptions §6.4):
#   Cell suppressed if numerator < NUM_THRESH (default 11) OR
#                     denominator < DEN_THRESH (default 20).
#
# Output: zcta x bin panel keyed on (zcta, bin_id) with
#   dm_denom, dfu_num, rate_per_1000, suppressed, bin_start, bin_end, bin_label.
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

TIER_RDS    <- file.path(ANALYTIC, "02_tiered.rds")

# ---- Parameters -------------------------------------------------------------
STUDY_YEARS  <- 2017:2022
NUM_THRESH   <- 11L     # suppress numerator < 11
DEN_THRESH   <- 20L     # suppress denominator < 20

# ---- Load tiered patient-level data -----------------------------------------
stopifnot(file_exists(TIER_RDS), file_exists(BIN_COMM), file_exists(BIN_MCR))
pt <- readRDS(TIER_RDS)
pt <- pt[dm_primary == 1L]  # PRIMARY population

# Attach zcta + tier flags we need
pt_keys <- pt[, .(data_source, patient_id, study_id, zcta,
                  tier2, first_dfu_date, first_amp_date,
                  first_dm_year, last_dm_year)]

# ---- Load bin-activity claim-level flags ------------------------------------
ba_comm <- fread(BIN_COMM, colClasses = list(character = c("submitter",
                                                           "group_policy",
                                                           "person_code")))
ba_mcr  <- fread(BIN_MCR, colClasses = list(character = "bene_id"))
ba_comm[, data_source := "COMMERCIAL"]
ba_mcr[,  data_source := "MEDICARE"]

# Reconstruct the same patient_id key that step5 builds:
#   commercial: submitter|group_policy|person_code
#   medicare:   bene_id
ba_comm[, patient_id := paste(submitter, group_policy, person_code, sep = "|")]
ba_mcr[,  patient_id := as.character(bene_id)]
ba_comm_keyed <- ba_comm
ba_mcr_keyed  <- ba_mcr

# step3b emits: had_dm, had_l97, had_combo, had_dfu (== l97 or combo).
# Debridement/amputation flags come from the patient-level analytic file
# (has_debridement / first_amp_date), NOT from bin activity.
keep_cols <- c("data_source", "patient_id", "bin_year", "half",
               "season", "season_year",
               "had_dm", "had_l97", "had_combo", "had_dfu")
ba <- rbindlist(list(ba_comm_keyed[, ..keep_cols],
                     ba_mcr_keyed[,  ..keep_cols]),
                use.names = TRUE)

# Primary window filter
ba <- ba[bin_year %in% STUDY_YEARS]

# Merge in zcta + tier/censor info
ba <- merge(ba, pt_keys, by = c("data_source", "patient_id"),
            all.x = FALSE, all.y = FALSE)

# ---- Build three bin labelings ----------------------------------------------
# 1. Half-year (PRIMARY) --------------------------------------------------
ba[, half_bin := paste0(bin_year, "-H", half)]

# 2. Seasonal (SENSITIVITY) - keys on season_year to push December to next winter
ba[, season_bin := paste0(season_year, "-", season)]
# Drop seasonal rows that escape the study window due to December rollover
ba <- ba[season_year >= min(STUDY_YEARS) & season_year <= max(STUDY_YEARS)]

# 3. Annual (descriptive) -------------------------------------------------
ba[, year_bin := as.character(bin_year)]

# ---- DFU-aware amputation censoring -----------------------------------------
# For each claim-bin row, mark whether the patient is censored as of that bin.
# A row is censored if: amp_year is not NA AND amp_year <= bin_year AND
# amp_year >= first_dfu_year_or_infinite.
ba[, amp_year := as.integer(format(first_amp_date, "%Y"))]
ba[, dfu_year := as.integer(format(first_dfu_date, "%Y"))]
ba[, censor_year := fifelse(
    !is.na(amp_year) & (is.na(dfu_year) | amp_year >= dfu_year),
    amp_year,
    NA_integer_
)]
# Censor the bin AFTER the amputation year (inclusive of years strictly >
# censor_year). The amputation year itself still counts in numerator since
# the DFU existed up to that point.
ba[, censored := !is.na(censor_year) & bin_year > censor_year]

ba_active <- ba[censored == FALSE]

# ---- Aggregate to zcta x bin ------------------------------------------------
# Numerator: at least one DFU-indicating claim in the bin (Tier 2 primary)
#   = patient is in tier2 cohort AND (had_l97 == 1 OR had_combo == 1) in bin
# Denominator: active diabetic in the bin
#   = had_dm == 1 in bin (the bin-extraction already restricts to DM-positive
#     claim rows where had_dm flag = 1)
# Deduplicate patient per (zcta, bin) before counting.

aggregate_panel <- function(bin_col, label) {
    d <- ba_active[, .(
        in_dm_denom = as.integer(any(had_dm == 1L)),
        in_dfu_num  = as.integer(tier2[1] == 1L &
                                 any(had_l97 == 1L | had_combo == 1L))
    ), by = c("zcta", bin_col, "patient_id")]

    panel <- d[, .(
        dm_denom = sum(in_dm_denom),
        dfu_num  = sum(in_dfu_num)
    ), by = c("zcta", bin_col)]
    setnames(panel, bin_col, "bin_id")
    panel[, bin_type := label]
    panel[, rate_per_1000 := 1000 * dfu_num / pmax(dm_denom, 1L)]
    panel[, suppressed := dfu_num < NUM_THRESH | dm_denom < DEN_THRESH]
    panel[]
}

panel_half    <- aggregate_panel("half_bin",   "halfyear")
panel_seasonal<- aggregate_panel("season_bin", "seasonal")
panel_annual  <- aggregate_panel("year_bin",   "annual")

# ---- Rectangularize: ensure every (zcta x bin) cell exists -----------------
rectangularize <- function(panel, all_bins) {
    all_zctas <- sort(unique(panel$zcta))
    grid <- CJ(zcta = all_zctas, bin_id = all_bins)
    out  <- merge(grid, panel, by = c("zcta", "bin_id"), all.x = TRUE)
    out[is.na(dm_denom), dm_denom := 0L]
    out[is.na(dfu_num),  dfu_num  := 0L]
    out[, bin_type := panel$bin_type[1]]
    out[, rate_per_1000 := 1000 * dfu_num / pmax(dm_denom, 1L)]
    out[, suppressed := dfu_num < NUM_THRESH | dm_denom < DEN_THRESH]
    out[]
}

bins_half     <- sort(unique(panel_half$bin_id))
bins_seasonal <- sort(unique(panel_seasonal$bin_id))
bins_annual   <- sort(unique(panel_annual$bin_id))

panel_half_rect     <- rectangularize(panel_half,     bins_half)
panel_seasonal_rect <- rectangularize(panel_seasonal, bins_seasonal)
panel_annual_rect   <- rectangularize(panel_annual,   bins_annual)

# ---- Bin date metadata for EHSA cube ----------------------------------------
half_dates <- data.table(
    bin_id    = c(sapply(STUDY_YEARS, function(y) paste0(y, c("-H1", "-H2")))),
    bin_start = as.Date(c(sapply(STUDY_YEARS,
                    function(y) c(sprintf("%d-01-01", y),
                                  sprintf("%d-07-01", y))))),
    bin_end   = as.Date(c(sapply(STUDY_YEARS,
                    function(y) c(sprintf("%d-06-30", y),
                                  sprintf("%d-12-31", y)))))
)
panel_half_rect <- merge(panel_half_rect, half_dates, by = "bin_id", all.x = TRUE)

season_months <- list(W = c(12, 1, 2), S = c(3, 4, 5),
                      U = c(6, 7, 8), A = c(9, 10, 11))
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
panel_seasonal_rect <- merge(panel_seasonal_rect, season_dates,
                             by = "bin_id", all.x = TRUE)

# ---- Suppression report -----------------------------------------------------
supp_report <- rbind(
    panel_half_rect[,     .(bin_type = "halfyear",
                            n_cells = .N,
                            n_supp = sum(suppressed),
                            pct_supp = round(100 * mean(suppressed), 1))],
    panel_seasonal_rect[, .(bin_type = "seasonal",
                            n_cells = .N,
                            n_supp = sum(suppressed),
                            pct_supp = round(100 * mean(suppressed), 1))],
    panel_annual_rect[,   .(bin_type = "annual",
                            n_cells = .N,
                            n_supp = sum(suppressed),
                            pct_supp = round(100 * mean(suppressed), 1))]
)
print(supp_report)

# ---- Save -------------------------------------------------------------------
saveRDS(panel_half_rect,     file.path(OUT_DIR, "panel_halfyear.rds"))
saveRDS(panel_seasonal_rect, file.path(OUT_DIR, "panel_seasonal.rds"))
saveRDS(panel_annual_rect,   file.path(OUT_DIR, "panel_annual.rds"))
fwrite(panel_half_rect,      file.path(OUT_DIR, "panel_halfyear.csv"))
fwrite(panel_seasonal_rect,  file.path(OUT_DIR, "panel_seasonal.csv"))
fwrite(panel_annual_rect,    file.path(OUT_DIR, "panel_annual.csv"))
fwrite(supp_report,          file.path(OUT_DIR, "suppression_report.csv"))

message("\n04_zcta_aggregation.R complete.")
message("Panels written to: ", OUT_DIR)
message("Primary EHSA input: panel_halfyear.rds (", nrow(panel_half_rect),
        " ZCTA x bin cells; ",
        sum(panel_half_rect$suppressed), " suppressed)")
