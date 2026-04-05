# =============================================================================
# 01_load_and_clean.R
#
# Load dm_dfu_analytic.csv (step5 output), harmonize types, apply exclusions,
# cross-payer deduplicate, and write a clean analytic data.table.
#
# Every exclusion is logged to flow_counts for the CONSORT-style diagram.
#
# Assumptions documented in R/_assumptions.md §1, §2, §7.
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(fs)
})

# ---- Paths ------------------------------------------------------------------
# DFU_DROPBOX_DIR env var overrides the default for CI/test runs.
DROPBOX_DIR  <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
CSV_IN       <- file.path(DROPBOX_DIR, "dm_dfu_analytic.csv")
OUT_DIR      <- file.path(DROPBOX_DIR, "analytic")
dir_create(OUT_DIR)

CLEAN_RDS    <- file.path(OUT_DIR, "01_clean.rds")
FLOW_RDS     <- file.path(OUT_DIR, "01_flow_counts.rds")

stopifnot(file_exists(CSV_IN))

# ---- Load -------------------------------------------------------------------
# Use fread with explicit NA handling; dates parsed downstream
dat <- fread(
    CSV_IN,
    na.strings = c("", "NA", ".", "NULL"),
    colClasses = list(character = c("patient_id", "study_id", "ar_zip",
                                    "state", "county", "sex", "race_cd",
                                    "rti_race_cd", "data_source",
                                    "diabetes_type", "zip_source", "dfu_source"))
)

message("Loaded ", format(nrow(dat), big.mark = ","), " rows from ", CSV_IN)

# ---- Flow log ---------------------------------------------------------------
# Initialize a flow counter that every exclusion step updates
flow <- list()
add_flow <- function(label, n) {
    flow[[length(flow) + 1L]] <<- data.table(step = label, n = as.integer(n))
}
add_flow("01_loaded_raw_rows", nrow(dat))
add_flow("01_unique_patients_raw", uniqueN(dat$patient_id))
add_flow("01_unique_study_ids_raw", uniqueN(dat[study_id != ""]$study_id))

# ---- Type harmonization -----------------------------------------------------
# Dates: step5 exports with yymmdd10. format -> character; parse to Date
date_cols <- c("first_dm_date", "first_dfu_date",
               "first_debride_date", "first_amp_date")
for (col in date_cols) {
    if (col %in% names(dat)) {
        dat[, (col) := as.Date(get(col))]
    }
}

# Integer year fields
year_cols <- c("first_dm_year", "last_dm_year",
               "first_dfu_year", "last_dfu_year")
for (col in year_cols) {
    if (col %in% names(dat)) {
        dat[, (col) := as.integer(get(col))]
    }
}

# Numeric counts and flags
num_cols <- c("n_dm_claims", "n_dfu_claims", "n_debride_claims", "n_amp_claims",
              "ever_l97", "ever_dm_combo", "severity_rank",
              "has_dfu", "has_debridement", "has_amputation",
              "age", "days_dm_to_dfu")
for (col in num_cols) {
    if (col %in% names(dat)) {
        dat[, (col) := as.numeric(get(col))]
    }
}

# ---- Exclusion 1: Missing ZIP -----------------------------------------------
# Step5 already filtered to valid AR ZIPs, but re-check in R
before <- nrow(dat)
dat <- dat[!is.na(ar_zip) & ar_zip != "" & nchar(ar_zip) == 5]
add_flow("02_excl_missing_zip", before - nrow(dat))

# ---- Exclusion 2: Non-Arkansas ZIP ------------------------------------------
before <- nrow(dat)
dat <- dat[substr(ar_zip, 1, 2) %chin% c("71", "72")]
add_flow("03_excl_non_AR_zip", before - nrow(dat))

# ---- Exclusion 3: Age < 18 --------------------------------------------------
# Missing age -> retain (don't drop on missingness); drop only if known <18
before <- nrow(dat)
dat <- dat[is.na(age) | age >= 18]
add_flow("04_excl_age_under_18", before - nrow(dat))

# ---- Exclusion 4: Cross-payer deduplication (prefer Medicare) --------------
# Rule: if same study_id appears in both COMMERCIAL and MEDICARE, keep MEDICARE
# Patients with study_id == "" cannot be deduplicated; retained as-is.
before <- nrow(dat)

dat[, src_rank := fifelse(data_source == "MEDICARE", 1L, 2L)]
setorder(dat, study_id, src_rank)

linked <- dat[study_id != ""]
unlinked <- dat[study_id == ""]

# For linked patients, keep first row per study_id (Medicare preferred)
linked_dedup <- linked[, .SD[1L], by = study_id]

dat <- rbindlist(list(linked_dedup, unlinked), use.names = TRUE, fill = TRUE)
dat[, src_rank := NULL]

add_flow("05_dropped_cross_payer_dups", before - nrow(dat))

# ---- Exclusion 5: Drop remaining patient_id duplicates (safety net) --------
# Within a single data_source, patient_id should be unique. Drop any stragglers.
before <- nrow(dat)
dat <- unique(dat, by = c("data_source", "patient_id"))
add_flow("06_dropped_pid_dups_within_source", before - nrow(dat))

add_flow("07_final_analytic_rows", nrow(dat))

# ---- Derived fields ---------------------------------------------------------
# ZCTA (5-digit ZIP treated as ZCTA proxy for AR; true crosswalk applied in 04)
dat[, zcta := ar_zip]

# Age groups
dat[, age_group := fcase(
    is.na(age),                       "Unknown",
    age < 45,                         "18-44",
    age >= 45 & age < 65,             "45-64",
    age >= 65 & age < 75,             "65-74",
    age >= 75,                        "75+"
)]
dat[, age_group := factor(age_group,
                          levels = c("18-44", "45-64", "65-74", "75+", "Unknown"))]

# Sex harmonization (already M/F/U from step5 but enforce)
dat[, sex := fcase(
    sex %chin% c("M", "1"), "M",
    sex %chin% c("F", "2"), "F",
    default = "U"
)]

# Race: harmonized race only for Medicare via rti_race_cd; commercial -> NA
# RTI race codes (CMS): 0=unknown, 1=NH White, 2=Black, 3=Other, 4=Asian/PI,
# 5=Hispanic, 6=AI/AN
dat[, race_harmonized := fcase(
    data_source == "COMMERCIAL",                       NA_character_,
    is.na(rti_race_cd) | rti_race_cd %chin% c("", "0"), "Unknown",
    rti_race_cd == "1",                                "NH White",
    rti_race_cd == "2",                                "NH Black",
    rti_race_cd == "3",                                "Other",
    rti_race_cd == "4",                                "Asian/PI",
    rti_race_cd == "5",                                "Hispanic",
    rti_race_cd == "6",                                "AI/AN",
    default = "Unknown"
)]

# Diabetes type factor (primary analysis excludes AMBIGUOUS in 02_tier_assignment)
dat[, diabetes_type := factor(diabetes_type,
                              levels = c("T1D", "T2D", "AMBIGUOUS"))]

# ---- Save -------------------------------------------------------------------
flow_dt <- rbindlist(flow)
print(flow_dt)

saveRDS(dat, CLEAN_RDS)
saveRDS(flow_dt, FLOW_RDS)

message("\nSaved cleaned data: ", CLEAN_RDS)
message("Saved flow counts:  ", FLOW_RDS)
message("Final rows: ", format(nrow(dat), big.mark = ","),
        " | Unique patients: ", format(uniqueN(dat$patient_id), big.mark = ","),
        " | Unique study_ids: ", format(uniqueN(dat[study_id != ""]$study_id), big.mark = ","))
