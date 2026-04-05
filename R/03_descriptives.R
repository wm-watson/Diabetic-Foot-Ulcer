# =============================================================================
# 03_descriptives.R
#
# Descriptive analysis for the PCD paper:
#   - CONSORT-style flow diagram (flow_counts.rds + 02 tier counts)
#   - Table 1 (demographics) stratified by DFU status (Tier 2 primary)
#   - Table 2 (DFU phenotype: debridement, amputation, time-to-DFU)
#   - Overall crude DFU prevalence by year and by payer
#   - Descriptive maps are produced in 05_static_spatial.R
#
# Primary population: dm_primary == 1 (T1D + T2D, excludes AMBIGUOUS)
# Primary DFU definition: tier2 == 1 (with tier2_temporal as a row in Table 2)
#
# Assumptions: R/_assumptions.md §2, §3, §6, §7
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(gtsummary)
    library(gt)
    library(fs)
    library(ggplot2)
})

# ---- Paths ------------------------------------------------------------------
DROPBOX_DIR <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
ANALYTIC    <- file.path(DROPBOX_DIR, "analytic")
OUT_DIR     <- file.path(DROPBOX_DIR, "outputs", "descriptives")
dir_create(OUT_DIR)

TIER_RDS     <- file.path(ANALYTIC, "02_tiered.rds")
FLOW_RDS     <- file.path(ANALYTIC, "01_flow_counts.rds")
TIER_FLOW    <- file.path(ANALYTIC, "02_tier_counts.rds")

stopifnot(file_exists(TIER_RDS), file_exists(FLOW_RDS))

dat        <- readRDS(TIER_RDS)
flow_load  <- readRDS(FLOW_RDS)
tier_flow  <- if (file_exists(TIER_FLOW)) readRDS(TIER_FLOW) else NULL

# =============================================================================
# 1. CONSORT-STYLE FLOW DIAGRAM (COUNTS)
# =============================================================================
# Combine load-step and tier-step counts into a single table that drives
# the flow figure. The figure itself is drawn in the paper repo; here we
# emit the numeric table the caption depends on.

flow_full <- rbindlist(list(
    flow_load,
    data.table(step = "08_primary_dm_T1D_T2D_only",
               n    = dat[dm_primary == 1L, .N]),
    data.table(step = "09_tier1_any_L97_or_combo",
               n    = dat[dm_primary == 1L & tier1 == 1L, .N]),
    data.table(step = "10_tier2_primary_L97_plus_debride_or_combo",
               n    = dat[dm_primary == 1L & tier2 == 1L, .N]),
    data.table(step = "11_tier2_temporal_claimlevel_30d_forward",
               n    = dat[dm_primary == 1L &
                           !is.na(tier2_temporal) & tier2_temporal == 1L, .N]),
    data.table(step = "12_tier2b_365d_patientlevel",
               n    = dat[dm_primary == 1L & tier2b == 1L, .N]),
    data.table(step = "13_tier3_combo_only",
               n    = dat[dm_primary == 1L & tier3 == 1L, .N])
), use.names = TRUE)

fwrite(flow_full, file.path(OUT_DIR, "flow_counts_full.csv"))
print(flow_full)

# =============================================================================
# 2. TABLE 1 — DEMOGRAPHICS BY DFU STATUS (TIER 2 PRIMARY)
# =============================================================================
# Population: dm_primary == 1
# Grouping:   tier2 (1 = DFU, 0 = no DFU among diabetics)

tbl1_dat <- dat[dm_primary == 1L,
                .(age, age_group, sex, race_harmonized,
                  data_source, diabetes_type, tier2)]

tbl1_dat[, dfu := factor(fifelse(tier2 == 1L, "DFU (Tier 2)", "No DFU"),
                         levels = c("No DFU", "DFU (Tier 2)"))]

tbl1 <- tbl1_dat[, .(age, age_group, sex, race_harmonized,
                     data_source, diabetes_type, dfu)] |>
    tbl_summary(
        by = dfu,
        missing_text = "Missing",
        label = list(
            age             ~ "Age (years)",
            age_group       ~ "Age group",
            sex             ~ "Sex",
            race_harmonized ~ "Race/ethnicity (Medicare only, RTI)",
            data_source     ~ "Payer source",
            diabetes_type   ~ "Diabetes type"
        ),
        statistic = list(
            all_continuous()  ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        )
    ) |>
    add_overall() |>
    add_p(test = list(all_continuous() ~ "wilcox.test",
                      all_categorical() ~ "chisq.test")) |>
    modify_header(label ~ "**Characteristic**") |>
    modify_caption("**Table 1.** Characteristics of Arkansas diabetic
                    patients with and without diabetic foot ulcer (Tier 2,
                    primary definition), AR APCD 2014–2024")

gtsave(as_gt(tbl1), file.path(OUT_DIR, "table1_demographics.html"))
saveRDS(tbl1, file.path(OUT_DIR, "table1_demographics.rds"))

# =============================================================================
# 3. TABLE 2 — DFU PHENOTYPE AND PROCEDURES
# =============================================================================
# Among DFU patients (Tier 2 primary), describe clinical activity.

tbl2_dat <- dat[dm_primary == 1L & tier2 == 1L,
                .(data_source, diabetes_type,
                  has_debridement, has_amputation,
                  days_dm_to_dfu,
                  n_dfu_claims, n_debride_claims, n_amp_claims,
                  tier2_temporal, tier2b, tier3)]

tbl2_dat[, days_dm_to_dfu := as.numeric(days_dm_to_dfu)]
tbl2_dat[, tier2_temporal := factor(fifelse(is.na(tier2_temporal), NA_character_,
                                            fifelse(tier2_temporal == 1L,
                                                    "Yes", "No")),
                                    levels = c("No", "Yes"))]
tbl2_dat[, tier2b := factor(fifelse(tier2b == 1L, "Yes", "No"),
                            levels = c("No", "Yes"))]
tbl2_dat[, tier3 := factor(fifelse(tier3 == 1L, "Yes", "No"),
                           levels = c("No", "Yes"))]

tbl2 <- tbl2_dat |>
    tbl_summary(
        by = data_source,
        missing_text = "Missing",
        label = list(
            diabetes_type    ~ "Diabetes type",
            has_debridement  ~ "Any debridement claim",
            has_amputation   ~ "Any amputation claim",
            days_dm_to_dfu   ~ "Days from first DM to first DFU",
            n_dfu_claims     ~ "Number of DFU (L97/combo) claims",
            n_debride_claims ~ "Number of debridement claims",
            n_amp_claims     ~ "Number of amputation claims",
            tier2_temporal   ~ "Tier 2 temporal (SAS 30d forward match)",
            tier2b           ~ "Tier 2b (debridement within 365d of DFU)",
            tier3            ~ "Tier 3 (combo code only)"
        ),
        statistic = list(
            all_continuous()  ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        )
    ) |>
    add_overall() |>
    modify_header(label ~ "**Characteristic**") |>
    modify_caption("**Table 2.** Clinical phenotype of diabetic foot ulcer
                    patients (Tier 2, primary), AR APCD")

gtsave(as_gt(tbl2), file.path(OUT_DIR, "table2_dfu_phenotype.html"))
saveRDS(tbl2, file.path(OUT_DIR, "table2_dfu_phenotype.rds"))

# =============================================================================
# 4. SENSITIVITY TABLE — INCLUDE AMBIGUOUS-DM
# =============================================================================
# Does including AMBIGUOUS-DM patients in Tier 1 and Tier 2 change counts?

sens <- dat[, .(
    n_total         = .N,
    n_dm_primary    = sum(dm_primary),
    n_dm_with_amb   = sum(dm_with_ambiguous),
    tier1_primary   = sum(dm_primary == 1L & tier1 == 1L),
    tier1_with_amb  = sum(dm_with_ambiguous == 1L & tier1 == 1L),
    tier2_primary   = sum(dm_primary == 1L & tier2 == 1L),
    tier2_with_amb  = sum(dm_with_ambiguous == 1L & tier2 == 1L),
    tier3_primary   = sum(dm_primary == 1L & tier3 == 1L),
    tier3_with_amb  = sum(dm_with_ambiguous == 1L & tier3 == 1L)
), by = data_source]

fwrite(sens, file.path(OUT_DIR, "sensitivity_ambiguous_dm_counts.csv"))
print(sens)

# =============================================================================
# 5. CRUDE DFU PREVALENCE BY YEAR (PRIMARY WINDOW 2017–2022)
# =============================================================================
# Active diabetic = first_dm_year <= Y <= last_dm_year
# DFU prevalent  = tier2 == 1 & first_dfu_year <= Y <= last_dfu_year
# DFU-aware amputation censor: if first_amp_date exists and Y >= year of
# first_amp_date AND year of first_amp_date >= first_dfu_year, exit denom/num.

years <- 2017:2022

yearly <- rbindlist(lapply(years, function(Y) {
    d <- dat[dm_primary == 1L]
    d[, first_amp_year := as.integer(format(first_amp_date, "%Y"))]
    d[, censored := !is.na(first_amp_year) &
                    first_amp_year <= Y &
                    (is.na(first_dfu_year) | first_amp_year >= first_dfu_year)]

    active_dm <- d[first_dm_year <= Y & last_dm_year >= Y & !censored]
    dfu_prev  <- active_dm[tier2 == 1L &
                           !is.na(first_dfu_year) &
                           first_dfu_year <= Y &
                           last_dfu_year  >= Y]

    data.table(
        year = Y,
        dm_denom       = nrow(active_dm),
        dfu_num        = nrow(dfu_prev),
        rate_per_1000  = 1000 * nrow(dfu_prev) / pmax(nrow(active_dm), 1L)
    )
}))

fwrite(yearly, file.path(OUT_DIR, "yearly_dfu_prevalence_primary.csv"))
print(yearly)

p <- ggplot(yearly, aes(x = year, y = rate_per_1000)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = years) +
    labs(x = "Year",
         y = "Prevalent DFU per 1,000 active diabetics",
         title = "Crude DFU prevalence, Arkansas APCD 2017–2022 (Tier 2 primary)",
         caption = "Source: AR APCD; Tier 2 = L97 + debridement or combo code; ambiguous-DM excluded") +
    theme_minimal(base_size = 12)
ggsave(file.path(OUT_DIR, "fig_yearly_dfu_prevalence.png"),
       p, width = 7, height = 4.5, dpi = 300)

# =============================================================================
# 6. PAYER-STRATIFIED YEARLY PREVALENCE (SENSITIVITY)
# =============================================================================
yearly_payer <- rbindlist(lapply(years, function(Y) {
    d <- dat[dm_primary == 1L]
    d[, first_amp_year := as.integer(format(first_amp_date, "%Y"))]
    d[, censored := !is.na(first_amp_year) &
                    first_amp_year <= Y &
                    (is.na(first_dfu_year) | first_amp_year >= first_dfu_year)]
    active <- d[first_dm_year <= Y & last_dm_year >= Y & !censored]
    out <- active[, .(dm_denom = .N,
                      dfu_num  = sum(tier2 == 1L &
                                     !is.na(first_dfu_year) &
                                     first_dfu_year <= Y & last_dfu_year >= Y)),
                  by = data_source]
    out[, year := Y]
    out[]
}))
yearly_payer[, rate_per_1000 := 1000 * dfu_num / pmax(dm_denom, 1L)]

fwrite(yearly_payer, file.path(OUT_DIR, "yearly_dfu_prevalence_by_payer.csv"))

message("\n03_descriptives.R complete.")
message("Outputs written to: ", OUT_DIR)
