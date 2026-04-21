# =============================================================================
# 06_ehsa_prep.R
#
# Prepare inputs for ArcGIS Pro Emerging Hot Spot Analysis (EHSA) space-time
# cube construction. Emits two CSVs plus the matching ZCTA polygon shapefile.
#
#   - ehsa_input_halfyear.csv  (PRIMARY: 12 H1/H2 slices, 2017-2022)
#   - ehsa_input_seasonal.csv  (SENSITIVITY: 24 meteorological bins)
#
# Columns per row: zcta, bin_start (yyyy-mm-dd), rate_per_1000,
#                  dm_denom, dfu_num, suppressed
#
# In ArcGIS Pro:
#   1. Project ZCTA shapefile to EPSG:5070 (Conus Albers).
#   2. Run "Create Space Time Cube From Defined Locations" with:
#        Locations:   AR ZCTA polygons
#        Input File:  ehsa_input_halfyear.csv
#        Location ID: zcta
#        Time Field:  bin_start
#        Time Step:   6 months (primary) / 3 months (seasonal sensitivity)
#        Variables:   rate_per_1000 (SUM -> but each zcta-bin has ONE row)
#   3. Run "Emerging Hot Spot Analysis" on the cube:
#        Neighborhood Distance: 8 nearest neighbors (or default)
#        Neighborhood Time Step: 1
#        Value Field:  RATE_PER_1000_SUM_ZEROS
#
# Suppressed cells (NA rate) are written with empty rate_per_1000 so the
# cube treats them as missing rather than zero.
#
# Assumptions: R/_assumptions.md §4, §5.5, §5.6, §6.4, §8.3
# =============================================================================

suppressPackageStartupMessages({
    library(data.table)
    library(sf)
    library(tigris)
    library(fs)
})

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ---- Paths ------------------------------------------------------------------
DROPBOX_DIR <- Sys.getenv(
    "DFU_DROPBOX_DIR",
    "/Users/williamwatson/Library/CloudStorage/Dropbox/Dissertation/Aim 3 - DFU"
)
PANELS      <- file.path(DROPBOX_DIR, "outputs", "panels")
OUT_DIR     <- file.path(DROPBOX_DIR, "outputs", "ehsa_input")
dir_create(OUT_DIR)

# ---- Load panels ------------------------------------------------------------
# Cohort selection is controlled by DFU_COHORT env var; default is the
# primary (continuous enrollment) cohort. Fractional is the sensitivity
# input for the EHSA robustness check. See assumptions §6.1.
COHORT <- Sys.getenv("DFU_COHORT", "continuous")
load_panel <- function(base) {
    f <- file.path(PANELS, sprintf("panel_%s_%s.rds", base, COHORT))
    if (!file.exists(f)) {
        legacy <- file.path(PANELS, sprintf("panel_%s.rds", base))
        if (file.exists(legacy)) {
            message("Using legacy ", basename(legacy),
                    " (pre-cohort pipeline)")
            return(readRDS(legacy))
        }
        stop("Panel not found: ", f)
    }
    message("Using cohort panel: ", basename(f))
    readRDS(f)
}
panel_half     <- load_panel("halfyear")
panel_seasonal <- load_panel("seasonal")

# ---- Build EHSA CSVs --------------------------------------------------------
make_ehsa <- function(panel, outfile) {
    out <- copy(panel)
    # No suppression applied to EHSA input — the space-time cube is an
    # analytic method, not a published display. Suppression is applied
    # only to choropleth maps and descriptive count/rate tables.
    # Zero-denominator cells get rate = 0 (not NA) so the cube has a
    # complete panel for Mann-Kendall trend detection.
    out[, rate_per_1000 := fifelse(dm_denom == 0L, 0, rate_per_1000)]
    out[, bin_start := format(bin_start, "%Y-%m-%d")]
    out <- out[, .(zcta, bin_start, rate_per_1000,
                   dm_denom, dfu_num)]
    setorder(out, zcta, bin_start)
    fwrite(out, outfile)
    out
}

ehsa_half <- make_ehsa(panel_half,
                       file.path(OUT_DIR,
                                 sprintf("ehsa_input_halfyear_%s.csv", COHORT)))
ehsa_seas <- make_ehsa(panel_seasonal,
                       file.path(OUT_DIR,
                                 sprintf("ehsa_input_seasonal_%s.csv", COHORT)))

# ---- Export ZCTA shapefile (matching ZCTAs present in panel) ----------------
# tigris::zctas(state=) only works for 2000/2010 vintages. For 2020, pull the
# national ZCTA file and SPATIALLY clip to the Arkansas state polygon. A
# naive prefix filter on "71"/"72" includes Louisiana ZCTAs (LA = 700-714);
# we restrict to ZCTAs whose centroid lies inside AR.
zcta_set <- union(unique(panel_half$zcta), unique(panel_seasonal$zcta))

ar_state <- states(progress_bar = FALSE)
ar_state <- ar_state[ar_state$STUSPS == "AR", ]
ar_state <- st_transform(ar_state, 5070)

ar <- zctas(year = 2020, progress_bar = FALSE)
ar$zcta <- ar$ZCTA5CE20
ar <- st_transform(ar, 5070)
ar_centroids <- st_centroid(ar)
inside_ar    <- st_intersects(ar_centroids, ar_state, sparse = FALSE)[, 1]
ar <- ar[inside_ar, ]
ar <- ar[ar$zcta %in% zcta_set, ]

st_write(ar, file.path(OUT_DIR, "ar_zctas_for_ehsa.shp"),
         delete_dsn = TRUE, quiet = TRUE)

# ---- Sanity report ----------------------------------------------------------
report <- data.table(
    file = c("ehsa_input_halfyear.csv", "ehsa_input_seasonal.csv"),
    n_rows = c(nrow(ehsa_half), nrow(ehsa_seas)),
    n_zctas = c(uniqueN(ehsa_half$zcta), uniqueN(ehsa_seas$zcta)),
    n_bins  = c(uniqueN(ehsa_half$bin_start), uniqueN(ehsa_seas$bin_start)),
    n_zero_denom = c(sum(ehsa_half$dm_denom == 0), sum(ehsa_seas$dm_denom == 0)),
    pct_zero_denom = c(round(100 * mean(ehsa_half$dm_denom == 0), 1),
                       round(100 * mean(ehsa_seas$dm_denom == 0), 1))
)
fwrite(report, file.path(OUT_DIR, "ehsa_input_report.csv"))
print(report)

message("\n06_ehsa_prep.R complete.")
message("EHSA inputs written to: ", OUT_DIR)
message("Primary cube input: ehsa_input_halfyear.csv (",
        nrow(ehsa_half), " cells, ",
        uniqueN(ehsa_half$bin_start), " time slices)")
message("Load these files in ArcGIS Pro to build the space-time cube.")
