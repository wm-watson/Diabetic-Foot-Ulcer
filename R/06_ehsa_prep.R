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
panel_half     <- readRDS(file.path(PANELS, "panel_halfyear.rds"))
panel_seasonal <- readRDS(file.path(PANELS, "panel_seasonal.rds"))

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
                       file.path(OUT_DIR, "ehsa_input_halfyear.csv"))
ehsa_seas <- make_ehsa(panel_seasonal,
                       file.path(OUT_DIR, "ehsa_input_seasonal.csv"))

# ---- Export ZCTA shapefile (matching ZCTAs present in panel) ----------------
# tigris::zctas(state=) only works for 2000/2010 vintages. For 2020, pull the
# national ZCTA file and subset to Arkansas ZCTAs (those starting with 71/72).
zcta_set <- union(unique(panel_half$zcta), unique(panel_seasonal$zcta))
ar <- zctas(year = 2020, progress_bar = FALSE)
ar$zcta <- ar$ZCTA5CE20
ar <- ar[substr(ar$zcta, 1, 2) %in% c("71", "72"), ]
ar <- ar[ar$zcta %in% zcta_set, ]
ar <- st_transform(ar, 5070)

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
