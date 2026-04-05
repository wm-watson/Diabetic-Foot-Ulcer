# =============================================================================
# run_all.R
#
# Orchestrator for the R portion of the DFU Arkansas geospatial pipeline.
# Runs scripts 01 -> 06 in order. After this completes, hand off to ArcGIS
# Pro using the CSV + shapefile in outputs/ehsa_input/ to build the space-
# time cube and run Emerging Hot Spot Analysis.
#
# Usage from the R console or shell:
#   Rscript R/run_all.R
#
# Assumptions: R/_assumptions.md (all sections)
# =============================================================================

HERE <- normalizePath(dirname(sys.frame(1)$ofile %||% "R"), mustWork = FALSE)
if (!dir.exists(HERE)) HERE <- "R"

scripts <- c(
    "01_load_and_clean.R",
    "02_tier_assignment.R",
    "03_descriptives.R",
    "04_zcta_aggregation.R",
    "05_static_spatial.R",
    "06_ehsa_prep.R"
)

t0 <- Sys.time()
for (s in scripts) {
    path <- file.path(HERE, s)
    message("\n================================================================")
    message("[run_all] ", Sys.time(), "  Running ", s)
    message("================================================================")
    source(path, echo = FALSE)
}
message("\n[run_all] Complete. Total time: ",
        round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
        " minutes.")
message("[run_all] Next step: open ArcGIS Pro and build the space-time cube ",
        "from outputs/ehsa_input/ehsa_input_halfyear.csv")
