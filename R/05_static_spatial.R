# =============================================================================
# 05_static_spatial.R
#
# Static (time-pooled) spatial analysis of DFU prevalence across AR ZCTAs,
# using the 2017-2022 window aggregated over all bins (primary figure) and
# separately for each calendar year (small multiples).
#
#   - Global Moran's I (adaptive KNN, k = 8, 999 permutations)
#   - Local Getis-Ord Gi* with FDR-adjusted permutation p-values
#   - LISA (Local Moran's I) as sensitivity
#   - Choropleth maps (raw rate, Gi* z-score, significance bins)
#
# Assumptions: R/_assumptions.md §4, §8
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
PANELS      <- file.path(DROPBOX_DIR, "outputs", "panels")
OUT_DIR     <- file.path(DROPBOX_DIR, "outputs", "static_spatial")
dir_create(OUT_DIR)

# ---- Parameters -------------------------------------------------------------
KNN_K        <- 8L
NPERM        <- 999L
FDR_ALPHA    <- 0.05

# ---- Load panel -------------------------------------------------------------
# Cohort selection is controlled by DFU_COHORT env var; default is the
# primary (continuous enrollment) cohort. See assumptions §6.1 for the
# two-cohort design rationale.
COHORT     <- Sys.getenv("DFU_COHORT", "continuous")
panel_file <- file.path(PANELS, sprintf("panel_halfyear_%s.rds", COHORT))
if (!file.exists(panel_file)) {
    # Back-compat: fall back to the pre-rev-3 single-cohort panel file
    legacy <- file.path(PANELS, "panel_halfyear.rds")
    if (file.exists(legacy)) {
        message("Using legacy panel_halfyear.rds (pre-cohort pipeline)")
        panel_file <- legacy
    } else {
        stop("Panel not found: ", panel_file)
    }
}
message("Using cohort panel: ", basename(panel_file))
panel <- readRDS(panel_file)

# Pool all 12 H1/H2 bins, summing person-halfyears at risk and DFU
# patient-bins. This gives prevalence per 1000 person-halfyears. Per-bin
# suppression is a privacy guardrail for the time-resolved EHSA inputs and
# bin-level displays; for the pooled descriptive map we sum the *full*
# panel and then re-apply suppression at the pooled scale (a much weaker
# filter, since pooling 12 bins lifts most rural ZCTAs above threshold).
pooled <- panel[, .(dm_person_halfyears  = sum(dm_denom),
                    dfu_person_halfyears = sum(dfu_num)),
                by = zcta]
pooled[, rate_per_1000 := 1000 * dfu_person_halfyears /
                          pmax(dm_person_halfyears, 1L)]
pooled[, suppressed := dfu_person_halfyears < 11 | dm_person_halfyears < 20]
pooled_active <- pooled[!(suppressed)]

# ---- ZCTA geometry ----------------------------------------------------------
# tigris::zctas(state=) is only valid for 2000/2010 vintages; for 2020 we
# pull the national ZCTA file and SPATIALLY clip to the Arkansas state
# polygon. A naive prefix filter on "71"/"72" wrongly includes Louisiana
# ZCTAs (LA = 700-714). We keep ZCTAs whose centroid lies inside AR.
ar_state <- states(progress_bar = FALSE)
ar_state <- ar_state[ar_state$STUSPS == "AR", ]
ar_state <- st_transform(ar_state, 5070)

ar_zctas <- zctas(year = 2020, progress_bar = FALSE)
ar_zctas$zcta <- ar_zctas$ZCTA5CE20
ar_zctas <- st_transform(ar_zctas, 5070)   # EPSG:5070 NAD83 Conus Albers
ar_centroids <- st_centroid(ar_zctas)
inside_ar    <- st_intersects(ar_centroids, ar_state, sparse = FALSE)[, 1]
ar_zctas     <- ar_zctas[inside_ar, ]

zcta_sf <- merge(ar_zctas, pooled_active, by = "zcta", all.x = FALSE)

message("ZCTAs with data (unsuppressed pooled): ", nrow(zcta_sf))

# ---- Spatial weights (adaptive KNN k = 8) -----------------------------------
# Built BEFORE EB smoothing because local EB needs the neighborhood structure.
coords <- st_centroid(zcta_sf) |> st_coordinates()
knn    <- knearneigh(coords, k = KNN_K)
nb     <- knn2nb(knn, sym = FALSE)
lw     <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ---- Empirical Bayes smoothing (local, Marshall 1991) -----------------------
# Raw rates from small-denominator ZCTAs are extremely noisy: a ZCTA with
# ~70 person-halfyears and 19 DFU produces rate=284/1000 purely from chance.
# Local EB shrinks each ZCTA's rate toward its NEIGHBORHOOD mean (not the
# global mean) by an amount proportional to its denominator-driven
# unreliability. Standard practice for small-area disease mapping
# (assumptions §9.6 — promoted from sensitivity to primary because raw-rate
# Gi* is dominated by small-N noise from rural ZCTAs). Local EB is preferred
# over global EB because it preserves spatial heterogeneity instead of
# pulling all ZCTAs toward the statewide mean.
eb_local <- EBlocal(zcta_sf$dfu_person_halfyears,
                    zcta_sf$dm_person_halfyears, nb)
zcta_sf$rate_raw <- zcta_sf$rate_per_1000           # keep raw for reference
zcta_sf$rate_per_1000 <- eb_local$est * 1000        # primary = local EB

# ---- Global Moran's I -------------------------------------------------------
set.seed(20260405)
moran_global <- moran.mc(zcta_sf$rate_per_1000, lw,
                         nsim = NPERM, zero.policy = TRUE)
print(moran_global)
saveRDS(moran_global, file.path(OUT_DIR,
        sprintf("global_morans_I_pooled_%s.rds", COHORT)))

# Sensitivity: Moran's I on raw (unsmoothed) rates for the supplement
set.seed(20260405)
moran_global_raw <- moran.mc(zcta_sf$rate_raw, lw,
                             nsim = NPERM, zero.policy = TRUE)
saveRDS(moran_global_raw,
        file.path(OUT_DIR,
                  sprintf("global_morans_I_pooled_rawrate_%s.rds", COHORT)))

# ---- Local Getis-Ord Gi* (permutation + FDR) --------------------------------
set.seed(20260405)
gi_perm <- localG_perm(zcta_sf$rate_per_1000, lw,
                       nsim = NPERM, zero.policy = TRUE)

zcta_sf$gi_z     <- as.numeric(gi_perm)
zcta_sf$gi_p     <- attr(gi_perm, "internals")[, "Pr(z != E(Gi))"]
zcta_sf$gi_p_fdr <- p.adjust(zcta_sf$gi_p, method = "BH")

zcta_sf$gi_bin <- with(as.data.frame(zcta_sf), fcase(
    gi_p_fdr >= FDR_ALPHA,          "Not significant",
    gi_z >=  2.58,                   "Hot 99%",
    gi_z >=  1.96 & gi_z < 2.58,     "Hot 95%",
    gi_z >=  1.65 & gi_z < 1.96,     "Hot 90%",
    gi_z <= -2.58,                   "Cold 99%",
    gi_z <= -1.96 & gi_z > -2.58,    "Cold 95%",
    gi_z <= -1.65 & gi_z > -1.96,    "Cold 90%",
    default = "Not significant"
))
zcta_sf$gi_bin <- factor(zcta_sf$gi_bin, levels = c(
    "Cold 99%", "Cold 95%", "Cold 90%",
    "Not significant",
    "Hot 90%", "Hot 95%", "Hot 99%"
))

saveRDS(zcta_sf, file.path(OUT_DIR,
        sprintf("zcta_sf_gi_pooled_%s.rds", COHORT)))

# ---- LISA (Local Moran's I) sensitivity -------------------------------------
set.seed(20260405)
lisa <- localmoran_perm(zcta_sf$rate_per_1000, lw,
                        nsim = NPERM, zero.policy = TRUE)
zcta_sf$lisa_I   <- lisa[, "Ii"]
zcta_sf$lisa_z   <- lisa[, "Z.Ii"]
zcta_sf$lisa_p   <- lisa[, "Pr(z != E(Ii))"]
zcta_sf$lisa_p_fdr <- p.adjust(zcta_sf$lisa_p, method = "BH")

mean_rate <- mean(zcta_sf$rate_per_1000, na.rm = TRUE)
lagged    <- lag.listw(lw, zcta_sf$rate_per_1000, zero.policy = TRUE)
zcta_sf$lisa_quad <- with(as.data.frame(zcta_sf), fcase(
    lisa_p_fdr >= FDR_ALPHA, "NS",
    rate_per_1000 >= mean_rate & lagged >= mean_rate, "High-High",
    rate_per_1000 <  mean_rate & lagged <  mean_rate, "Low-Low",
    rate_per_1000 >= mean_rate & lagged <  mean_rate, "High-Low",
    rate_per_1000 <  mean_rate & lagged >= mean_rate, "Low-High"
))
saveRDS(zcta_sf, file.path(OUT_DIR,
        sprintf("zcta_sf_lisa_pooled_%s.rds", COHORT)))

# ---- Maps -------------------------------------------------------------------
theme_map <- theme_void(base_size = 11) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"))

# Raw rate
p_rate <- ggplot(zcta_sf) +
    geom_sf(aes(fill = rate_per_1000), color = "grey40", linewidth = 0.1) +
    scale_fill_viridis(option = "C", name = "DFU per 1,000\nperson-halfyears") +
    labs(title = "Crude DFU prevalence, AR ZCTAs, 2017-2022 (pooled)",
         caption = "Tier 2 primary; ambiguous-DM excluded; cells with <11 cases or <20 DM suppressed") +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_rate_pooled_%s.png", COHORT)),
       p_rate, width = 8, height = 7, dpi = 300)

# Gi* significance bins
gi_palette <- c(
    "Cold 99%" = "#053061", "Cold 95%" = "#2166ac", "Cold 90%" = "#4393c3",
    "Not significant" = "grey85",
    "Hot 90%"  = "#f4a582", "Hot 95%"  = "#d6604d", "Hot 99%"  = "#b2182b"
)
p_gi <- ggplot(zcta_sf) +
    geom_sf(aes(fill = gi_bin), color = "grey40", linewidth = 0.1) +
    scale_fill_manual(values = gi_palette, name = "Gi* (FDR)") +
    labs(title = "Local Getis-Ord Gi* hotspot analysis, AR DFU 2017-2022",
         caption = "Adaptive KNN k=8; 999 permutations; BH-FDR q<0.05") +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_gi_pooled_%s.png", COHORT)),
       p_gi, width = 8, height = 7, dpi = 300)

# LISA quadrants
lisa_palette <- c("High-High" = "#b2182b", "Low-Low" = "#2166ac",
                  "High-Low" = "#f4a582", "Low-High" = "#92c5de",
                  "NS" = "grey85")
p_lisa <- ggplot(zcta_sf) +
    geom_sf(aes(fill = lisa_quad), color = "grey40", linewidth = 0.1) +
    scale_fill_manual(values = lisa_palette, name = "LISA (FDR)") +
    labs(title = "Local Moran's I (LISA), AR DFU 2017-2022 (sensitivity)",
         caption = "Adaptive KNN k=8; 999 permutations; BH-FDR q<0.05") +
    theme_map
ggsave(file.path(OUT_DIR, sprintf("map_lisa_pooled_%s.png", COHORT)),
       p_lisa, width = 8, height = 7, dpi = 300)

message("\n05_static_spatial.R complete.")
message("Global Moran's I = ", round(moran_global$statistic, 4),
        " (p = ", round(moran_global$p.value, 4), ")")
message("Outputs written to: ", OUT_DIR)
