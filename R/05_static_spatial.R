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
panel <- readRDS(file.path(PANELS, "panel_halfyear.rds"))

# Pool all 12 H1/H2 bins: patient-bins must not be double-counted as patients
# in the pooled count, so we take max(dm_denom, dfu_num) across bins per ZCTA
# when pooling -- but for a descriptive pooled rate we use SUM of patient-bins
# as person-halfyears at risk. This gives prevalence per 1000 person-halfyears.
pooled <- panel[!(suppressed),
                .(dm_person_halfyears = sum(dm_denom),
                  dfu_person_halfyears = sum(dfu_num)),
                by = zcta]
pooled[, rate_per_1000 := 1000 * dfu_person_halfyears /
                          pmax(dm_person_halfyears, 1L)]

# Re-apply suppression at pooled level (should be rare after pooling)
pooled[, suppressed := dfu_person_halfyears < 11 | dm_person_halfyears < 20]
pooled_active <- pooled[!(suppressed)]

# ---- ZCTA geometry ----------------------------------------------------------
# tigris::zctas(state=) is only valid for 2000/2010 vintages; for 2020 we
# pull the national ZCTA file and subset to Arkansas ZCTAs (prefix 71/72).
ar_zctas <- zctas(year = 2020, progress_bar = FALSE)
ar_zctas$zcta <- ar_zctas$ZCTA5CE20
ar_zctas <- ar_zctas[substr(ar_zctas$zcta, 1, 2) %in% c("71", "72"), ]
ar_zctas <- st_transform(ar_zctas, 5070)   # EPSG:5070 NAD83 Conus Albers

zcta_sf <- merge(ar_zctas, pooled_active, by = "zcta", all.x = FALSE)

message("ZCTAs with data (unsuppressed pooled): ", nrow(zcta_sf))

# ---- Spatial weights (adaptive KNN k = 8) -----------------------------------
coords <- st_centroid(zcta_sf) |> st_coordinates()
knn    <- knearneigh(coords, k = KNN_K)
nb     <- knn2nb(knn, sym = FALSE)
lw     <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ---- Global Moran's I -------------------------------------------------------
set.seed(20260405)
moran_global <- moran.mc(zcta_sf$rate_per_1000, lw,
                         nsim = NPERM, zero.policy = TRUE)
print(moran_global)
saveRDS(moran_global, file.path(OUT_DIR, "global_morans_I_pooled.rds"))

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

saveRDS(zcta_sf, file.path(OUT_DIR, "zcta_sf_gi_pooled.rds"))

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
saveRDS(zcta_sf, file.path(OUT_DIR, "zcta_sf_lisa_pooled.rds"))

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
ggsave(file.path(OUT_DIR, "map_rate_pooled.png"),
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
ggsave(file.path(OUT_DIR, "map_gi_pooled.png"),
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
ggsave(file.path(OUT_DIR, "map_lisa_pooled.png"),
       p_lisa, width = 8, height = 7, dpi = 300)

message("\n05_static_spatial.R complete.")
message("Global Moran's I = ", round(moran_global$statistic, 4),
        " (p = ", round(moran_global$p.value, 4), ")")
message("Outputs written to: ", OUT_DIR)
