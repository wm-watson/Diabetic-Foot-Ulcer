library(tidyverse)

#Data----
dm_summary <- read_csv("C:/Users/watso/Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))

#1175067 observations, 13006 unique IDS, 1174501 claims


# ## T1D----
# t1d <- dm_summary %>% 
#   filter(grepl("^E10", CODEVALUE))

## T2D----
# Filter to include only Type 2 Diabetes (T2D) cases 1028718 observationss, 125412 unique ids
t2d <- dm_summary %>% 
  filter(grepl("^E11", CODEVALUE))

# Filter to include only Arkansas (AR) cases 1008436, 121502 Unique IDS, 1007905 claims
t2d_AR <- t2d %>% 
  filter(STATE == "AR")

# Calculate total diabetics by county 121502 diabetics; 121, 497 without missing county
total_diabetes_type2 <- t2d_AR %>%
  group_by(COUNTYNAME) %>%
  summarize(total_diabetes_type2 = n_distinct(Unique_ID), .groups = "drop") %>% 
  filter(!is.na(COUNTYNAME))

# Calculate DFU cases by county 3468 DFUs
dfu_summary <- t2d_AR %>%
  filter(DFU > 0) %>%  # Assuming DFU > 0 indicates a case of DFU
  group_by(COUNTYNAME) %>%
  summarize(total_dfu_cases = n_distinct(Unique_ID), .groups = "drop") %>% 
  filter(!is.na(COUNTYNAME))

# Join the total diabetes and DFU summaries
rate_data <- total_diabetes_type2 %>%
  left_join(dfu_summary, by = "COUNTYNAME") %>%
  replace_na(list(total_dfu_cases = 0)) %>%
  mutate(
    DFU_per_1000 = (total_dfu_cases / total_diabetes_type2) * 1000,
    DFU_per_10000 = (total_dfu_cases / total_diabetes_type2) * 10000,
    DFU_per_100000 = (total_dfu_cases / total_diabetes_type2) * 100000
  )

# Remove rows with missing county names
ar_rates <- rate_data %>% 
  filter(!is.na(COUNTYNAME))

# Calculate deciles for DFU per 1000
ar_rates <- ar_rates %>%
  mutate(DFU_per_1000_decile = ntile(DFU_per_1000, 10))

# Calculate mean, SD, and CoV of counties using the aggregated data
county_stats_within <- ar_rates %>%
  group_by(DFU_per_1000_decile) %>%
  summarise(
    mean_value = mean(DFU_per_1000, na.rm = TRUE),
    sd_value = sd(DFU_per_1000, na.rm = TRUE),
    n = n(),  
    CoV_within_decile = (sd_value / mean_value) * 100,  
    .groups = "drop"
  )

# # Calculate mean, SD, and CoV between counties using the aggregated data
# county_stats_between <- ar_rates %>%
#   group_by(CoV_within_decile) %>%
#   summarise(
#     mean_value = mean(DFU_per_1000, na.rm = TRUE),
#     sd_value = sd(DFU_per_1000, na.rm = TRUE),
#     n = n(),  
#     CoV_between = (sd_value / mean_value) * 100,  
#     .groups = "drop"
#   )


# Assign CoV decile groups between counties
county_stats_within <- county_stats_within %>%
  mutate(cov_between_decile = ntile(CoV_within_decile, 10))

# Merge the between-county CoV back to the original data
ar_rates <- ar_rates %>%
  left_join(county_stats_within, by = "DFU_per_1000_decile")

# Prepare a detailed summary that includes between deciles
decile_summary_between <- ar_rates %>%
  group_by(cov_between_decile) %>%
  summarise(
    average_CoV_between = mean(CoV_within_decile),
    min_CoV_between = min(CoV_within_decile),
    max_CoV_between = max(CoV_within_decile),
    .groups = "drop"
  )

# write.csv(ar_rates, file = "C:/Users/watso/Box/PhD/GIS/DFU/dfu_rates.csv",
#            col.names = TRUE)
##Plots----
## Sort data by DFU_per_1000 in descending order
rate_data_sorted <- ar_rates %>%
  arrange(desc(DFU_per_1000))

## Plotting the data
### Unique Colors DFU/1,000----
ggplot(rate_data_sorted, aes(x = reorder(COUNTYNAME, -DFU_per_1000), y = DFU_per_1000, fill = COUNTYNAME)) +
  geom_bar(stat = "identity") +
  labs(title = "DFU per 1000 Diabetics by County",
       x = "County",
       y = "DFU per 1000 Diabetics") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_viridis_d(direction = -1)  # Reverse the color scale

### Unique Colors DFU_Decile/1,000----
rate_data_sorted <- rate_data_sorted[order(-rate_data_sorted$DFU_per_1000), ]
rate_data_sorted$DFU_per_1000_decile <- factor(rate_data_sorted$DFU_per_1000_decile, 
                                               levels = unique(rate_data_sorted$DFU_per_1000_decile))

# Plotting
ggplot(rate_data_sorted, aes(x = reorder(COUNTYNAME, -DFU_per_1000), y = DFU_per_1000, fill = DFU_per_1000_decile)) +
  geom_bar(stat = "identity") +
  labs(title = "DFU Decile per 1000 Diabetics by County",
       x = "County",
       y = "DFU per 1000 Diabetics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_viridis_d(direction = 1) 


### Deciles ----
#### DFU Rate
decile_averages <- rate_data_sorted %>%
  group_by(DFU_per_1000_decile) %>%
  summarize(average_DFU_per_1000 = mean(DFU_per_1000), .groups = "drop")

### Create a vector of colors
colors <- viridis::viridis(10, direction = -1)  # Reverse the color scale


# Aggregate the data to compute the mean DFU rate per decile
aggregated_data <- rate_data_sorted %>%
  group_by(DFU_per_1000_decile) %>%
  summarize(mean_value = mean(DFU_per_1000, na.rm = TRUE)) %>%
  ungroup()

# Ensure the DFU_per_1000_decile column is a factor and ordered by decile
aggregated_data$DFU_per_1000_decile <- factor(aggregated_data$DFU_per_1000_decile, 
                                              levels = sort(unique(aggregated_data$DFU_per_1000_decile), decreasing = TRUE))

# Plotting the averages for each decile with the 10th decile on the far right and the 1st on the far left
ggplot(aggregated_data, aes(x = DFU_per_1000_decile, y = mean_value, fill = DFU_per_1000_decile)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", mean_value)), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Average DFU per 1000 Diabetics by Decile",
       x = "Decile",
       y = "Average DFU per 1000 Diabetics") +
  scale_fill_viridis_d(direction = -1) +  # Use viridis color scale with reversed direction
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


### VoC----
### Create a vector of colors
colors <- viridis::viridis(10, direction = -1) 

# Calculate CoV for each decile
voc_decile <- ar_rates %>%
  group_by(cov_between_decile) %>%
  summarize(mean_value = mean(DFU_per_1000, na.rm = TRUE),
            sd_value = sd(DFU_per_1000, na.rm = TRUE),
            n = n(),
            CoV_between = (sd_value / mean_value) * 100,
            .groups = 'drop') %>%
  arrange(desc(CoV_between)) %>%
  mutate(cov_between_decile = factor(cov_between_decile, levels = cov_between_decile))

# Ensure the decile factor is ordered from 1st to 10th
voc_decile$cov_between_decile <- factor(voc_decile$cov_between_decile, 
                                        levels = sort(unique(voc_decile$cov_between_decile), decreasing = TRUE))

# Plotting CoV averages for each decile with the 10th decile on the far right
ggplot(voc_decile, aes(x = cov_between_decile, y = CoV_between, fill = cov_between_decile)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", CoV_between)), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Average CoV Between AR Counties by Decile",
       x = "Decile",
       y = "CoV Between Decile") +
  scale_fill_viridis_d(direction = -1) +  # Use viridis color scale with reversed direction for darkest color on the highest decile
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Cartography ----

library(sf)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(ggthemes)
library(ggalt)
library(maps)
library(rgeos)
library(maptools)
library(albersusa)
library(grid)

## DFU per 1,000 ----
# Shapefile
gdf <- st_read("C:/Users/watso/Box/PhD/GIS/DFU/COUNTY_BOUNDARY/COUNTY_BOUNDARY.shp")



 write.csv(ar_rates, file = "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/ar_rates.csv",
            row.names = FALSE)
 
 gdf <- gdf %>%
   mutate(COUNTY = toupper(COUNTY),
          COUNTY = gsub("\\.", "", COUNTY))
 
 merged_data <- gdf %>%
   left_join(ar_rates, by = c("COUNTY" = "COUNTYNAME"))
 

 
 # Calculate centroids for text labels
 centroids <- st_centroid(merged_data)
 
 # Merge centroid coordinates back with the original data for labeling
 merged_data <- merged_data %>%
   mutate(centroid_x = st_coordinates(centroids)[,1],
          centroid_y = st_coordinates(centroids)[,2])
 
 # Generate a diverging color palette with 10 distinct colors
 decile_colors <- brewer.pal(10, "Spectral")
 
 # Reverse the palette to go from light to dark
 decile_colors <- rev(decile_colors)
 
 # Create the choropleth map
 gmap <- ggplot(merged_data) +
   geom_sf(aes(fill = factor(DFU_per_1000_decile))) +
   geom_text(aes(x = centroid_x, y = centroid_y,
                 label = ifelse(!is.na(DFU_per_1000), round(DFU_per_1000, 1), "")),
             size = 3, color = "black") +
   theme_void() +
   theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10))) +
   theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
   theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
   theme(legend.position = "right",
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8)) +
   scale_fill_manual(name = "DFU per 1000 Decile",
                     values = decile_colors,
                     labels = paste0("Decile ", 1:10),
                     guide = guide_legend(reverse = TRUE)) +
   labs(caption = "Author: Wm. Watson",
        title = "DFUs per 1,000 Diabetics by Decile")
 
 # Print the map
 print(gmap)
 
 ##COV Map----
 # Generate a diverging color palette with 10 distinct colors from RdYlBu
 decile_colors_cov <- brewer.pal(10, "RdYlBu")
 
 # Reverse the palette to go from light to dark
 decile_colors_cov <- rev(decile_colors_cov)
 
 
 # Calculate centroids for text labels
 centroids <- st_centroid(merged_data)
 
 # Merge centroid coordinates back with the original data for labeling
 merged_data <- merged_data %>%
   mutate(centroid_x_1 = st_coordinates(centroids)[, 1],
          centroid_y_1 = st_coordinates(centroids)[, 2])
 
 # Create the choropleth map
 gmap_cov <- ggplot(merged_data) +
   geom_sf(aes(fill = factor(cov_between_decile))) +
   geom_text(aes(x = centroid_x_1, y = centroid_y_1,
                 label = ifelse(!is.na(CoV_within_decile), round(CoV_within_decile, 1), "")),
             size = 3, color = "black") +
   theme_void() +
   theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10))) +
   theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
   theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
   theme(legend.position = "right",
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8)) +
   scale_fill_manual(name = "CoV Between Decile",
                     values = decile_colors_cov,
                     labels = paste0("Decile ", 1:10),
                     guide = guide_legend(reverse = TRUE)) +
   labs(caption = "Author: Wm. Watson",
        title = "CoV Within DFU Rate Deciles")
 
 # Print the map
 print(gmap_cov)
 
 #Tables----
 library(knitr)
 library(kableExtra)
 library(dplyr)
 
 # Create the summary table, explicitly excluding the 'geometry' column
 summary_table <- merged_data %>%
   st_drop_geometry() %>%  # This function drops the geometry column from the sf object
   select(COUNTY, DFU_per_1000, DFU_per_1000_decile, CoV_within_decile, cov_between_decile) %>%
   arrange(COUNTY)
 
 # Save the summary table to a CSV file with the specified path
 write.csv(summary_table, 
           "C:/Users/watso/Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/county_summary_table.csv",
           row.names = FALSE)
 
 