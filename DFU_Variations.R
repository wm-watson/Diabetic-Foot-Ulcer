library(tidyverse)

#Data----
dm_summary <- read_csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))

# ## T1D----
# t1d <- dm_summary %>% 
#   filter(grepl("^E10", CODEVALUE))

## T2D----
# Filter to include only Type 2 Diabetes (T2D) cases
t2d <- dm_summary %>% 
  filter(grepl("^E11", CODEVALUE))

# Filter to include only Arkansas (AR) cases
t2d_AR <- t2d %>% 
  filter(STATE == "AR")

# Calculate total diabetics by county
total_diabetes_type2 <- t2d_AR %>%
  group_by(COUNTYNAME) %>%
  summarize(total_diabetes_type2 = n_distinct(Unique_ID), .groups = "drop")

# Calculate DFU cases by county
dfu_summary <- t2d_AR %>%
  filter(DFU > 0) %>%  # Assuming DFU > 0 indicates a case of DFU
  group_by(COUNTYNAME) %>%
  summarize(total_dfu_cases = n_distinct(Unique_ID), .groups = "drop")

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

# Calculate mean, SD, and CoV between counties using the aggregated data
county_stats_between <- county_stats_between %>%
  group_by(CoV_b) %>%
  summarise(
    mean_value = mean(DFU_per_1000, na.rm = TRUE),
    sd_value = sd(DFU_per_1000, na.rm = TRUE),
    n = n(),  
    CoV_between = (sd_value / mean_value) * 100,  
    .groups = "drop"
  )


# Assign CoV decile groups between counties
county_stats_within <- county_stats_within %>%
  mutate(cov_between_decile = ntile(CoV_within_decile, 10))

# Merge the between-county CoV back to the original data
ar_rates <- ar_rates %>%
  left_join(county_stats_within, by = "DFU_per_1000_decile")

# Prepare a detailed summary that includes between deciles
decile_summary_between <- county_stats_within %>%
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
                                              levels = sort(unique(aggregated_data$DFU_per_1000_decile)))

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

## DFU per 1,000 ----
# Shapefile
gdf <- st_read('/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/GIS/DFU/COUNTY_BOUNDARY/COUNTY_BOUNDARY.shp')



 write.csv(ar_rates, file = "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/ar_rates.csv",
            row.names = FALSE)