library(tidyverse)

#Data----
dm_summary <- read_csv("C:/Users/watso/Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))

## T1D----
t1d <- dm_summary %>% 
  filter(grepl("^E10", CODEVALUE))

## T2D----
t2d <- dm_summary %>% 
  filter(grepl("^E11", CODEVALUE))

#AR Only
t2d_AR <- t2d %>% 
  filter(STATE == "AR")

### Calculate diabetics by county----
total_diabetes_type2 <- t2d_AR %>%
  filter(grepl("^E11", CODEVALUE)) %>%
  group_by(COUNTYNAME) %>%
  summarize(total_diabetes_type2 = n_distinct(Unique_ID), .groups = "drop")

### DFU by county----
dfu_summary <- t2d_AR %>%
  filter(DFU > 0) %>%  # Assuming DFU > 0 indicates a case of DFU
  group_by(COUNTYNAME) %>%
  summarize(total_dfu_cases = n_distinct(Unique_ID), .groups = "drop")

### Join back to original dataset----
rate_data <- total_diabetes_type2 %>%
  left_join(dfu_summary, by = "COUNTYNAME") %>%
  replace_na(list(total_dfu_cases = 0)) %>%
  mutate(
    DFU_per_1000 = (total_dfu_cases / total_diabetes_type2) * 1000,
    DFU_per_10000 = (total_dfu_cases / total_diabetes_type2) * 10000,
    DFU_per_100000 = (total_dfu_cases / total_diabetes_type2) * 100000
  )

# Get rid of missing
ar_rates <- rate_data %>% 
  filter(!is.na(COUNTYNAME))

###Deciles----
# Calculate deciles for DFU per 1000
ar_rates <- ar_rates %>%
  mutate(
    DFU_per_1000_decile = ntile(DFU_per_1000, 10)  # Calculate deciles
  )

### CoV----
# Calculate mean, SD, and CoV for each decile
county_stats <- ar_rates %>%
  group_by(DFU_per_1000_decile) %>%
  summarise(
    mean_value = mean(DFU_per_1000, na.rm = TRUE),
    sd_value = sd(DFU_per_1000, na.rm = TRUE),
    n = n(),  # Number of observations per decile
    CoV = (sd_value / mean_value) * 100,  # Compute CoV directly here
    .groups = "drop"
  )

# Assign CoV decile groups
county_stats <- county_stats %>%
  mutate(cov_decile = ntile(CoV, 10))

# Prepare a detailed summary that includes both deciles
decile_summary <- county_stats %>%
  group_by(cov_decile) %>%
  summarise(
    average_CoV = mean(CoV),
    min_CoV = min(CoV),
    max_CoV = max(CoV),
    .groups = "drop"
  )

# Merge back to the original data to keep track of county details
ar_rates_detailed <- ar_rates %>%
  left_join(county_stats, by = "DFU_per_1000_decile") %>%
  select(county = COUNTYNAME, DFU_per_1000, DFU_per_1000_decile, cov_decile)

# Print the detailed data frame
print(ar_rates_detailed)

# Print the decile summary
print(decile_summary)

write.csv(ar_rates_detailed, file = "C:/Users/watso/Box/PhD/GIS/DFU/dfu_rates.csv",
          col.names = TRUE)
##Plots----
### Sort data by DFU_per_1000 in descending order
rate_data_sorted <- ar_rates %>%
  arrange(desc(DFU_per_1000))

### Plotting the data
### Unique Colors----
ggplot(rate_data_sorted, aes(x = reorder(COUNTYNAME, -DFU_per_1000), y = DFU_per_1000, fill = COUNTYNAME)) +
  geom_bar(stat = "identity") +
  labs(title = "DFU per 1000 Diabetics by County",
       x = "County",
       y = "DFU per 1000 Diabetics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_viridis_d() 

### Deciles----
library(viridis)
####DFU Rate----
decile_averages <- rate_data_sorted %>%
  group_by(DFU_per_1000_decile) %>%
  summarize(average_DFU_per_1000 = mean(DFU_per_1000), .groups = "drop")

### Create a vector of colors
colors <- viridis::viridis(10)

#Redorder
decile_averages <- decile_averages %>%
  mutate(DFU_per_1000_decile = factor(DFU_per_1000_decile, levels = rev(unique(DFU_per_1000_decile))))

### Plotting the averages for each decile with custom legend labels
ggplot(decile_averages, aes(x = DFU_per_1000_decile, y = average_DFU_per_1000, fill = DFU_per_1000_decile)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", average_DFU_per_1000)), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Average DFU per 1000 Diabetics by Decile",
       x = "Decile",
       y = "Average DFU per 1000 Diabetics") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

### VoC----

### Create a vector of colors
colors <- viridis::viridis(10)

#Redorder
voc_averages <- decile_summary %>%
  mutate(cov_decile = factor(cov_decile, levels = unique(cov_decile[order(-average_CoV)])))

# Plotting the averages for each decile
ggplot(voc_averages, aes(x = cov_decile, y = average_CoV, fill = cov_decile)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", average_CoV)), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Average CoV by Decile",
       x = "Decile",
       y = "Average CoV") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
