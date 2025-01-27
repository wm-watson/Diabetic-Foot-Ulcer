library(tidyverse)

#Data----
dm_summary <- read_csv("C:/Users/watso/Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))

#1175067 observations, 13006 unique IDS, 1174501 claims

## T2D----
# Filter to include only Type 2 Diabetes (T2D) cases 1028718 observationss, 125412 unique ids
t2d <- dm_summary %>% 
  filter(grepl("^E11", CODEVALUE))

# Filter to include only Arkansas (AR) cases 1008436, 121502 Unique IDS, 1007905 claims
t2d_AR <- t2d %>% 
  filter(STATE == "AR")