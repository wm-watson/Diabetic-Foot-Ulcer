library(tidyverse)

#Data----
dm_summary <- read_csv("C:/Users/1187507/Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))

## T1D----
t1d <- dm_summary %>% 
  filter(grepl("^E10", CODEVALUE))

## T2D----
t2d <- dm_summary %>% 
  filter(grepl("^E11", CODEVALUE))