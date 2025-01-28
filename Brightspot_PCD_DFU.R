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

## Map Zip to census code----
library(tidycensus)

#Census API Key for pulling in Census data
census_api_key("ce88034325910cc3e5766b8f7b5636882aca39bd", install = TRUE)

#Get range of time
early <- t2d_AR %>% 
  arrange(desc(Year),desc(Month)) %>% 
  slice(1)
view(early)

#Filter out > 2023 119,996 unique IDS, 9947321
t2d_AR <- t2d_AR %>% 
  filter(Year <= 2023 & Month <= 12)

## Combine zip to census tract---- 
library(dplyr)
library(readxl)
library(stringr)

# Define the directory where your files are located
file_directory <- "C:/Users/watso/Box/PhD/PCD - Scientific Writing/Data"

# List all Excel files in the directory with the pattern ZIP_TRACT_MMYYYY.xlsx
file_list <- list.files(path = file_directory, pattern = "ZIP_TRACT_\\d{6}\\.xlsx", full.names = TRUE)
file_list <- unique(trimws(file_list))  # Remove duplicates and trim spaces

# Print the list of files to verify they are being identified correctly
print("List of files to process:")
print(file_list)

# Function to extract year and month from the filename and determine the quarter
extract_year_quarter <- function(filename) {
  # Extract the MMYYYY part from the filename
  date_part <- str_extract(filename, "\\d{6}")
  
  # Extract month and year
  month <- as.numeric(substr(date_part, 1, 2))
  year <- as.numeric(substr(date_part, 3, 6))
  
  # Determine the quarter based on the month
  quarter <- case_when(
    month >= 1 & month <= 3 ~ 1,
    month >= 4 & month <= 6 ~ 2,
    month >= 7 & month <= 9 ~ 3,
    month >= 10 & month <= 12 ~ 4
  )
  
  return(list(year = year, quarter = quarter))
}

# Initialize an empty list to store individual data frames
data_list <- list()

# Loop through each file, read it, add Year and Quarter columns, and keep only ZIP and TRACT
for (file in file_list) {
  tryCatch({
    print(paste("Processing file:", file))
    
    # Extract year and quarter from the filename
    year_quarter <- extract_year_quarter(file)
    print(paste("Year:", year_quarter$year, "Quarter:", year_quarter$quarter))
    
    # Read the Excel file
    data <- read_excel(file)
    print(paste("Number of rows:", nrow(data)))
    
    # Standardize column names to uppercase
    colnames(data) <- toupper(colnames(data))
    
    # Print the column names to verify the data is being read correctly
    print("Columns in the file:")
    print(colnames(data))
    
    # Skip empty files
    if (nrow(data) > 0) {
      # Add Year and Quarter columns and keep only ZIP and TRACT
      data <- data %>%
        mutate(Year = year_quarter$year,
               Quarter = year_quarter$quarter) %>%
        select(ZIP, TRACT, Year, Quarter)
      
      # Append the modified data frame to the list
      data_list <- append(data_list, list(data))
    } else {
      message("Skipping empty file: ", file)
    }
  }, error = function(e) {
    message("Error reading file: ", file, "\n", e$message)
  })
}

# Combine all data frames into one
combined_data <- bind_rows(data_list)

# Ensure ZIP codes are in the same format (uppercase) in both datasets
combined_data <- combined_data %>%
  mutate(ZIP = toupper(ZIP))

# Convert Month in t2d_AR to Quarter
t2d_AR <- t2d_AR %>%
  mutate(
    Quarter = case_when(
      Month >= 1 & Month <= 3 ~ 1,
      Month >= 4 & Month <= 6 ~ 2,
      Month >= 7 & Month <= 9 ~ 3,
      Month >= 10 & Month <= 12 ~ 4
    )
  )

# Ensure Year and Quarter columns are consistent
t2d_AR <- t2d_AR %>%
  mutate(
    Year = as.numeric(Year),
    Quarter = as.numeric(Quarter)
  )

library(dplyr)

# Pull row 37 from x
row_37_x <- t2d_AR %>% slice(37)

# Pull row 2012214 from y
row_2012214_y <- combined_data %>% slice(2012214)

# Print the rows
print(row_37_x)
print(row_2012214_y)

# Unique rows
t2d_AR <-  t2d_AR %>%
  distinct()

#Unique Rows

combined_data <- combined_data %>%
  distinct(ZIP, Year, Quarter, .keep_all = TRUE)


# Join the datasets on ZIP code, Year, and Quarter
final_data <- t2d_AR %>%
  left_join(combined_data, by = c("PAT_ZIP_5" = "ZIP", "Year" = "Year", "Quarter" = "Quarter"))

# Write the data to a CSV file
write_csv(final_data, "final_data.csv")
