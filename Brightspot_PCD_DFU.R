library(tidyverse)
library(vroom)
library(data.table)
library(beepr)


<<<<<<< HEAD
# 1. Raw Data----
##A. Data----
# File paths
claims_directory <- "C:/Users/watso/OneDrive - University of Arkansas for Medical Sciences/Deductible_Project/Deductibles/Data/"
output_directory <- "C:/Users/watso/Box/PhD/PCD - Scientific Writing/Data/"
=======
#Data----
dm_summary <- read_csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/prov_pat_zip.csv",
                       col_types = cols(
                         PAT_ZIP_5 = col_character()  # Adjust accordingly if there are more columns
                       ))
>>>>>>> 8004542a24eaebf8951aa3b466ca778b010faef9

# ICD-10 codes for Type 2 Diabetes
t2d_codes <- c("E110", "E111", "E112", "E113", "E114", "E115", 
               "E116", "E117", "E118", "E119", "E11")

# Initialize tracking dataframe
tracking_counts <- data.frame(
  year = 2016:2023,
  total_mvdids = 0,
  t2d_mvdids = 0,
  total_claims = 0,
  t2d_claims = 0
)

# Create temporary file for T2D claims
temp_file <- paste0(output_directory, "temp_t2d_claims.csv")
file.create(temp_file)

# Process each year
for(year in 2016:2023) {
  print(paste("Processing", year))
  
  filepath <- paste0(claims_directory, "claims_", year, ".csv")
  
  # Get column names
  cols <- names(vroom(filepath, n_max = 1))
  dx_cols <- cols[grep("^DX", cols)]
  select_cols <- c("MVDID", dx_cols)
  
  # Read the file in chunks using fread
  chunk_size <- 1e6
  # Count total rows (subtract 1 for header)
  total_rows <- as.numeric(count_fields(filepath, tokenizer = tokenizer_csv())[1]) - 1
  n_chunks <- ceiling(total_rows / chunk_size)
  
  for(chunk in 1:n_chunks) {
    skip_rows <- (chunk - 1) * chunk_size + 1  # Add 1 to skip header
    
    # Read chunk
    if(chunk == n_chunks) {
      x <- fread(filepath, 
                 select = select_cols,
                 skip = skip_rows,
                 nrows = total_rows - (chunk - 1) * chunk_size)
    } else {
      x <- fread(filepath, 
                 select = select_cols,
                 skip = skip_rows,
                 nrows = chunk_size)
    }
    
    # Clean diagnosis codes
    for(col in dx_cols) {
      set(x, j = col, value = gsub("[. ]", "", x[[col]]))
    }
    
    # Find T2D diagnoses
    t2d_rows <- x[Reduce(`|`, lapply(dx_cols, function(col) x[[col]] %in% t2d_codes))]
    
    # Update tracking counts
    tracking_counts[tracking_counts$year == year, "total_claims"] <- 
      tracking_counts[tracking_counts$year == year, "total_claims"] + nrow(x)
    
    tracking_counts[tracking_counts$year == year, "total_mvdids"] <- 
      tracking_counts[tracking_counts$year == year, "total_mvdids"] + uniqueN(x$MVDID)
    
    if(nrow(t2d_rows) > 0) {
      tracking_counts[tracking_counts$year == year, "t2d_claims"] <- 
        tracking_counts[tracking_counts$year == year, "t2d_claims"] + nrow(t2d_rows)
      
      tracking_counts[tracking_counts$year == year, "t2d_mvdids"] <- 
        tracking_counts[tracking_counts$year == year, "t2d_mvdids"] + uniqueN(t2d_rows$MVDID)
      
      # Append T2D claims to temporary file
      fwrite(t2d_rows, temp_file, append = TRUE)
    }
    
    # Print progress
    print(paste("Processed chunk", chunk, "of", n_chunks, "for year", year))
  }
}

<<<<<<< HEAD
# Read and process final T2D claims
t2d_claims_raw <- fread(temp_file)
=======
#Census API Key for pulling in Census data
census_api_key("XXXXX", install = TRUE)
>>>>>>> 8004542a24eaebf8951aa3b466ca778b010faef9

# Clean up temporary file
unlink(temp_file)

# Save results
fwrite(t2d_claims_raw, paste0(output_directory, "t2d_claims_raw.csv"))
fwrite(as.data.table(tracking_counts), paste0(output_directory, "t2d_tracking_counts.csv"))

# Print summary
print("Processing complete. Summary by year:")
print(tracking_counts)

# Print overall summary
total_summary <- data.table(tracking_counts)[, .(
  total_years = uniqueN(year),
  total_mvids = sum(total_mvdids),
  unique_t2d_mvids = uniqueN(t2d_claims_raw$MVDID),
  total_claims = sum(total_claims),
  t2d_claims = sum(t2d_claims),
  t2d_mvid_pct = round(100 * uniqueN(t2d_claims_raw$MVDID) / sum(total_mvdids), 2),
  t2d_claims_pct = round(100 * sum(t2d_claims) / sum(total_claims), 2)
)]

print("\nOverall Summary:")
print(total_summary)
beep(8)

## B. Census Tract Data---- 
library(dplyr)
library(readxl)
library(stringr)

# Define the directory where your files are located
file_directory <- "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/PCD - Scientific Writing/Data"

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

final_data <- final_data %>% select(-Season, -Zip_diab_pt_seen, -tot_year_diab, -Zip_Pat_from_year)

# Write the data to a CSV file
write_csv(final_data, "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/final_data.csv")

#Read in
final_data <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/final_data.csv",
                       header = TRUE, sep = ",")

## Crosswalk of Census Tracts 2010 to 2020
census_crosswalk <- read_excel("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/PCD - Scientific Writing/Data/CENSUS_TRACT_CROSSWALK_2010_to_2020_2019.xlsx")

r

# Libraries
library(readr)
library(dplyr)
library(readxl)
library(tidyr)

# Read in data
final_data <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/final_data.csv",
                       header = TRUE, sep = ",")

## Crosswalk of Census Tracts 2010 to 2020
census_crosswalk <- read_excel("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/PCD - Scientific Writing/Data/CENSUS_TRACT_CROSSWALK_2010_to_2020_2019.xlsx")

# Function to fix Arkansas tracts
fix_ar_tracts <- function(data) {
  # Create reference table of valid 2010 Arkansas census tracts
  valid_ar_tracts <- census_crosswalk %>%
    filter(substr(GEOID_2010, 1, 2) == "05") %>%
    select(GEOID_2010) %>%
    distinct()
  
  # Create county-tract reference using only valid tracts
  county_tract_reference <- data %>%
    filter(Year < 2020, STATE == "AR", !is.na(TRACT)) %>%
    mutate(
      TRACT = as.character(TRACT),
      TRACT = case_when(
        nchar(TRACT) == 10 ~ paste0("0", TRACT),
        TRUE ~ TRACT
      )
    ) %>%
    inner_join(valid_ar_tracts, by = c("TRACT" = "GEOID_2010")) %>%
    group_by(COUNTYNAME) %>%
    summarise(
      valid_tract = first(TRACT),
      tract_count = n_distinct(TRACT)
    )
  
  # Apply the fix
  fixed_data <- data %>%
    mutate(
      TRACT = as.character(TRACT),
      TRACT = case_when(
        nchar(TRACT) == 10 ~ paste0("0", TRACT),
        TRUE ~ TRACT
      )
    ) %>%
    left_join(county_tract_reference, by = "COUNTYNAME") %>%
    mutate(
      TRACT = case_when(
        !is.na(TRACT) ~ TRACT,
        is.na(TRACT) & !is.na(valid_tract) ~ valid_tract,
        TRUE ~ NA_character_
      )
    ) %>%
    select(-valid_tract, -tract_count)
  
  return(fixed_data)
}

# Split data into pre-2020 and post-2020 subsets
pre_2020_data <- final_data %>%
  filter(Year < 2020) %>%
  mutate(TRACT = as.character(TRACT))

post_2020_data <- final_data %>%
  filter(Year >= 2020) %>%
  mutate(TRACT = as.character(TRACT))

# Process pre-2020 data
processed_pre_2020 <- pre_2020_data %>%
  # Apply Arkansas fixes
  fix_ar_tracts() %>%
  # Join with crosswalk and apply allocation ratios
  left_join(
    census_crosswalk %>% 
      select(GEOID_2010, GEOID_2020, TOT_RATIO),
    by = c("TRACT" = "GEOID_2010"),
    relationship = "many-to-many"
  ) %>%
  # Apply allocation ratios for split tracts
  mutate(
    final_tract = GEOID_2020,
    Diabetes_Type_2 = Diabetes_Type_2 * TOT_RATIO,
    DFU = DFU * TOT_RATIO
  )

# Process post-2020 data
processed_post_2020 <- post_2020_data %>%
  mutate(final_tract = TRACT)

# Combine datasets
processed_data <- bind_rows(processed_pre_2020, processed_post_2020)

# Verify the results
verification <- processed_data %>%
  group_by(Year < 2020) %>%
  summarise(
    total_records = n(),
    records_with_tract = sum(!is.na(final_tract)),
    match_rate = round(100 * sum(!is.na(final_tract)) / n(), 2)
  )

print("Final verification of all data:")
print(verification)

# Generate detailed report by state and year
detailed_report <- processed_data %>%
  group_by(STATE, Year) %>%
  summarise(
    total_records = n(),
    records_with_tract = sum(!is.na(final_tract)),
    match_rate = round(100 * sum(!is.na(final_tract)) / n(), 2)
  ) %>%
  arrange(STATE, Year)

print("\nDetailed report by state and year:")
print(detailed_report)

# Additional verification for Arkansas
ar_verification <- processed_data %>%
  filter(STATE == "AR") %>%
  group_by(Year) %>%
  summarise(
    total_records = n(),
    records_with_tract = sum(!is.na(final_tract)),
    match_rate = round(100 * sum(!is.na(final_tract)) / n(), 2)
  )

print("\nArkansas specific verification:")
print(ar_verification)

# Save the processed data
write.csv(processed_data, 
          "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 6213 - Variation in Health System Performance/Diabetic Disparities/Data/prov_pat_zip/final_data_processed.csv",
          row.names = FALSE)
