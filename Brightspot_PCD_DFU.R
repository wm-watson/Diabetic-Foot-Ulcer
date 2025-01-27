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

# Map zip to census tract
library(httr)
library(jsonlite)
library(dplyr)

# Function to query the Census Geocoding API with retries
get_census_tract_with_year <- function(zip_code, year, month, retries = 3) {
  # Determine the benchmark based on the year
  benchmark <- ifelse(year <= 2010, "Public_AR_Census2010", "Public_AR_Current")
  
  # Base URL for the Census Geocoding API
  base_url <- "https://geocoding.geo.census.gov/geocoder/locations/address"
  
  # Retry logic
  for (attempt in 1:retries) {
    # Query the API
    response <- GET(base_url, query = list(
      street = "",               # Leave blank for ZIP-level geocoding
      city = "",
      state = "",                # Not needed for ZIP-only queries
      zip = zip_code,            # Provide the ZIP code
      benchmark = benchmark,     # Specify the benchmark
      format = "json"            # Request JSON format
    ))
    
    # If the response is successful, parse and return the result
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed", type = "application/json")
      if (!is.null(result$result$addressMatches) && length(result$result$addressMatches) > 0) {
        tract <- result$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$TRACT
        county <- result$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$COUNTY
        state <- result$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$STATE
        return(data.frame(PAT_ZIP_5 = zip_code, Year = year, Month = month, TRACT = tract, COUNTY = county, STATE = state))
      } else {
        return(data.frame(PAT_ZIP_5 = zip_code, Year = year, Month = month, TRACT = NA, COUNTY = NA, STATE = NA))
      }
    }
    
    # If the response fails, wait and retry
    Sys.sleep(1)  # Delay before retrying
  }
  
  # If all retries fail, log the failure
  warning(paste("Failed to fetch data for ZIP:", zip_code, "Year:", year, "Month:", month))
  return(data.frame(PAT_ZIP_5 = zip_code, Year = year, Month = month, TRACT = NA, COUNTY = NA, STATE = NA))
}

# Function to process ZIP-year-month combinations in batches
process_batches <- function(data, batch_size = 100, delay = 3) {
  results <- data.frame()
  
  # Loop through the data in batches
  for (i in seq(1, nrow(data), by = batch_size)) {
    batch <- data[i:min(i + batch_size - 1, nrow(data)), ]
    
    # Process each row in the batch
    batch_results <- do.call(rbind, lapply(1:nrow(batch), function(j) {
      row <- batch[j, ]
      get_census_tract_with_year(row$PAT_ZIP_5, row$Year, row$Month)
    }))
    
    # Append the batch results to the final results
    results <- rbind(results, batch_results)
    
    # Delay between batches to respect API rate limits
    Sys.sleep(delay)
  }
  
  return(results)
}

# Extract unique combinations of ZIP, Year, and Month
unique_zip_year <- t2d_AR %>%
  select(PAT_ZIP_5, Year, Month) %>%
  distinct()

# Process the data in batches
results <- process_batches(unique_zip_year, batch_size = 100, delay = 3)

# Merge results back into the original dataset
enriched_data <- t2d_AR %>%
  left_join(results, by = c("PAT_ZIP_5", "Year", "Month"))

# Save the enriched dataset
write.csv(enriched_data, "t2d_AR_with_tracts.csv", row.names = FALSE)

# Inspect the enriched dataset
head(enriched_data)

# Log failed requests
failed_requests <- results %>% filter(is.na(TRACT))
if (nrow(failed_requests) > 0) {
  write.csv(failed_requests, "failed_requests.csv", row.names = FALSE)
  message("Some requests failed. Check 'failed_requests.csv' for details.")
}

# Retry failed requests (if any)
if (nrow(failed_requests) > 0) {
  retried_results <- process_batches(failed_requests, batch_size = 50, delay = 5)
  
  # Combine retried results with the original results
  results <- results %>%
    filter(!is.na(TRACT)) %>% # Keep only successful results
    bind_rows(retried_results)
  
  # Update the failed ZIP codes log
  failed_requests <- results %>% filter(is.na(TRACT))
  if (nrow(failed_requests) > 0) {
    write.csv(failed_requests, "failed_requests.csv", row.names = FALSE)
    message("Some ZIP codes still failed after retries.")
  }
}