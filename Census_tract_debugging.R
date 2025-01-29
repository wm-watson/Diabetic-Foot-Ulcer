# Diagnostic function for tract format issues
check_tract_formats <- function(data) {
  tract_analysis <- data %>%
    mutate(
      tract_length = nchar(as.character(TRACT)),
      has_decimals = grepl("\\.", as.character(TRACT)),
      is_numeric = !is.na(as.numeric(as.character(TRACT)))
    ) %>%
    group_by(STATE, tract_length, has_decimals, is_numeric) %>%
    summarise(
      count = n(),
      sample_tracts = list(head(unique(TRACT))),
      .groups = "drop"
    ) %>%
    arrange(STATE, desc(count))
  
  print("Tract Format Analysis:")
  print(tract_analysis)
  return(tract_analysis)
}

# Diagnostic function for crosswalk matching
check_crosswalk_matching <- function(data, crosswalk) {
  match_analysis <- data %>%
    left_join(crosswalk, by = c("TRACT" = "GEOID_2010")) %>%
    group_by(STATE) %>%
    summarise(
      total_records = n(),
      matched_records = sum(!is.na(GEOID_2020)),
      match_rate = round(100 * sum(!is.na(GEOID_2020)) / n(), 2),
      sample_unmatched = list(head(unique(TRACT[is.na(GEOID_2020)]))),
      .groups = "drop"
    ) %>%
    arrange(match_rate)
  
  print("\nCrosswalk Matching Analysis:")
  print(match_analysis)
  return(match_analysis)
}

# Run diagnostics on pre-2020 data
print("Running diagnostics on pre-2020 data...")
pre_2020_subset <- final_data %>% filter(Year < 2020)
tract_issues <- check_tract_formats(pre_2020_subset)
matching_issues <- check_crosswalk_matching(pre_2020_subset, census_crosswalk)

# Identify states with low match rates (< 95%)
problem_states <- matching_issues %>%
  filter(match_rate < 95) %>%
  pull(STATE)

if(length(problem_states) > 0) {
  print("\nDetailed analysis of problem states:")
  for(state in problem_states) {
    print(paste("\nAnalyzing state:", state))
    
    # Analyze problematic records for this state
    state_problems <- pre_2020_subset %>%
      filter(STATE == state) %>%
      select(TRACT, STATE, COUNTYNAME) %>%
      distinct() %>%
      left_join(census_crosswalk, by = c("TRACT" = "GEOID_2010")) %>%
      filter(is.na(GEOID_2020)) %>%
      group_by(COUNTYNAME) %>%
      summarise(
        unmatched_count = n(),
        sample_tracts = list(head(unique(TRACT))),
        .groups = "drop"
      ) %>%
      arrange(desc(unmatched_count))
    
    print(state_problems)
  }
}

# Check for data type consistency
check_data_types <- function(data) {
  type_summary <- data %>%
    summarise(across(everything(), 
                     ~paste(class(.), collapse = "/"))) %>%
    gather(column, type) %>%
    arrange(column)
  
  print("\nData Type Analysis:")
  print(type_summary)
  return(type_summary)
}

print("\nChecking data types...")
type_issues <- check_data_types(final_data)

# Summary of all issues found
print("\nSummary of Issues Found:")
print(paste("- Number of states with match rate < 95%:", length(problem_states)))
print(paste("- Number of different tract formats:", nrow(tract_issues)))

# Recommend fixes based on findings
print("\nRecommended Fixes:")
if(length(problem_states) > 0) {
  print("1. State-specific tract formatting fixes needed for:")
  for(state in problem_states) {
    print(paste("   -", state))
  }
}

if(any(tract_issues$tract_length != 11 & !is.na(tract_issues$tract_length))) {
  print("2. Standardize tract lengths to 11 digits")
}

if(any(tract_issues$has_decimals)) {
  print("3. Remove decimal points from tract IDs")
}


## Fix AR Tracts----
fix_ar_tracts <- function(data) {
  # Step 1: Create reference table of valid 2010 Arkansas census tracts
  valid_ar_tracts <- census_crosswalk %>%
    filter(substr(GEOID_2010, 1, 2) == "05") %>%
    select(GEOID_2010) %>%
    distinct()
  
  # Step 2: Create county-tract reference using only valid tracts
  county_tract_reference <- data %>%
    filter(Year < 2020, STATE == "AR", !is.na(TRACT)) %>%
    mutate(
      TRACT = case_when(
        nchar(as.character(TRACT)) == 10 ~ paste0("0", TRACT),
        TRUE ~ as.character(TRACT)
      )
    ) %>%
    inner_join(valid_ar_tracts, by = c("TRACT" = "GEOID_2010")) %>%
    group_by(COUNTYNAME) %>%
    summarise(
      valid_tract = first(TRACT),
      tract_count = n_distinct(TRACT)
    )
  
  # Step 3: Apply the fix
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

# Apply the fix to the pre-2020 Arkansas data
pre_2020_ar_fix <- final_data %>%
  filter(Year < 2020, STATE == "AR") %>%
  fix_ar_tracts()

# Verify the fix
verification <- pre_2020_ar_fix %>%
  left_join(
    census_crosswalk,
    by = c("TRACT" = "GEOID_2010"),
    relationship = "many-to-many"
  ) %>%
  summarise(
    total_records = n(),
    matched_records = sum(!is.na(GEOID_2020)),
    match_rate = round(100 * sum(!is.na(GEOID_2020)) / n(), 2)
  )

print("Final verification of Arkansas tract fixes:")
print(verification)