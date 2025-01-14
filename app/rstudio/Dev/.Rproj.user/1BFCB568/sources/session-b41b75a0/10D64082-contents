# Load necessary libraries
library(httr)
library(jsonlite)
library(openxlsx)

# Set environment variables within R (replace with your actual credentials)
Sys.setenv(DHIS2_BASE_URL = "https://dhis.moh.gov.et")
Sys.setenv(DHIS2_USERNAME = "michaelk")
Sys.setenv(DHIS2_PASSWORD = "Dhis2_12345")

# Function to fetch data from DHIS2 API with error handling
get_dhis2_data <- function(endpoint) {
  cat("Starting to fetch data from DHIS2 API...\n")
  cat("Endpoint:", endpoint, "\n")
  
  dhis2_base_url <- Sys.getenv("DHIS2_BASE_URL")
  dhis2_username <- Sys.getenv("DHIS2_USERNAME")
  dhis2_password <- Sys.getenv("DHIS2_PASSWORD")
  
  url <- paste0(dhis2_base_url, endpoint)
  cat("Constructed URL:", url, "\n")
  
  response <- GET(url, authenticate(dhis2_username, dhis2_password))
  
  if (http_status(response)$category != "Success") {
    stop(paste(
      "Failed to retrieve data from", endpoint, ":",
      http_status(response)$message
    ))
  }
  
  cat("Response received. Status:", http_status(response)$category, "\n")
  
  data <- fromJSON(content(response, as = "text"), flatten = TRUE)
  cat("Successfully fetched data from endpoint:", endpoint, "\n")
  
  return(data)
}

# Function to fetch analytics data for specific indicators, organisation units, and periods
fetch_indicator_data <- function(indicator_ids, org_unit_ids, periods) {
  cat("Fetching analytics data...\n")
  
  dhis2_base_url <- Sys.getenv("DHIS2_BASE_URL")
  dhis2_username <- Sys.getenv("DHIS2_USERNAME")
  dhis2_password <- Sys.getenv("DHIS2_PASSWORD")
  
  # Combine parameters into API-friendly strings
  indicators_str <- paste(indicator_ids, collapse = ";")
  org_units_str <- paste(org_unit_ids, collapse = ";")
  periods_str <- paste(periods, collapse = ";")
  
  cat("Indicators:", indicators_str, "\n")
  cat("Organisation Units:", org_units_str, "\n")
  cat("Periods:", periods_str, "\n")
  
  endpoint <- paste0("/api/analytics.json?dimension=dx:", indicators_str, 
                     "&dimension=ou:", org_units_str, "&dimension=pe:", periods_str)
  
  url <- paste0(dhis2_base_url, endpoint)
  cat("Constructed URL for Analytics Data Request:", url, "\n")
  
  response <- GET(url, authenticate(dhis2_username, dhis2_password))
  
  if (http_status(response)$category != "Success") {
    stop(paste("Failed to fetch data:", http_status(response)$message))
  }
  
  cat("Response received. Status:", http_status(response)$category, "\n")
  
  data <- fromJSON(content(response, as = "text"), flatten = TRUE)
  cat("Successfully fetched data for indicators:", paste(indicator_ids, collapse = ", "), "\n")
  
  return(data)
}

# Function to fetch population data
fetch_population_data <- function(org_unit_ids, periods, zone_ids = NULL) {
  cat("Fetching population data...\n")
  
  population_indicator <- "cpItyCYXKPd"  # population indicator ID
  cat("Using population Indicator ID:", population_indicator, "\n")
  
  # If zone_ids are provided, fetch population based on zone dimension
  if (!is.null(zone_ids)) {
    population_data <- fetch_indicator_data(
      indicator_ids = population_indicator,
      org_unit_ids = zone_ids,
      periods = periods
    )
  } else {
    population_data <- fetch_indicator_data(
      indicator_ids = population_indicator,
      org_unit_ids = org_unit_ids,
      periods = periods
    )
  }
  
  # Convert rows to data frame and assign column names
  population_rows <- as.data.frame(population_data$rows, stringsAsFactors = FALSE)
  colnames(population_rows) <- c("Indicator_ID", "Organisation_Unit_ID", "Period", "Population_Value")
  
  # Ensure Population_Value is numeric
  population_rows$Population_Value <- as.numeric(population_rows$Population_Value)
  
  # Handle missing population values (set to 0)
  population_rows$Population_Value[is.na(population_rows$Population_Value)] <- 0
  
  cat("Population data fetched successfully.\n")
  return(population_rows)
}

# Function to fetch Woreda data recursively, similar to how zone data is fetched
fetch_woreda_data_recursive <- function() {
  cat("Fetching Woreda data recursively...\n")
  
  # Initialize variables
  page_size <- 600  # Number of records per page
  current_page <- 1  # Start from page 1
  all_woreda_data <- list()  # Start with an empty list to store all Woreda data
  
  repeat {
    # Construct the URL with pagination for Woreda data
    endpoint <- paste0("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=", page_size, "&page=", current_page)
    woreda_data_page <- get_dhis2_data(endpoint)  # Get current page of Woreda data
    
    # Check if 'organisationUnits' exists in the response
    if (!is.null(woreda_data_page$organisationUnits)) {
      # Append the current page's Woreda data to the result list
      all_woreda_data <- append(all_woreda_data, woreda_data_page$organisationUnits)
    } else {
      cat("No Woreda data found on page", current_page, "\n")
    }
    
    # Check if there's a next page in the pagination response
    if (!is.null(woreda_data_page$pager$`next`)) {
      current_page <- current_page + 1  # Move to the next page
      cat("Fetching page:", current_page, "\n")
    } else {
      break  # No more pages, exit the loop
    }
  }
  
  # Flatten the list into a data frame if Woreda data was found
  if (length(all_woreda_data) > 0) {
    all_woreda_data <- do.call(rbind, lapply(all_woreda_data, as.data.frame))
  } else {
    stop("No Woreda data found.")
  }
  
  cat("Finished fetching Woreda data.\n")
  return(all_woreda_data)  # Return the final data frame with Woreda IDs and names
}

# Function to format analytics data for a given dimension
format_analytics_data <- function(analytics_data, indicators, org_units, population_data, indicator_map, dimension = "Region") {
  cat("Formatting analytics data...\n")
  
  if (!"rows" %in% names(analytics_data)) {
    warning("No data returned from API.")
    return(data.frame())
  }
  
  rows <- as.data.frame(analytics_data$rows, stringsAsFactors = FALSE)
  
  # Assign column names dynamically based on the number of columns
  colnames(rows) <- c("Indicator_ID", "Organisation_Unit_ID", "Period", "Value")
  
  # Add human-readable names for indicators and organisation units
  rows$indicator_name <- sapply(rows$Indicator_ID, function(id) indicator_map[[id]]$name)
  rows$indicator_abbr <- sapply(rows$Indicator_ID, function(id) indicator_map[[id]]$abbr)
  rows$Organisation_Unit <- org_units$displayName[match(rows$Organisation_Unit_ID, org_units$id)]
  
  # Merge with population data
  rows <- merge(rows, population_data, by = c("Organisation_Unit_ID", "Period"), all.x = TRUE)
  rows$population <- rows$Population_Value
  
  # Ensure Value is numeric
  rows$Value <- as.numeric(rows$Value)
  
  # Add dimension column
  rows$dimension <- dimension
  
  # Calculate standard error (se), confidence intervals
  rows$se <- sqrt(rows$Value * (100 - rows$Value)) / rows$population
  rows$ci_lb <- ""  # rows$Value - 1.96 * rows$se
  rows$ci_ub <- ""  # rows$Value + 1.96 * rows$se
  
  # Additional required columns
  rows$setting <- "Ethiopia"
  rows$date <- rows$Period
  rows$source <- "DHIS_2"
  rows$subgroup <- rows$Organisation_Unit
  rows$estimate <- rows$Value
  
  # Cap estimate values greater than 100
  rows$estimate[rows$estimate > 100] <- 100
  
  rows$note <- ""
  
  # Ensure proportions/percentages are divided by 100
  rows$estimate <- rows$estimate / 100
  
  # Calculate setting average for unique combinations (ensure consistency across setting, year, source, and indicator)
  rows$setting_average <- ave(rows$estimate, rows$indicator_name, rows$source, rows$date, FUN = mean) * 100
  
  # Format setting_average to display as percentage with 2 decimal places
  rows$setting_average <- sprintf("%.2f%%", round(rows$setting_average, 0))
  
  # Check for consistency of setting_average
  setting_average_consistency_check <- with(rows, 
                                            all(ave(setting_average, indicator_name, source, date, FUN = function(x) length(unique(x))) == 1)
  )
  
  if (!setting_average_consistency_check) {
    cat("Warning: Inconsistent setting averages detected for unique combinations.\n")
  }
  
  rows$iso3 <- "ETH"
  rows$favourable_indicator <- 1
  rows$indicator_scale <- 100
  rows$ordered_dimension <- 0
  rows$subgroup_order <- 0
  rows$reference_subgroup <- 0
  
  # Reorder columns
  formatted_rows <- rows[, c("setting", "date", "source", "indicator_abbr", "indicator_name",
                             "dimension", "subgroup", "estimate", "se", "ci_lb", "ci_ub",
                             "population", "note", "setting_average", "iso3", "favourable_indicator",
                             "indicator_scale", "ordered_dimension", "subgroup_order", "reference_subgroup")]
  
  # Sort by Period
  formatted_rows$date <- as.numeric(formatted_rows$date)
  formatted_rows <- formatted_rows[order(formatted_rows$indicator_name, formatted_rows$date), ]
  
  # Replace NA values with 0
  formatted_rows[is.na(formatted_rows)] <- 0
  
  cat("Formatting complete.\n")
  return(formatted_rows)
}

# Indicator mapping for names and abbreviations
indicator_map <- list(
  "AxBNUGGwwXJ" = list("name" = "Skilled birth attendants", "abbr" = "Sba"),
  "qbwqrHsRlr4" = list("name" = "Ratio of hospitals per population", "abbr" = "RHPP")
)

# Fetch metadata
cat("Fetching metadata...\n")
indicators_metadata <- get_dhis2_data("/api/indicators?paging=false")$indicators
org_units_metadata <- get_dhis2_data("/api/organisationUnits?paging=false")$organisationUnits

cat("Fetched metadata. Indicators:", length(indicators_metadata), "Organisation Units:", length(org_units_metadata), "\n")

# Define specific indicators, organisation units, and periods
specific_indicators <- c("AxBNUGGwwXJ", "qbwqrHsRlr4")
specific_org_units <- c("yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
                        "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
                        "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
                        "HIlnt7Qj8do", "Gmw0DJLXGtx")
periods <- c("2016", "2017")

cat("Specific Indicators:", paste(specific_indicators, collapse = ", "), "\n")
cat("Specific Organisation Units:", paste(specific_org_units, collapse = ", "), "\n")
cat("Periods:", paste(periods, collapse = ", "), "\n")

# Fetch population data for Region
population_data_region <- fetch_population_data(specific_org_units, periods)

# Process data for Region
analytics_data_region <- fetch_indicator_data(specific_indicators, specific_org_units, periods)
formatted_data_region <- format_analytics_data(analytics_data_region, indicators_metadata, org_units_metadata, population_data_region, indicator_map, dimension = "Region")

# Process data for Zone
zone_data <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")
specific_zones <- zone_data$organisationUnits$id
population_data_zone <- fetch_population_data(NULL, periods, zone_ids = specific_zones)  # Pass Zone IDs for population data
analytics_data_zone <- fetch_indicator_data(specific_indicators, specific_zones, periods)
formatted_data_zone <- format_analytics_data(analytics_data_zone, indicators_metadata, org_units_metadata, population_data_zone, indicator_map, dimension = "Zone")

# Fetch Woreda data recursively
woreda_data <- fetch_woreda_data_recursive()
specific_woredas <- unlist(lapply(woreda_data, function(x) x$id))  # Extract Woreda IDs
population_data_woreda <- fetch_population_data(NULL, periods, zone_ids = specific_woredas)  # Pass Woreda IDs for population data
analytics_data_woreda <- fetch_indicator_data(specific_indicators, specific_woredas, periods)
formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda")

# Extract Woreda IDs from the recursive structure
specific_woredas <- unlist(lapply(woreda_data, function(x) x$organisationUnit$id))

# Now use the IDs to fetch population and analytics data
population_data_woreda <- fetch_population_data(NULL, periods, zone_ids = specific_woredas)
analytics_data_woreda <- fetch_indicator_data(specific_indicators, specific_woredas, periods)
formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda")

# Combine and write to Excel
combined_data <- rbind(formatted_data_region, formatted_data_zone, formatted_data_woreda)

if (nrow(combined_data) > 0) {
  cat("Writing combined data to Excel...\n")
  write.xlsx(combined_data, file = "07_MOHEATPlus_TEST.xlsx",
             sheetName = "Indicators_Data", rowNames = FALSE)
  cat("Excel file '07_MOHEATPlus_TEST.xlsx' created successfully.\n")
} else {
  cat("No data to write.\n")
}
