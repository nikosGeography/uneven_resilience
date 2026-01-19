# Load required packages
pacman::p_load(
  dplyr,     # Data manipulation
  tidyr,      # Data reshaping
  ggplot2,    # Plotting
  plotly,     # Interactive plots (optional)
  purrr       # Functional programming
)

# Define file paths
main_dir <- "path/clipped_new"  # Main data directory
lockdown_csv <- "path/lockdown_df.csv"  # Lockdown dates

# Control flag: whether to write month indices back to source files
write_back_monthNum <- FALSE  # Set to TRUE to update source files with month indices

# ==============================================================================
# 1. TIME INDEX MAPPING FUNCTION
# ==============================================================================

# Convert dates to monthly indices (January 2018 = 1, December 2023 = 72)
to_month_index <- function(d) {
  if (is.na(d)) {
    return(NA_integer_)
  }
  y <- as.integer(format(d, "%Y"))   # Extract year
  m <- as.integer(format(d, "%m"))   # Extract month
  idx <- (y - 2018) * 12 + m         # Calculate index relative to Jan 2018
  if (idx < 1 || idx > 72) {         # Validate within study period
    return(NA_integer_)
  }
  idx
}

# ==============================================================================
# 2. LOAD AND PROCESS LOCKDOWN DATES
# ==============================================================================

# Read lockdown period definitions (city-specific start/end dates)
lockdown_df <- read.csv(lockdown_csv, stringsAsFactors = FALSE)

# Parse dates and convert to monthly indices
lockdown_df <- lockdown_df %>%
  mutate(
    start_date = as.Date(start_date, format = "%d-%m-%y"),  # Convert to Date
    end_date   = as.Date(end_date, format = "%d-%m-%y"),
    start_idx  = sapply(start_date, to_month_index),        # Start month index
    end_idx    = sapply(end_date, to_month_index)           # End month index
  ) %>%
  # Remove invalid entries and ensure chronological order
  filter(!is.na(start_idx), !is.na(end_idx), start_idx <= end_idx)

# Create named list: city name → [start_index, end_index]
knot_positions <- setNames(
  Map(c, lockdown_df$start_idx, lockdown_df$end_idx),
  lockdown_df$csv_name
)

# ==============================================================================
# 3. DISCOVER AND LOAD INDUSTRIAL SECTOR DATA
# ==============================================================================

# Identify city directories (top-level folders)
city_dirs <- list.dirs(main_dir, recursive = FALSE, full.names = TRUE)

# Function to read industrial NTL data for a single city
read_industrial_city <- function(city_dir, write_back = FALSE) {
  city <- basename(city_dir)  # Extract city name from directory
  
  # Construct file path to industrial dissolved CSV
  fpath <- file.path(city_dir, "lu", "industrial", "dissolved.csv")
  
  # Return NULL if file doesn't exist
  if (!file.exists(fpath)) {
    return(NULL)
  }
  
  # Read CSV with error handling
  df <- tryCatch(
    read.csv(fpath, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(df)) {
    return(NULL)
  }
  
  # Validate data structure: must have 72 rows (6 years × 12 months)
  if (nrow(df) != 72) {
    message(sprintf("Skipping %s (industrial): expected 72 rows, found %d", 
                    city, nrow(df)))
    return(NULL)
  }
  
  # Add month indices (1-72) and city identifier
  df$monthNum <- 1:72
  df$city <- city
  
  # Optionally write back to source file (with month indices)
  if (write_back) {
    try(
      {
        write.csv(df, fpath, row.names = FALSE)
      },
      silent = TRUE
    )
  }
  
  return(df)
}

# Read data for all cities
industrial_list <- lapply(city_dirs, read_industrial_city, 
                          write_back = write_back_monthNum)

# Remove NULL entries (cities without valid data)
industrial_list <- industrial_list[!sapply(industrial_list, is.null)]

# Check if any data was loaded
if (length(industrial_list) == 0) {
  stop("No industrial dissolved.csv files found with 72 rows.")
}

# Combine all city data into single dataframe
industrial_data <- bind_rows(industrial_list)

# Filter to cities with defined lockdown periods
industrial_data <- industrial_data %>% 
  filter(city %in% names(knot_positions))

if (nrow(industrial_data) == 0) {
  stop("No industrial cities with valid lockdown dates after filtering.")
}

# ==============================================================================
# 4. PIECEWISE LINEAR MODELING FUNCTION
# ==============================================================================

# Fit linear models and predict NTL at period midpoints for a single city
fit_midpoints_city <- function(df_city) {
  city <- df_city$city[1]  # Get city name
  knots <- knot_positions[[city]]  # Get lockdown start/end indices
  
  # Return NA if no lockdown dates defined
  if (is.null(knots)) {
    return(tibble(
      city = city,
      period1_midpoint = NA_real_,
      period2_midpoint = NA_real_,
      period3_midpoint = NA_real_
    ))
  }
  
  # Extract lockdown period indices
  start_idx <- knots[1]
  end_idx <- knots[2]
  
  # Split data into three periods:
  # Period 1: Pre-lockdown (months 1 to start_idx-1)
  # Period 2: During lockdown (months start_idx to end_idx)
  # Period 3: Post-lockdown (months end_idx+1 to 72)
  period1 <- df_city %>% filter(monthNum <= (start_idx - 1))
  period2 <- df_city %>% filter(monthNum >= start_idx, monthNum <= end_idx)
  period3 <- df_city %>% filter(monthNum >= (end_idx + 1))
  
  # Helper function to fit linear model (robust to small sample sizes)
  fit_or_null <- function(dat) {
    if (nrow(dat) >= 2) {
      lm(trend ~ monthNum, data = dat)  # Linear trend
    } else if (nrow(dat) == 1) {
      lm(trend ~ 1, data = dat)         # Intercept-only (constant)
    } else {
      NULL                              # Empty period
    }
  }
  
  # Fit models for each period
  model1 <- fit_or_null(period1)
  model2 <- fit_or_null(period2)
  model3 <- fit_or_null(period3)
  
  # Calculate midpoint month for each period
  mid1 <- if (nrow(period1) >= 1) mean(c(1, start_idx - 1)) else NA_real_
  mid2 <- if (nrow(period2) >= 1) mean(c(start_idx, end_idx)) else NA_real_
  mid3 <- if (nrow(period3) >= 1) mean(c(end_idx + 1, 72)) else NA_real_
  
  # Helper function to predict from model (returns NA if no model)
  predict_or_na <- function(mod, x) {
    if (is.null(mod) || is.na(x)) {
      return(NA_real_)
    }
    predict(mod, newdata = data.frame(monthNum = x))
  }
  
  # Return predicted NTL at each period midpoint
  tibble(
    city = city,
    period1_midpoint = predict_or_na(model1, mid1),
    period2_midpoint = predict_or_na(model2, mid2),
    period3_midpoint = predict_or_na(model3, mid3)
  )
}

# ==============================================================================
# 5. COMPUTE MIDPOINTS FOR ALL CITIES
# ==============================================================================

# Apply fitting function to each city and combine results
midpoints_df <- industrial_data %>%
  group_by(city) %>%                 # Group by city
  group_split() %>%                  # Split into list of city dataframes
  lapply(fit_midpoints_city) %>%     # Apply fitting function
  bind_rows()                        # Combine results

# ==============================================================================
# 6. NORMALIZE BY CITY-SPECIFIC MEANS
# ==============================================================================

# Mean-center NTL values to account for baseline brightness differences
midpoints_df <- midpoints_df %>%
  rowwise() %>%  # Operate row-wise for city-specific calculations
  mutate(
    # Calculate city mean across all three periods
    city_mean = mean(c(period1_midpoint, period2_midpoint, period3_midpoint), 
                     na.rm = TRUE),
    # Center each period relative to city mean
    p1_center = period1_midpoint - city_mean,
    p2_center = period2_midpoint - city_mean,
    p3_center = period3_midpoint - city_mean
  ) %>%
  ungroup()  # Return to normal dataframe operations

# ==============================================================================
# 7. RESILIENCE ARCHETYPE CLASSIFICATION
# ==============================================================================

# Define classification logic based on NTL trend patterns
classify_city <- function(s1, s2, s3) {
  # s1 = pre-lockdown, s2 = during lockdown, s3 = post-lockdown
  
  # Full recovery: lockdown decline followed by full rebound
  if (s2 < s1 && s3 > s1) {
    return("Full recovery")
  }
  
  # Partial recovery: lockdown decline with incomplete recovery
  if (s2 < s1 && s3 < s1 && s3 > s2) {
    return("Partial recovery")
  }
  
  # Resilient: no significant decline or sustained growth
  if ((s2 >= s1 && s3 >= s2) || (s2 >= s1 && s3 >= s1) || (s3 >= s1 && s2 >= s3)) {
    return("Resilient")
  }
  
  # Chronic decline: sustained decline through all periods
  if (s2 < s1 && s3 < s2) {
    return("Chronic decline")
  }
  
  # Catch-all for ambiguous patterns
  return("Other")
}

# Apply classification to each city
midpoints_df$class <- mapply(
  classify_city,
  midpoints_df$period1_midpoint,
  midpoints_df$period2_midpoint,
  midpoints_df$period3_midpoint
)

# ==============================================================================
# 8. SAVE RESULTS
# ==============================================================================

# Write classified midpoint data to CSV for downstream analysis
write.csv(midpoints_df, 
          "path/industrial_midpoint.csv", 
          row.names = FALSE)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================

# ==============================================================================
# 7. EXPORT HIGH-QUALITY VECTOR GRAPHIC
# ==============================================================================

# Save as SVG for publication-quality vector graphic
# Note: Update file path to your desired location
ggsave(
  filename = "figure2.svg",  # Output filename
  plot = combined_plot,
  device = svglite,          # SVG device for vector output
  width = 180,               # Width in mm (standard figure width)
  height = 100,              # Height in mm
  units = "mm",
  dpi = 300,                 # High resolution
  scaling = 1                # Critical: prevents font rescaling in SVG
)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================