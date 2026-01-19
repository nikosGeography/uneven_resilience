# Load required packages
pacman::p_load(
  tidyverse,  # Data manipulation and visualization
  broom,      # Tidy model outputs
  lubridate,  # Date handling
  officer,    # Word document creation
  flextable   # Formatted tables
)

# ==============================================================================
# 1. CONFIGURATION AND DATA LOADING
# ==============================================================================

# Set main directory path (update to your local path)
main_dir <- "path/"

# Define path to lockdown dates CSV
lockdown_csv_path <- file.path("path/lockdown_df.csv")

# Load lockdown dates if not already in environment
if (!exists("lockdown_df")) {
  if (file.exists(lockdown_csv_path)) {
    lockdown_df <- read_csv(lockdown_csv_path, col_types = cols(
      csv_name = col_character(),
      start_date = col_character(),
      end_date = col_character()
    ))
  } else {
    stop("Lockdown dates not found. Please provide lockdown_df.csv or load the dataframe.")
  }
}

# Normalize and parse lockdown dates
lockdown_df <- lockdown_df |>
  mutate(
    start_date = dmy(start_date),  # Convert to Date format
    end_date   = dmy(end_date)
  )

# ==============================================================================
# 2. DISCOVER DATA FILES
# ==============================================================================

# Recursively find all dissolved.csv files (containing NTL trend data)
dissolved_paths <- list.files(
  path = main_dir,
  pattern = "dissolved\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Validate that files were found
if (length(dissolved_paths) == 0) {
  stop("No dissolved.csv files found in the specified directory.")
}

# Helper function to extract city name from file path
get_city_name <- function(file_path, main_dir = "Main") {
  # Remove main directory path to get relative path
  rel <- sub(paste0("^", normalizePath(main_dir, winslash = "/"), "/?"), "", 
             normalizePath(file_path, winslash = "/"))
  parts <- strsplit(rel, "/|\\\\")[[1]]
  parts[1]  # First element after main_dir is city name
}

# ==============================================================================
# 3. PROCESSING FUNCTION FOR SINGLE FILE
# ==============================================================================

process_dissolved <- function(path) {
  # Extract city name from file path
  city <- get_city_name(path, main_dir = main_dir)
  
  # Read the dissolved.csv file
  df <- read_csv(path, col_types = cols(
    Date = col_character(),
    trend = col_double()
  ))
  
  # Parse dates and sort chronologically
  df <- df |>
    mutate(Date = ymd(Date)) |>
    arrange(Date)
  
  # Validate date parsing
  if (all(is.na(df$Date))) {
    stop("All dates failed to parse in file: ", path)
  }
  
  # Get lockdown dates for this city
  lk_row <- lockdown_df |> filter(csv_name == city)
  if (nrow(lk_row) == 0) {
    stop("No lockdown dates found for city: ", city)
  }
  start_dt <- lk_row$start_date[1]
  end_dt <- lk_row$end_date[1]
  
  # Assign observations to time periods
  df_periods <- df |>
    mutate(
      period = case_when(
        Date < start_dt ~ "pre_lockdown",
        Date >= start_dt & Date <= end_dt ~ "during_lockdown",
        Date > end_dt ~ "post_lockdown",
        TRUE ~ NA_character_
      )
    )
  
  # Convert dates to numeric (days since first observation)
  # Improves model stability and interpretation
  origin_date <- min(df$Date, na.rm = TRUE)
  df_periods <- df_periods |>
    mutate(
      date_numeric = as.numeric(difftime(Date, origin_date, units = "days"))
    )
  
  # Fit linear models and extract diagnostics for each period
  results <- df_periods |>
    group_by(period) |>
    summarise(
      n = sum(!is.na(trend) & !is.na(date_numeric)),  # Valid observations
      data_list = list(
        tibble(date_numeric = date_numeric, trend = trend) |> 
          filter(!is.na(trend) & !is.na(date_numeric))
      ),
      .groups = "drop"
    ) |>
    mutate(
      model_info = map2(n, data_list, ~ {
        # Only fit model if at least 2 observations
        if (.x < 2) {
          tibble(r2 = NA_real_, p_value = NA_real_)
        } else {
          # Attempt linear model: trend ~ time
          fit <- try(lm(trend ~ date_numeric, data = .y), silent = TRUE)
          if (inherits(fit, "try-error")) {
            tibble(r2 = NA_real_, p_value = NA_real_)
          } else {
            s <- summary(fit)
            # Extract p-value for slope (time coefficient)
            coef_tbl <- coef(s)
            p_val <- if ("date_numeric" %in% rownames(coef_tbl)) {
              coef_tbl["date_numeric", "Pr(>|t|)"]
            } else {
              NA_real_
            }
            tibble(r2 = s$r.squared, p_value = p_val)
          }
        }
      })
    ) |>
    unnest(cols = c(model_info)) |>
    select(period, n, r2, p_value)
  
  # Ensure all three periods are represented (fill missing with NA)
  all_periods <- tibble(period = c("pre_lockdown", "during_lockdown", "post_lockdown"))
  results <- all_periods |>
    left_join(results, by = "period") |>
    mutate(
      city = city,
      dissolved_path = path,
      start_date = start_dt,
      end_date = end_dt
    ) |>
    select(city, dissolved_path, start_date, end_date, period, n, r2, p_value)
  
  return(results)
}

# ==============================================================================
# 4. PROCESS ALL FILES WITH ERROR HANDLING
# ==============================================================================

# Process each file with tryCatch to handle errors gracefully
results_list <- purrr::map(dissolved_paths, function(p) {
  tryCatch(
    {
      process_dissolved(p)
    },
    error = function(e) {
      # Return error information in structured format
      tibble(
        city = get_city_name(p, main_dir = main_dir),
        dissolved_path = p,
        start_date = NA_Date_,
        end_date = NA_Date_,
        period = NA_character_,
        n = NA_integer_,
        r2 = NA_real_,
        p_value = NA_real_,
        error_msg = as.character(e$message)
      )
    }
  )
})

# Combine all results into single dataframe
final_results <- dplyr::bind_rows(results_list)

# ==============================================================================
# 5. FORMAT AND DISPLAY RESULTS
# ==============================================================================

# Print formatted results to console
final_results |>
  arrange(city, dissolved_path, 
          factor(period, levels = c("pre_lockdown", "during_lockdown", "post_lockdown"))) |>
  mutate(
    r2 = round(r2, 2),  # Round R² to 2 decimal places
    # Format very small p-values
    p_value = ifelse(!is.na(p_value) & p_value < 2e-16, "<2e-16", signif(p_value, 2))
  ) |>
  print(n = Inf)

# ==============================================================================
# 6. AGGREGATE ANALYSES BY SECTOR AND PERIOD
# ==============================================================================

# Helper function to extract sector name from file path
get_sector_name <- function(file_path) {
  parts <- strsplit(normalizePath(file_path, winslash = "/"), "/")[[1]]
  lu_idx <- which(parts == "lu")  # Find "lu" directory
  if (length(lu_idx) == 0) {
    return(NA_character_)
  }
  if (length(parts) >= lu_idx + 1) {
    return(parts[lu_idx + 1])  # Sector is next directory after "lu"
  }
  NA_character_
}

# Add sector information to results
final_results <- final_results |>
  mutate(
    sector = purrr::map_chr(dissolved_path, get_sector_name)
  )

# Calculate mean statistics by sector and period
sector_period_summary <- final_results |>
  filter(!is.na(period)) |>
  group_by(sector, period) |>
  summarise(
    mean_r2 = mean(r2, na.rm = TRUE),
    mean_p  = mean(p_value, na.rm = TRUE),
    mean_n  = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(sector, factor(period, levels = c("pre_lockdown", "during_lockdown", "post_lockdown")))

print(sector_period_summary, n = Inf)

# Calculate overall statistics by sector (collapsing periods)
sector_summary_overall <- final_results |>
  filter(!is.na(sector)) |>
  summarise(
    mean_r2 = mean(r2, na.rm = TRUE),
    mean_p = mean(p_value, na.rm = TRUE),
    mean_n = mean(n, na.rm = TRUE),
    .by = sector
  ) |>
  arrange(sector)

print(sector_summary_overall, n = Inf)

# Calculate overall statistics by period (collapsing sectors)
period_summary_overall <- final_results |>
  filter(!is.na(period)) |>
  summarise(
    mean_r2 = mean(r2, na.rm = TRUE),
    mean_p = mean(p_value, na.rm = TRUE),
    mean_n = mean(n, na.rm = TRUE),
    .by = period
  ) |>
  arrange(factor(period, levels = c("pre_lockdown", "during_lockdown", "post_lockdown")))

print(period_summary_overall, n = Inf)

# ==============================================================================
# 7. PREPARE DATA FOR TABLE EXPORT
# ==============================================================================

# Prepare city-level data with simplified period names
combined_data <- final_results |>
  filter(!is.na(period) & !is.na(sector)) |>
  mutate(
    period = case_when(
      period == "pre_lockdown" ~ "Pre",
      period == "during_lockdown" ~ "During",
      period == "post_lockdown" ~ "Post",
      TRUE ~ period
    )
  ) |>
  select(sector, city, period, n, r2, p_value)

# Prepare sector-period mean rows
sector_period_means <- sector_period_summary |>
  mutate(
    city = "Mean (Sector-Period)",
    period = case_when(
      period == "pre_lockdown" ~ "Pre",
      period == "during_lockdown" ~ "During",
      period == "post_lockdown" ~ "Post",
      TRUE ~ period
    ),
    n = mean_n,
    r2 = mean_r2,
    p_value = mean_p
  ) |>
  select(sector, city, period, n, r2, p_value)

# Prepare sector overall mean rows
sector_overall_means <- sector_summary_overall |>
  mutate(
    city = "Mean (Sector Overall)",
    period = "Pre",  # Placeholder for consistent structure
    n = mean_n,
    r2 = mean_r2,
    p_value = mean_p
  ) |>
  select(sector, city, period, n, r2, p_value)

# Combine all data types
combined_data <- bind_rows(
  combined_data,
  sector_period_means,
  sector_overall_means
)

# ==============================================================================
# 8. CITY NAME MAPPING AND DATA RESHAPING
# ==============================================================================

# Map abbreviated city codes to full names
city_rows <- combined_data |>
  filter(!str_detect(city, "Mean")) |>  # Exclude mean rows
  mutate(city = case_when(
    city == "ba" ~ "Buenos Aires",
    city == "hk" ~ "Hong Kong",
    city == "sp" ~ "Sao Paulo",
    city == "rio" ~ "Rio de Janeiro",
    city == "cape" ~ "Cape Town",
    city == "la" ~ "Los Angeles",
    city == "mexico" ~ "Mexico City",
    city == "abu" ~ "Abu Dhabi",
    city == "ny" ~ "New York",
    city == "wash" ~ "Washington DC",
    TRUE ~ str_to_title(city)  # Capitalize remaining city names
  ))

# Separate mean rows
sector_period_mean <- combined_data |> filter(city == "Mean (Sector-Period)")
sector_overall_mean <- combined_data |> filter(city == "Mean (Sector Overall)")

# Reshape city data to wide format (periods as columns)
city_wide <- city_rows |>
  pivot_wider(
    names_from = period,
    values_from = c(n, r2, p_value),
    names_glue = "{.value}_{period}"
  ) |>
  select(
    sector, city,
    n_Pre, n_During, n_Post,
    r2_Pre, r2_During, r2_Post,
    p_value_Pre, p_value_During, p_value_Post
  )

# Reshape sector-period mean data
sector_period_wide <- sector_period_mean |>
  pivot_wider(
    names_from = period,
    values_from = c(n, r2, p_value),
    names_glue = "{.value}_{period}"
  ) |>
  select(
    sector, city,
    n_Pre, n_During, n_Post,
    r2_Pre, r2_During, r2_Post,
    p_value_Pre, p_value_During, p_value_Post
  )

# Prepare sector overall mean data (only Pre column populated)
sector_overall_wide <- sector_overall_mean |>
  mutate(
    n_Pre = n,
    r2_Pre = r2,
    p_value_Pre = p_value,
    n_During = NA_real_,
    n_Post = NA_real_,
    r2_During = NA_real_,
    r2_Post = NA_real_,
    p_value_During = NA_real_,
    p_value_Post = NA_real_
  ) |>
  select(
    sector, city,
    n_Pre, n_During, n_Post,
    r2_Pre, r2_During, r2_Post,
    p_value_Pre, p_value_During, p_value_Post
  )

# ==============================================================================
# 9. COMBINE AND ORDER DATA FOR FINAL TABLE
# ==============================================================================

# Define sector order (alphabetical)
sector_order <- c("airport", "commercial", "industrial", "retail")
sectors <- sector_order[sector_order %in% unique(city_wide$sector)]

# Combine data in desired order: cities, then sector-period means, then sector overall means
wide_data <- map_dfr(sectors, function(s) {
  bind_rows(
    city_wide |> filter(sector == s),           # Individual cities
    sector_period_wide |> filter(sector == s),  # Sector-period means
    sector_overall_wide |> filter(sector == s)  # Sector overall means
  )
})

# Capitalize sector names for display
wide_data <- wide_data |>
  mutate(sector = str_to_title(sector))

# ==============================================================================
# 10. CREATE FORMATTED TABLE WITH FLEXTABLE
# ==============================================================================

# Initialize flextable with data
ft_wide <- flextable(wide_data) |>
  # Set column labels
  set_header_labels(
    sector = "Sector",
    city = "City",
    n_Pre = "Pre",
    n_During = "During",
    n_Post = "Post",
    r2_Pre = "Pre",
    r2_During = "During",
    r2_Post = "Post",
    p_value_Pre = "Pre",
    p_value_During = "During",
    p_value_Post = "Post"
  ) |>
  # Add top header row for metric categories
  add_header_row(
    values = c("Sector", "City", "N", "N", "N", "R²", "R²", "R²", "P-Values", "P-Values", "P-Values"),
    colwidths = rep(1, 11)
  ) |>
  # Merge header cells for better readability
  merge_v(j = 1:2, part = "header") |>  # Merge Sector and City headers
  merge_h(i = 1, part = "header") |>    # Merge top header row
  # Format numeric values
  set_formatter(
    n_Pre = function(x) ifelse(is.na(x), "", as.character(round(x, 0))),
    n_During = function(x) ifelse(is.na(x), "", as.character(round(x, 0))),
    n_Post = function(x) ifelse(is.na(x), "", as.character(round(x, 0))),
    r2_Pre = function(x) ifelse(is.na(x), "", sprintf("%.2f", x)),
    r2_During = function(x) ifelse(is.na(x), "", sprintf("%.2f", x)),
    r2_Post = function(x) ifelse(is.na(x), "", sprintf("%.2f", x)),
    p_value_Pre = function(x) ifelse(is.na(x), "", sprintf("%.2f", x)),
    p_value_During = function(x) ifelse(is.na(x), "", sprintf("%.2f", x)),
    p_value_Post = function(x) ifelse(is.na(x), "", sprintf("%.2f", x))
  ) |>
  # Style header
  bg(bg = "#D8D6C2", part = "header") |>  # Header background color
  border_remove() |>                      # Remove default borders
  # Add custom borders
  hline_top(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "body") |>
  # Auto-adjust column widths
  autofit() |>
  # Set font and alignment
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "header") |>
  align(j = 3:11, align = "center", part = "body") |>
  bold(part = "header")  # Bold header text

# ==============================================================================
# 11. ADDITIONAL FORMATTING AND HIGHLIGHTING
# ==============================================================================

# Add alternating row shading (excluding Sector column)
ft_wide <- ft_wide |>
  bg(i = seq(2, nrow(wide_data), by = 2), j = 2:11, bg = "#ECEADF", part = "body")

# Identify mean rows for highlighting
mean_period_rows <- which(wide_data$city == "Mean (Sector-Period)")
mean_overall_rows <- which(wide_data$city == "Mean (Sector Overall)")

# Bold mean rows
if (length(mean_period_rows) > 0) {
  ft_wide <- ft_wide |> bold(i = mean_period_rows, bold = TRUE, part = "body")
}

if (length(mean_overall_rows) > 0) {
  ft_wide <- ft_wide |> bold(i = mean_overall_rows, bold = TRUE, part = "body")
}

# Add horizontal lines above mean rows for visual separation
if (length(mean_period_rows) > 0) {
  ft_wide <- ft_wide |>
    hline(i = mean_period_rows - 1, border = fp_border(color = "black", width = 0.5), part = "body")
}

if (length(mean_overall_rows) > 0) {
  ft_wide <- ft_wide |>
    hline(i = mean_overall_rows - 1, border = fp_border(color = "black", width = 0.5), part = "body")
}

# ==============================================================================
# 12. SECTOR-SPECIFIC COLOR CODING
# ==============================================================================

# Apply sector-specific background colors to Sector column
airport_rows <- which(wide_data$sector == "Airport")
commercial_rows <- which(wide_data$sector == "Commercial")
industrial_rows <- which(wide_data$sector == "Industrial")
retail_rows <- which(wide_data$sector == "Retail")

# Color coding by sector
if (length(airport_rows) > 0) {
  ft_wide <- ft_wide |> bg(i = airport_rows, j = "sector", bg = "#E8B4A8", part = "body")
}

if (length(commercial_rows) > 0) {
  ft_wide <- ft_wide |> bg(i = commercial_rows, j = "sector", bg = "#BDA699", part = "body")
}

if (length(industrial_rows) > 0) {
  ft_wide <- ft_wide |> bg(i = industrial_rows, j = "sector", bg = "#A1A1A1", part = "body")
}

if (length(retail_rows) > 0) {
  ft_wide <- ft_wide |> bg(i = retail_rows, j = "sector", bg = "#72C6CC", part = "body")
}

# Merge sector cells vertically for cleaner look
ft_wide <- ft_wide |> merge_v(j = "sector", part = "body")

# ==============================================================================
# 13. FINAL OUTPUT
# ==============================================================================

# Create Word document
doc_wide <- read_docx() |> body_add_flextable(value = ft_wide)

# Display table in R
print(ft_wide)

# Save to file (uncomment and update path as needed)
# print(doc_wide, target = "path/r2_diagnostics_table.docx")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================