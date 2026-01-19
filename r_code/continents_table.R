pacman::p_load(dplyr,      # Data manipulation
               purrr,      # Functional programming
               officer,    # Word document creation
               flextable,  # Formatted tables
               tidyselect, 
               tidyr)      # Data reshaping

# Read the midpoint data for all sectors with na.strings
retail_df <- read.csv("C:/Users/nikos/OneDrive/Desktop/clipped_new/retail_midpoint.csv",
                      na.strings = c("NA", "")
) |>
  mutate(Sector = "Retail") |>
  filter(template != "Other")

commercial_df <- read.csv("C:/Users/nikos/OneDrive/Desktop/clipped_new/commercial_midpoint.csv",
                          na.strings = c("NA", "")
) |>
  mutate(Sector = "Commercial")

industrial_df <- read.csv("C:/Users/nikos/OneDrive/Desktop/clipped_new/industrial_midpoint.csv",
                          na.strings = c("NA", "")
) |>
  mutate(Sector = "Industrial")

# Combine all dataframes
df <- rbind(commercial_df, industrial_df, retail_df)

# Remove toronto and warsaw
df <- df |>
  filter(city != "toronto" & city != "warsaw")

# Remove NAs completely
df <- df |>
  filter(!is.na(template), !is.na(Sector))

# Calculate all magnitudes
df$decline <- df$period2_midpoint - df$period1_midpoint
df$recovery <- df$period3_midpoint - df$period2_midpoint
df$net_change <- df$period3_midpoint - df$period1_midpoint

# Define template order
template_order <- c("Chronic decline", "Partial recovery", "Full recovery", "Resilient")

# City name mapping
city_name_map <- c(
  "ba" = "Buenos Aires",
  "ny" = "New York",
  "hk" = "Hong Kong",
  "la" = "Los Angeles",
  "wash" = "Washington DC",
  "sp" = "Sao Paulo",
  "cape" = "Cape Town",
  "mexico" = "Mexico City",
  "abu" = "Abu Dhabi",
  "rio" = "Rio De Janeiro",
  "almaty" = "Almaty",
  "amsterdam" = "Amsterdam",
  "athens" = "Athens",
  "baku" = "Baku",
  "beijing" = "Beijing",
  "berlin" = "Berlin",
  "brisbane" = "Brisbane",
  "cairo" = "Cairo",
  "caracas" = "Caracas",
  "chicago" = "Chicago",
  "dallas" = "Dallas",
  "delhi" = "Delhi",
  "dubai" = "Dubai",
  "frankfurt" = "Frankfurt",
  "guangzhou" = "Guangzhou",
  "istanbul" = "Istanbul",
  "jeddah" = "Jeddah",
  "johannesburg" = "Johannesburg",
  "lahore" = "Lahore",
  "lima" = "Lima",
  "london" = "London",
  "madrid" = "Madrid",
  "manchester" = "Manchester",
  "melbourne" = "Melbourne",
  "milan" = "Milan",
  "mumbai" = "Mumbai",
  "munich" = "Munich",
  "nairobi" = "Nairobi",
  "paris" = "Paris",
  "pune" = "Pune",
  "riyadh" = "Riyadh",
  "rome" = "Rome",
  "santiago" = "Santiago",
  "shanghai" = "Shanghai",
  "shenzhen" = "Shenzhen",
  "sydney" = "Sydney",
  "vienna" = "Vienna",
  "wuhan" = "Wuhan"
)

# Define continent mapping for all cities
city_continent_map <- c(
  # North America
  "New York" = "North America",
  "Los Angeles" = "North America",
  "Washington DC" = "North America",
  "Chicago" = "North America",
  "Dallas" = "North America",
  
  # Central America
  "Mexico City" = "Central America",
  
  # South America
  "Buenos Aires" = "South America",
  "Sao Paulo" = "South America",
  "Rio De Janeiro" = "South America",
  "Lima" = "South America",
  "Santiago" = "South America",
  "Caracas" = "South America",
  
  # Europe
  "London" = "Europe",
  "Paris" = "Europe",
  "Berlin" = "Europe",
  "Madrid" = "Europe",
  "Rome" = "Europe",
  "Milan" = "Europe",
  "Amsterdam" = "Europe",
  "Frankfurt" = "Europe",
  "Munich" = "Europe",
  "Vienna" = "Europe",
  "Athens" = "Europe",
  "Manchester" = "Europe",
  "Istanbul" = "Europe",
  
  # Asia
  "Hong Kong" = "Asia",
  "Abu Dhabi" = "Asia",
  "Beijing" = "Asia",
  "Shanghai" = "Asia",
  "Guangzhou" = "Asia",
  "Shenzhen" = "Asia",
  "Wuhan" = "Asia",
  "Dubai" = "Asia",
  "Delhi" = "Asia",
  "Mumbai" = "Asia",
  "Pune" = "Asia",
  "Lahore" = "Asia",
  "Jeddah" = "Asia",
  "Riyadh" = "Asia",
  "Baku" = "Asia",
  "Almaty" = "Asia",
  
  # Africa
  "Cape Town" = "Africa",
  "Johannesburg" = "Africa",
  "Cairo" = "Africa",
  "Nairobi" = "Africa",
  
  # Oceania
  "Sydney" = "Oceania",
  "Melbourne" = "Oceania",
  "Brisbane" = "Oceania"
)

# ====== CREATE THE CONTINENT-WISE TABLE ======

# First, create City and Continent columns in df
df_with_continents <- df |>
  mutate(
    Template = factor(template, levels = template_order),
    Sector = factor(Sector, levels = c("Commercial", "Industrial", "Retail")),
    # Map city names
    City = case_when(
      city == "ba" ~ "Buenos Aires",
      city == "ny" ~ "New York",
      city == "hk" ~ "Hong Kong",
      city == "la" ~ "Los Angeles",
      city == "wash" ~ "Washington DC",
      city == "sp" ~ "Sao Paulo",
      city == "cape" ~ "Cape Town",
      city == "mexico" ~ "Mexico City",
      city == "abu" ~ "Abu Dhabi",
      city == "rio" ~ "Rio De Janeiro",
      TRUE ~ tools::toTitleCase(city)
    )
  ) |>
  mutate(
    Continent = city_continent_map[City],
    Continent = factor(Continent,
                       levels = c(
                         "Africa", "Asia", "Central America", "Europe",
                         "North America", "Oceania", "South America"
                       )
    )
  )

# Filter out rows without continent mapping
df_with_continents <- df_with_continents |>
  filter(!is.na(Continent))

# Now create the aggregated continent-sector table
continent_sector_table <- df_with_continents |>
  # Group by Template, Continent, Sector
  group_by(Template, Continent, Sector) |>
  summarize(
    `Net Impact` = mean(net_change, na.rm = TRUE),
    `Gross Impact` = mean(decline, na.rm = TRUE),
    Recovery = mean(recovery, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Round the values
  mutate(
    `Net Impact` = round(`Net Impact`, 1),
    `Gross Impact` = round(`Gross Impact`, 1),
    Recovery = round(Recovery, 1)
  )

# Pivot to wide format for sectors (showing Net Impact)
table_wide <- continent_sector_table |>
  # Select which metric to show (Net Impact as requested)
  select(Template, Continent, Sector, Value = `Net Impact`) |>
  # Pivot sectors to columns
  pivot_wider(
    names_from = Sector,
    values_from = Value,
    values_fill = NA
  ) |>
  # Reorder columns
  select(Template, Continent, Commercial, Industrial, Retail) |>
  # Order by Template order and then Continent
  mutate(
    Template = factor(Template, levels = template_order),
    Continent = factor(Continent,
                       levels = c(
                         "Africa", "Asia", "Central America", "Europe",
                         "North America", "Oceania", "South America"
                       )
    )
  ) |>
  arrange(Template, Continent)

# Format the values with proper signs and handle NAs
table_formatted <- table_wide |>
  mutate(
    across(
      c(Commercial, Industrial, Retail),
      ~ case_when(
        is.na(.) ~ "—", # em dash for missing
        . >= 0 ~ sprintf("+%.1f", .), # positive with plus
        . < 0 ~ sprintf("−%.1f", abs(.)) # negative with minus sign (U+2212)
      )
    )
  )

# Create flextable
ft <- flextable(table_formatted) |>
  set_caption(
    caption = "Table 1| Net sectoral change by resilience archetype and continent. Mean net change in nighttime light (NTL) brightness from pre-lockdown to post-lockdown periods, summarized by resilience archetype, continent, and economic sector. Archetypes are assigned at the city–sector level; continental values represent averages across observed city-sector combinations within each archetype. Negative values (purple shading) indicate sustained dimming in NTL brightness, whereas positive values (green shading) indicate net increases in activity. Cells marked with “–” denote continent-archetype combinations not observed in the dataset.",
    style = "Table Caption",
    autonum = NULL
  ) |>
  # Merge Template cells vertically
  merge_v(j = "Template", part = "body") |>
  # Style the header
  bold(part = "header") |>
  bg(part = "header", bg = "#D8D6C2") |>
  # Alignments
  align(j = c("Commercial", "Industrial", "Retail"), align = "center", part = "all") |>
  align(j = c("Template", "Continent"), align = "left", part = "all") |>
  # Borders
  border_remove() |>
  hline_top(border = fp_border(width = 1, color = "black"), part = "all") |>
  hline_bottom(border = fp_border(width = 1, color = "black"), part = "all") |>
  hline(
    i = ~ !is.na(lag(Template)) & Template != lag(Template),
    border = fp_border(width = 0.5, color = "gray")
  ) |>
  # Font
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all")

# Add conditional formatting for extreme values
# Add conditional formatting for extreme values
for (template_type in template_order) {
  # Get the original numeric values for this template
  template_data <- table_wide |> filter(Template == template_type)
  
  # Find min and max values (ignoring NAs)
  all_values <- unlist(template_data[, c("Commercial", "Industrial", "Retail")])
  all_values <- all_values[!is.na(all_values)]
  
  if (length(all_values) > 0) {
    min_val <- min(all_values)
    max_val <- max(all_values)
    
    # Highlight min (worst) values
    min_rows <- which(
      table_wide$Template == template_type &
        (table_wide$Commercial == min_val |
           table_wide$Industrial == min_val |
           table_wide$Retail == min_val)
    )
    
    # Highlight max (best) values
    max_rows <- which(
      table_wide$Template == template_type &
        (table_wide$Commercial == max_val |
           table_wide$Industrial == max_val |
           table_wide$Retail == max_val)
    )
    
    # Apply formatting
    if (length(min_rows) > 0) {
      ft <- ft |>
        bold(
          i = min_rows, j = c("Commercial", "Industrial", "Retail"),
          bold = TRUE, part = "body"
        )
    }
    
    if (length(max_rows) > 0) {
      ft <- ft |>
        bold(
          i = max_rows, j = c("Commercial", "Industrial", "Retail"),
          bold = TRUE, part = "body"
        )
    }
  }
}

# Get the actual range of your numeric data
numeric_cols <- c("Commercial", "Industrial", "Retail")
all_numeric_vals <- unlist(table_wide[, numeric_cols])
all_numeric_vals <- all_numeric_vals[!is.na(all_numeric_vals)]

# Find range separately for negative and positive values
neg_vals <- all_numeric_vals[all_numeric_vals < 0]
pos_vals <- all_numeric_vals[all_numeric_vals > 0]

# Set ranges
neg_min <- ifelse(length(neg_vals) > 0, min(neg_vals), -1)
neg_max <- ifelse(length(neg_vals) > 0, max(neg_vals), -0.001)
pos_min <- ifelse(length(pos_vals) > 0, min(pos_vals), 0.001)
pos_max <- ifelse(length(pos_vals) > 0, max(pos_vals), 1)

# Create color ramps
red_palette <- colorRampPalette(c("#e5d4e8", "#742881"))(100)
green_palette <- colorRampPalette(c("#d9f1d5", "#1b7939"))(100)

# Function to get the appropriate color
get_rdgr_color <- function(value) {
  if (is.na(value)) {
    return(NULL)
  }
  
  if (value < 0) {
    normalized <- (value - neg_max) / (neg_min - neg_max)
    normalized <- max(0, min(1, normalized))
    color_index <- round(normalized * 99) + 1
    return(red_palette[color_index])
  } else if (value > 0) {
    normalized <- (value - pos_min) / (pos_max - pos_min)
    normalized <- max(0, min(1, normalized))
    color_index <- round(normalized * 99) + 1
    return(green_palette[color_index])
  } else {
    return(NULL)
  }
}

# First, explicitly remove background from ALL numeric cells
for (col_name in numeric_cols) {
  col_index <- which(names(table_formatted) == col_name)
  ft <- bg(ft, j = col_index, bg = "transparent", part = "body")
}

# Then apply colors only to non-NA values
for (col_name in numeric_cols) {
  col_index <- which(names(table_formatted) == col_name)
  
  for (row_num in 1:nrow(table_wide)) {
    original_value <- table_wide[[col_name]][row_num]
    
    # Only apply color if value is not NA
    if (!is.na(original_value)) {
      cell_color <- get_rdgr_color(original_value)
      
      if (!is.null(cell_color)) {
        ft <- bg(ft, i = row_num, j = col_index, bg = cell_color)
      }
    }
  }
}

# Define template colors
template_colors <- c(
  "Chronic decline" = "#D767B2",
  "Partial recovery" = "#E88490",
  "Full recovery" = "#83C8DD",
  "Resilient" = "#859AD5"
)

# Apply colors to the Template column with white text
for (template_name in names(template_colors)) {
  template_rows <- which(table_wide$Template == template_name)
  
  if (length(template_rows) > 0) {
    ft <- ft |>
      bg(
        i = template_rows, j = "Template",
        bg = template_colors[template_name], part = "body"
      ) |>
      color(
        i = template_rows, j = "Template",
        color = "black", part = "body"
      )
  }
}

# Apply alternating background color to Continent column (starting from row 2)
continent_rows <- seq(2, nrow(table_wide), by = 2) # Every other row starting from 2

if (length(continent_rows) > 0) {
  ft <- ft |>
    bg(
      i = continent_rows, j = "Continent",
      bg = "#ECEADF", part = "body"
    )
}

# Autofit and display
ft <- ft |> autofit()

print(ft)

# Save to Word
doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_par("Table 1| Net sectoral change by resilience archetype and continent. Mean net change in nighttime light (NTL) brightness from pre-lockdown to post-lockdown periods, summarized by resilience archetype, continent, and economic sector. Archetypes are assigned at the city–sector level; continental values represent averages across observed city-sector combinations within each archetype. Negative values (purple shading) indicate sustained dimming in NTL brightness, whereas positive values (green shading) indicate net increases in activity. Cells marked with “–” denote continent-archetype combinations not observed in the dataset.", style = "Normal") |>
  body_add_par("Recovery archetypes:", style = "Normal") |>
  body_add_par("• Chronic Decline: Sustained nighttime lights (NTL) dimming throughout study period", style = "Normal") |>
  body_add_par("• Partial Recovery: NTL dimming during lockdown with incomplete post-lockdown recovery", style = "Normal") |>
  body_add_par("• Full Recovery: NTL dimming during lockdown with complete recovery to baseline", style = "Normal") |>
  body_add_par("• Resilient: No significant NTL decline", style = "Normal")

print(doc, target = "path/Table1.docx")