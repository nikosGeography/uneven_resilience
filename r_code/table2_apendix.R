pacman::p_load(dplyr,       # Data manipulation
               purrr,       # Functional programming
               officer,     # Word document creation
               flextable,   # Formatted tables
               tidyselect)

# ============================================
# 1. CITY TO CONTINENT MAPPING (UPDATED)
# ============================================
# Updated according to your specifications
continent_lookup <- c(
  "abu" = "Asia", "almaty" = "Asia", "amsterdam" = "Europe", "athens" = "Europe",
  "ba" = "South America", "baku" = "Asia", "beijing" = "Asia", "berlin" = "Europe",
  "brisbane" = "Oceania", "cairo" = "Africa", "cape" = "Africa", "caracas" = "South America",
  "chicago" = "North America", "dallas" = "North America", "delhi" = "Asia", "dubai" = "Asia",
  "frankfurt" = "Europe", "guangzhou" = "Asia", "hk" = "Asia", "istanbul" = "Asia",
  "jeddah" = "Asia", "johannesburg" = "Africa", "la" = "North America", "lahore" = "Asia",
  "lima" = "South America", "london" = "Europe", "madrid" = "Europe", "manchester" = "Europe",
  "melbourne" = "Oceania", "mexico" = "Central America", "milan" = "Europe", "mumbai" = "Asia",
  "munich" = "Europe", "nairobi" = "Africa", "ny" = "North America", "paris" = "Europe",
  "pune" = "Asia", "rio" = "South America", "riyadh" = "Asia", "rome" = "Europe",
  "santiago" = "South America", "shanghai" = "Asia", "shenzhen" = "Asia", "sp" = "South America",
  "sydney" = "Oceania", "vienna" = "Europe", "wash" = "North America", "wuhan" = "Asia"
)

# ============================================
# 2. DATA LOADING & PREPARATION
# ============================================
prepare_table <- function(file_path, sector_name) {
  read.csv(file_path, na.strings = c("NA", "")) |>
    mutate(Sector = sector_name) |>
    filter(city != "toronto" & city != "warsaw") |>
    filter(!is.na(template)) |>
    mutate(
      decline = period2_midpoint - period1_midpoint,
      recovery = period3_midpoint - period2_midpoint,
      net_change = period3_midpoint - period1_midpoint
    )
}

retail_df <- prepare_table("path/retail_midpoint.csv", "Retail") |> filter(template != "Other")
commercial_df <- prepare_table("path/commercial_midpoint.csv", "Commercial")
industrial_df <- prepare_table("path/industrial_midpoint.csv", "Industrial")

df <- rbind(commercial_df, industrial_df, retail_df)

# ============================================
# 3. ADD CONTINENT, ESSENTIALITY TYPE & FORMAT COLUMNS
# ============================================
table_base <- df |>
  mutate(
    Continent = continent_lookup[city],
    City = tools::toTitleCase(case_when(
      city == "abu" ~ "abu dhabi",
      city == "ba" ~ "buenos aires",
      city == "cape" ~ "cape town",
      city == "hk" ~ "hong kong",
      city == "la" ~ "los angeles",
      city == "ny" ~ "new york",
      city == "rio" ~ "rio de janeiro",
      city == "sp" ~ "sao paulo",
      city == "wash" ~ "washington dc",
      TRUE ~ city
    )),
    # Add Essentiality Type column
    `Essentiality Type` = tools::toTitleCase(as.character(type)),
    Archetype = factor(template, levels = c("Chronic decline", "Partial recovery", "Full recovery", "Resilient")),
    Sector = factor(Sector, levels = c("Commercial", "Industrial", "Retail")),
    Continent = factor(Continent, levels = c("Africa", "Asia", "Central America", "Europe", "North America", "South America", "Oceania"))
  ) |>
  select(
    Archetype,
    Sector,
    Continent,
    City,
    `Essentiality Type`, # New column added here
    `Gross Impact` = decline,
    Recovery = recovery,
    `Net Impact` = net_change
  )

# ============================================
# 4. CREATE STRICT HIERARCHY WITH SUMMARIES (UPDATED)
# ============================================
build_hierarchical_table <- function(data) {
  sorted_data <- data |>
    arrange(Archetype, Sector, Continent, City)

  all_rows <- list()
  archetypes <- levels(data$Archetype)

  for (arch in archetypes) {
    arch_data <- sorted_data |> filter(Archetype == arch)
    sectors <- levels(data$Sector)

    for (sect in sectors) {
      sect_data <- arch_data |> filter(Sector == sect)

      if (nrow(sect_data) > 0) {
        continents <- levels(data$Continent)

        for (cont in continents) {
          cont_data <- sect_data |> filter(Continent == cont)

          if (nrow(cont_data) > 0) {
            all_rows[[length(all_rows) + 1]] <- cont_data
          }
        }

        # Add SECTOR MEAN row
        sector_mean <- sect_data |>
          summarise(
            Archetype = arch,
            Sector = sect,
            Continent = NA_character_,
            City = "Sector Mean",
            `Essentiality Type` = NA_character_, # Add NA for Essentiality Type in summary rows
            `Gross Impact` = mean(`Gross Impact`, na.rm = TRUE),
            Recovery = mean(Recovery, na.rm = TRUE),
            `Net Impact` = mean(`Net Impact`, na.rm = TRUE)
          )
        all_rows[[length(all_rows) + 1]] <- sector_mean
      }
    }

    # Add ARCHETYPE MEAN row
    arch_mean <- arch_data |>
      summarise(
        Archetype = arch,
        Sector = NA_character_,
        Continent = NA_character_,
        City = "Archetype Mean",
        `Essentiality Type` = NA_character_, # Add NA for Essentiality Type in summary rows
        `Gross Impact` = mean(`Gross Impact`, na.rm = TRUE),
        Recovery = mean(Recovery, na.rm = TRUE),
        `Net Impact` = mean(`Net Impact`, na.rm = TRUE)
      )
    all_rows[[length(all_rows) + 1]] <- arch_mean
  }

  bind_rows(all_rows)
}

# Build the hierarchical table
table_hierarchical <- build_hierarchical_table(table_base)

# Round numeric columns
table_final <- table_hierarchical |>
  mutate(across(c(`Gross Impact`, Recovery, `Net Impact`), ~ round(.x, 2)))

# ============================================
# 5. CREATE THE FORMATTED FLEXTABLE (UPDATED)
# ============================================
ft <- flextable(table_final) |>
  align(j = c("Gross Impact", "Recovery", "Net Impact"), align = "center", part = "all") |>
  align(j = c("Archetype", "Sector", "Continent", "City", "Essentiality"), align = "left", part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#D8D6C2") |>
  border_remove() |>
  hline(border = fp_border(width = 0.5, color = "black"), part = "body") |>
  hline_top(border = fp_border(width = 1, color = "black"), part = "header") |>
  hline_bottom(border = fp_border(width = 1, color = "black"), part = "header") |>
  hline_bottom(border = fp_border(width = 1, color = "black"), part = "body") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

# ============================================
# 6. MERGE CELLS IN FIRST TWO COLUMNS
# ============================================
ft <- ft |>
  merge_v(j = "Archetype") |>
  merge_v(j = "Sector")

# ============================================
# 7. APPLY ALTERNATING ROW COLORS (COLUMNS 3+ FROM ROW 2)
# ============================================
is_regular_row <- !(table_final$City %in% c("Sector Mean", "Archetype Mean"))
regular_row_indices <- which(is_regular_row)
alternating_indices <- regular_row_indices[regular_row_indices >= 2 &
  seq_along(regular_row_indices) %% 2 == 0]

if (length(alternating_indices) > 0) {
  ft <- ft |>
    bg(i = alternating_indices, j = 3:8, bg = "#ECEADF", part = "body")
}

# ============================================
# 8. FORMAT SUMMARY ROWS (BOLD ONLY, NO ITALIC)
# ============================================
sector_mean_idx <- which(table_final$City == "Sector Mean")
archetype_mean_idx <- which(table_final$City == "Archetype Mean")

if (length(sector_mean_idx) > 0) {
  ft <- ft |>
    bold(i = sector_mean_idx, bold = TRUE, part = "body")
}

if (length(archetype_mean_idx) > 0) {
  ft <- ft |>
    bold(i = archetype_mean_idx, bold = TRUE, part = "body")
}

# ============================================
# 9. COLOR ARCHETYPE COLUMN
# ============================================
archetype_colors <- c(
  "Chronic decline" = "#D767B2",
  "Partial recovery" = "#E88490",
  "Full recovery" = "#83C8DD",
  "Resilient" = "#859AD5"
)

for (arch in names(archetype_colors)) {
  arch_rows <- which(table_final$Archetype == arch)
  if (length(arch_rows) > 0) {
    ft <- ft |>
      bg(i = arch_rows, j = "Archetype", bg = archetype_colors[arch], part = "body")
  }
}

# ============================================
# 10. COLOR SECTOR COLUMN
# ============================================
sector_colors <- c(
  "Commercial" = "#BDA699",
  "Industrial" = "#A1A1A1",
  "Retail" = "#72C6CC"
)

for (sect in names(sector_colors)) {
  sect_rows <- which(table_final$Sector == sect)
  if (length(sect_rows) > 0) {
    ft <- ft |>
      bg(i = sect_rows, j = "Sector", bg = sector_colors[sect], part = "body")
  }
}

# ============================================
# 11. ADD SEPARATION BETWEEN ARCHETYPE SECTIONS
# ============================================
archetype_starts <- which(!duplicated(table_final$Archetype) & !is.na(table_final$Archetype))
if (length(archetype_starts) > 1) {
  for (i in archetype_starts[-1]) {
    ft <- ft |>
      hline(i = i - 1, border = fp_border(width = 2, color = "black"), part = "body")
  }
}

print(ft)

# Save to Word
save_as_docx(
  "Table A2| City–sector impacts underlying resilience archetype classification. Gross Impact (Lockdown minus Pre-lockdown), Recovery (Post-lockdown minus Lockdown), and Net Impact (Post-lockdown minus Pre-lockdown) in nighttime light (NTL) brightness for each city–sector combination used in the archetype classification. Values are reported for transparency and traceability of the trajectory summaries. Essentiality is categorized as Essential, Non-essential, or Mixed. Summary rows (sector and archetype means) are provided for reference; empty cells indicate that no city–sector combinations were observed for that pairing." = ft,
  path = "path/TableA2.docx"
)
