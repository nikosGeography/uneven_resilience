# Load required packages
pacman::p_load(
  FactoMineR,  # For Multiple Correspondence Analysis (MCA)
  factoextra,   # For MCA visualization
  dplyr,        # Data manipulation
  gridExtra,    # Grid arrangement of plots
  tidyr,        # Data reshaping
  ggforce,      # Advanced ggplot2 features
  tidyplots,    # Enhanced plotting syntax
  patchwork,    # Plot composition and layout
  stats,        # Statistical tests
  svglite       # High-quality SVG export
)

# ==============================================================================
# 1. DATA LOADING AND PREPARATION
# ==============================================================================

# Load sector-specific data files
# Each file contains resilience archetype classifications and business types
commercial_data <- read.csv("commercial_midpoint.csv") |>
  select(city, template, type) |>          # Keep relevant columns
  mutate(sector = "Commercial") |>         # Add sector identifier
  filter(type != "")                        # Remove blank type entries

industrial_data <- read.csv("industrial_midpoint.csv") |>
  select(city, template, type) |>
  mutate(sector = "Industrial") |>
  filter(type != "")

retail_data <- read.csv("retail_midpoint.csv") |>
  select(city, template, type) |>
  mutate(sector = "Retail") |>
  filter(type != "")

# Combine all sectors into a single dataframe
all_df <- bind_rows(
  commercial_data,
  retail_data,
  industrial_data
) |>
  # Convert template to ordered factor for consistent analysis
  mutate(
    template = factor(
      template,
      levels = c(
        "Chronic decline",
        "Partial recovery",
        "Full recovery",
        "Resilient"
      )
    )
  )

# ==============================================================================
# 2. MULTIPLE CORRESPONDENCE ANALYSIS (MCA)
# ==============================================================================

# Prepare data for MCA: sector and type as active variables, template as supplementary
mca_input <- all_df |> select(sector, type, template)

# Perform MCA with template as supplementary variable (not used in dimension construction)
mca_res <- MCA(mca_input, 
               quali.sup = 3,    # Template is column 3 (supplementary)
               graph = FALSE)    # Disable automatic plotting

# Extract individual coordinates for statistical testing
mca_coords <- as.data.frame(mca_res$ind$coord)
mca_coords$archetype <- all_df$template  # Add archetype labels

# Extract and display explained variance for each dimension
eig <- mca_res$eig
cat(sprintf("Dim 1: %.2f%%\n", eig[1, 2]))  # First dimension variance
cat(sprintf("Dim 2: %.2f%%\n", eig[2, 2]))  # Second dimension variance

# ==============================================================================
# 3. STATISTICAL TESTING
# ==============================================================================

# Test 1: Association between archetypes and original categorical variables
# Fisher's exact test for contingency tables
fish_type <- fisher.test(table(all_df$template, all_df$type), 
                         simulate.p.value = TRUE)  # Monte Carlo simulation
fish_sector <- fisher.test(table(all_df$template, all_df$sector), 
                           simulate.p.value = TRUE)

# Test 2: Separation of archetypes along MCA dimensions
# Kruskal-Wallis test (non-parametric ANOVA)
kw_dim1 <- kruskal.test(`Dim 1` ~ archetype, data = mca_coords)
kw_dim2 <- kruskal.test(`Dim 2` ~ archetype, data = mca_coords)

# Print statistical test results
cat("=== STATISTICAL TESTS ===\n\n")
cat("ORIGINAL VARIABLE ASSOCIATIONS (Fisher's Exact):\n")
cat(sprintf(
  "  Essential type → Archetype: p = %.4f %s\n",
  fish_type$p.value,
  ifelse(fish_type$p.value < 0.05, "✓ SIGNIFICANT", "✗ NOT SIGNIFICANT")
))
cat(sprintf(
  "  Sector type → Archetype: p = %.4f %s\n\n",
  fish_sector$p.value,
  ifelse(fish_sector$p.value < 0.05, "✓ SIGNIFICANT", "✗ NOT SIGNIFICANT")
))

cat("MCA DIMENSIONAL SEPARATION (Kruskal-Wallis):\n")
cat(sprintf(
  "  Dim 1 (Essential): χ² = %.2f, p = %.4f %s\n",
  kw_dim1$statistic, kw_dim1$p.value,
  ifelse(kw_dim1$p.value < 0.05, "✓ SIGNIFICANT", "✗ NOT SIGNIFICANT")
))
cat(sprintf(
  "  Dim 2 (Sector): χ² = %.2f, p = %.4f %s\n\n",
  kw_dim2$statistic, kw_dim2$p.value,
  ifelse(kw_dim2$p.value < 0.05, "✓ SIGNIFICANT", "✗ NOT SIGNIFICANT")
))

# ==============================================================================
# 4. VISUALIZATION PREPARATION
# ==============================================================================

# Extract variable coordinates from MCA results
mca_var_coords <- as.data.frame(mca_res$var$coord)
mca_var_coords$variable <- rownames(mca_var_coords)
# Calculate total contribution to first two dimensions
mca_var_coords$contrib <- mca_res$var$contrib[, 1] + mca_res$var$contrib[, 2]

# Define color gradient for contribution heatmap
gradient.cols <- c("#00AFBB", "#E7B800", "#FC4E07")  # Blue → Yellow → Red

# ==============================================================================
# 5. CREATE SUBPLOTS FOR FIGURE 2
# ==============================================================================

# Subplot 1: MCA variable factor map
# Shows contribution of each variable to the MCA dimensions
p1 <- mca_var_coords |>
  tidyplot(x = `Dim 1`, y = `Dim 2`, color = contrib) |>
  add_reference_lines(x = 0, y = 0) |>          # Add zero reference lines
  add_data_points(size = 4, shape = 17) |>      # Triangle markers for variables
  adjust_font(fontsize = 7, family = "Arial") |> # Publication font
  adjust_x_axis_title("Dim 1: Essentiality (32%)", face = "bold") |>
  adjust_y_axis_title("Dim 2: Sector (26%)", face = "bold") |>
  adjust_legend_title("Variance explained (%)") |>
  remove_legend() |>                            # Remove legend for cleaner plot
  remove_x_axis_line() |> remove_y_axis_line() |> # Minimalist axes
  adjust_y_axis(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) |>
  adjust_colors(new_colors = gradient.cols) |>  # Apply contribution gradient
  adjust_theme_details(panel.border = element_rect(color = "black", fill = NA))

# Subplot 2: Distribution of archetypes along Dimension 1 (Essentiality)
p2 <- mca_coords |>
  tidyplot(x = archetype, y = `Dim 1`, color = archetype) |>
  add_violin(alpha = 1, color = "black", trim = FALSE) |>  # Violin plot
  add_boxplot(                                         # Boxplot overlay
    show_whiskers = TRUE,
    show_outliers = FALSE,
    outlier.size = .8,
    whiskers_width = 0,
    alpha = 1,
    fill = "white",
    box_width = .1,
    color = "black"
  ) |>
  remove_title() |> remove_legend() |>                # Clean plot
  adjust_font(fontsize = 7, family = "Arial") |>
  # Apply archetype-specific colors
  tidyplots::adjust_colors(
    new_colors = c("#D767B2", "#E88490", "#83C8DD", "#859AD5")
  ) |>
  adjust_y_axis_title(title = "Dim 1: Essentiality", face = "bold") |>
  remove_x_axis_labels() |>                           # Remove x-axis labels
  remove_x_axis_line() |> remove_y_axis_line() |>
  adjust_theme_details(panel.border = element_rect(color = "black", fill = NA))

# Subplot 3: Distribution of archetypes along Dimension 2 (Sector)
p3 <- mca_coords |>
  tidyplot(x = archetype, y = `Dim 2`, color = archetype) |>
  add_violin(alpha = 1, trim = FALSE, color = "black") |>
  add_boxplot(
    show_whiskers = TRUE,
    show_outliers = FALSE,
    outlier.size = .8,
    whiskers_width = 0,
    alpha = 1,
    fill = "white",
    box_width = .1,
    color = "black"
  ) |>
  remove_title() |> remove_legend() |>
  adjust_font(fontsize = 7, family = "Arial") |>
  tidyplots::adjust_colors(
    new_colors = c("#D767B2", "#E88490", "#83C8DD", "#859AD5")
  ) |>
  adjust_y_axis_title(title = "Dim 2: Sector", face = "bold") |>
  remove_x_axis_labels() |>
  remove_x_axis_line() |> remove_y_axis_line() |>
  adjust_theme_details(panel.border = element_rect(color = "black", fill = NA))

# Subplot 4: Summary table of statistical test results
summary_data <- data.frame(
  Test = c("Original Space\n(Fisher)", "MCA Space\n(Kruskal-Wallis)"),
  Essential_p = c(fish_type$p.value, kw_dim1$p.value),
  Sector_p = c(fish_sector$p.value, kw_dim2$p.value)
) |>
  pivot_longer(
    cols = c(Essential_p, Sector_p),
    names_to = "Dimension", values_to = "p_value"
  ) |>
  mutate(
    Dimension = gsub("_p", "", Dimension),  # Clean dimension names
    Significant = ifelse(p_value < 0.05, "Significant", "Not Significant")
  )

# Create heatmap-style summary table
p4 <- ggplot(summary_data, aes(x = Dimension, y = Test, fill = Significant)) +
  geom_tile(color = "white", linewidth = 1) +  # Tiles for each test-dimension pair
  geom_text(
    aes(label = ifelse(p_value < 0.001,
                       "p = 0.001",                    # Format very small p-values
                       sprintf("p = %.3f", p_value)    # Standard p-value formatting
    )),
    fontface = "bold",
    size = 7,
    size.unit = "pt",
    family = "Arial"
  ) +
  # Color coding for significance
  scale_fill_manual(
    values = c("Not Significant" = "#b8b8b8", "Significant" = "#DD4C4C"),
    breaks = c("Significant", "Not Significant")
  ) +
  labs(x = "Feature Dimension", y = "Test Type") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "none",  # Remove legend (colors are self-explanatory)
    axis.title.x = element_text(size = 7, family = "Arial", color = "black"),
    axis.title.y = element_blank(),  # No y-axis title
    axis.text.x = element_text(size = 7, family = "Arial", color = "black"),
    axis.text.y = element_text(size = 7, family = "Arial", color = "black"),
    axis.ticks.length = unit(0, "pt")  # Remove tick marks
  )

# ==============================================================================
# 6. COMPOSE FINAL FIGURE
# ==============================================================================

# Combine plots using patchwork syntax:
# Top row: MCA factor map (p1)
# Bottom row: Three subplots side by side (p2, p3, p4)
# Height ratio: 2:1 (top:bottom)
combined_plot <- p1 / (p2 | p3 | p4) +
  plot_layout(heights = c(2, 1))

# Display the combined plot
combined_plot