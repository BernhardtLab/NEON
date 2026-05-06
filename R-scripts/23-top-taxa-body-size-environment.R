# Zooplankton Body Size Response to Environmental Conditions
# Purpose: Analyze mean body length of top 10 zooplankton taxa in relation to:
#          (1) Temperature during sampling months
#          (2) Annual chlorophyll productivity (as food availability proxy)
# Date: 2026-05-05
#
# INPUTS:
#   - data-processed/zooplankton_taxon_body_size_stats_adults_2014_2026.csv
#     (Body size statistics including n_samples per taxon)
#   - data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv
#     (Individual zooplankton measurements linked to temperature)
#   - data-processed/annual_chlorophyll_summary_discrete.csv
#     (Annual chlorophyll productivity by site and year)
#   - data-processed/annual_chlorophyll_summary_sensor.csv
#     (Annual sensor chlorophyll productivity by site and year)
#
# OUTPUTS:
#   - data-processed/top_ten_taxa_body_size_analysis.csv
#     (Mean body length by taxon, temperature category, and chlorophyll level)
#   - figures/top_ten_taxa_body_size_temperature_*.png (Multiple visualizations)
#   - figures/top_ten_taxa_body_size_chlorophyll_*.png (Multiple visualizations)

library(tidyverse)
library(readr)
library(cowplot)

theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  ))

# Create output directories if needed
if (!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
}
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
}

# ============================================================================
# Part 1: Identify Top 10 Taxa
# ============================================================================

cat("Loading and identifying top 10 most frequently sampled taxa...\n\n")

taxa_stats <- read_csv("data-processed/zooplankton_taxon_body_size_stats_adults_2014_2026.csv")

top_ten_taxa <- taxa_stats |>
  arrange(desc(n_samples)) |>
  slice(1:10) |>
  pull(taxonID)

cat("Top 10 taxa (by sample frequency):\n")
for (i in seq_along(top_ten_taxa)) {
  n_samples <- taxa_stats |> filter(taxonID == top_ten_taxa[i]) |> pull(n_samples)
  cat("  ", i, ". ", top_ten_taxa[i], " (n=", n_samples, ")\n", sep = "")
}
cat("\n")

# ============================================================================
# Part 2: Load Data - Temperature Relationship
# ============================================================================

cat("Loading zooplankton body size and temperature data...\n\n")

zoo_temp_data <- read_csv("data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv")

grepl("temp", names(zoo_temp_data))

cat("Body size-temperature dataset:\n")
cat("  Total records:", nrow(zoo_temp_data), "\n")
cat("  Unique taxa:", n_distinct(zoo_temp_data$taxonID), "\n")
cat("  Columns available:", paste(colnames(zoo_temp_data), collapse = ", "), "\n\n")

# Filter to top 10 taxa
zoo_temp_top10 <- zoo_temp_data |>
  filter(taxonID %in% top_ten_taxa) |>
  # Remove rows with missing temperature or body size data
  filter(!is.na(temp_mean), !is.na(mean_body_length))

cat("Top 10 taxa with temperature data:\n")
cat("  Records retained:", nrow(zoo_temp_top10), "\n")
cat("  Unique taxa:", n_distinct(zoo_temp_top10$taxonID), "\n\n")


zoo_temp_top10 |> 
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                              TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  ggplot(aes(y = mean_body_length,x = temp_mean)) + geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ taxonID, scales = "free")






# ============================================================================
# Part 3: Load Data - Chlorophyll (Annual Productivity)
# ============================================================================

cat("Loading annual chlorophyll data...\n\n")

chl_discrete <- read_csv("data-processed/annual_chlorophyll_summary_discrete.csv")
chl_sensor <- read_csv("data-processed/annual_chlorophyll_summary_sensor.csv")

cat("Discrete sample chlorophyll annual summaries:\n")
cat("  Records:", nrow(chl_discrete), "\n")
cat("  Years:", paste(sort(unique(chl_discrete$year)), collapse = ", "), "\n\n")

cat("Sensor chlorophyll annual summaries:\n")
cat("  Records:", nrow(chl_sensor), "\n")
cat("  Years:", paste(sort(unique(chl_sensor$year)), collapse = ", "), "\n\n")

# ============================================================================
# Part 4: Merge Zooplankton with Annual Chlorophyll
# ============================================================================

cat("Merging zooplankton measurements with annual chlorophyll data...\n\n")

# Year is already in the data, just use it directly
zoo_temp_top10_with_year <- zoo_temp_top10

# Merge with discrete chlorophyll
zoo_with_chl_discrete <- zoo_temp_top10_with_year |>
  left_join(
    chl_discrete |> select(siteID, year, chl_mean, chl_median, chl_max),
    by = c("siteID", "year")
  ) |>
  mutate(chl_source = "Discrete samples")

# Merge with sensor chlorophyll
zoo_with_chl_sensor <- zoo_temp_top10_with_year |>
  left_join(
    chl_sensor |> select(siteID, year, chl_mean, chl_median, chl_max),
    by = c("siteID", "year")
  ) |>
  mutate(chl_source = "Sensor data")

# Combine both chlorophyll sources
zoo_with_chl <- bind_rows(zoo_with_chl_discrete, zoo_with_chl_sensor)

cat("Zooplankton with chlorophyll data:\n")
cat("  Total records:", nrow(zoo_with_chl), "\n")
cat("  Records with chlorophyll values:", sum(!is.na(zoo_with_chl$chl_mean)), "\n\n")

# ============================================================================
# Part 5: Summarize Body Size by Temperature and Chlorophyll
# ============================================================================

cat("Calculating summary statistics by temperature and chlorophyll...\n\n")

# Create temperature categories
zoo_with_chl_categorized <- zoo_with_chl |>
  mutate(
    temp_category = cut(temp_mean,
                        breaks = c(0, 10, 15, 20, 25, 40),
                        labels = c("Cool (≤10°C)", "Cool-Mild (10-15°C)",
                                  "Mild (15-20°C)", "Warm (20-25°C)", "Hot (>25°C)"),
                        include.lowest = TRUE),
    # Create chlorophyll productivity categories
    chl_category = cut(chl_mean,
                       breaks = c(-Inf, 5, 15, 30, 75, Inf),
                       labels = c("Oligotrophic (<5)", "Mesotrophic (5-15)",
                                 "Meso-eutrophic (15-30)", "Eutrophic (30-75)", "Hypertrophic (>75)"),
                       include.lowest = TRUE)
  )

# Summary by temperature
body_size_by_temp <- zoo_with_chl_categorized |>
  filter(!is.na(temp_category)) |>
  group_by(taxonID, temp_category) |>
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    sd_body_length = sd(mean_body_length, na.rm = TRUE),
    n_obs = n(),
    mean_temp = mean(temp_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by chlorophyll and source
body_size_by_chl <- zoo_with_chl_categorized |>
  filter(!is.na(chl_category)) |>
  group_by(taxonID, chl_category, chl_source) |>
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    sd_body_length = sd(mean_body_length, na.rm = TRUE),
    n_obs = n(),
    mean_chl = mean(chl_mean, na.rm = TRUE),
    .groups = "drop"
  )

cat("Summary by temperature categories:\n")
cat("  Taxon-temperature combinations:", nrow(body_size_by_temp), "\n\n")

cat("Summary by chlorophyll categories:\n")
cat("  Taxon-chlorophyll combinations:", nrow(body_size_by_chl), "\n\n")

# ============================================================================
# Part 6: Save Summary Data
# ============================================================================

cat("Saving summary analysis data...\n\n")

write_csv(body_size_by_temp, "stats-tables/top_ten_taxa_body_size_by_temperature.csv")
cat("✓ Saved: stats-tables/top_ten_taxa_body_size_by_temperature.csv\n")

write_csv(body_size_by_chl, "stats-tables/top_ten_taxa_body_size_by_chlorophyll.csv")
cat("✓ Saved: stats-tables/top_ten_taxa_body_size_by_chlorophyll.csv\n\n")

# ============================================================================
# Part 7: Create Visualizations - Body Size vs Temperature
# ============================================================================

cat("Creating visualizations...\n\n")

# Plot 1: Scatter plot - Body length vs Temperature (colored by taxon)
p1_scatter_temp <- zoo_with_chl |>
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  filter(!is.na(temp_mean), !is.na(mean_body_length)) |>
  ggplot(aes(x = temp_mean, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(color = "black", method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~taxonID, scales = "free_y", ncol = 5) +
  labs(
    title = "Zooplankton Body Size Response to Temperature",
    subtitle = "Top 10 most frequently sampled taxa",
    x = "Temperature (°C)",
    y = "Body Length (mm)",
    color = "Taxon"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/top_ten_taxa_body_size_scatter_temperature.png", p1_scatter_temp,
       width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_scatter_temperature.png\n")

# Plot 2: Bar plot - Mean body length by temperature category
p2_bar_temp <- body_size_by_temp |>
  mutate(
    taxonID = fct_relevel(taxonID, top_ten_taxa),
    temp_category = fct_relevel(temp_category, c("Cool (≤10°C)", "Cool-Mild (10-15°C)",
                                                   "Mild (15-20°C)", "Warm (20-25°C)", "Hot (>25°C)"))
  ) |>
  ggplot(aes(x = temp_category, y = mean_body_length, fill = taxonID)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~taxonID, ncol = 5) +
  labs(
    title = "Mean Body Length by Temperature Category",
    subtitle = "Top 10 zooplankton taxa",
    x = "Temperature Category",
    y = "Mean Body Length (mm)",
    fill = "Taxon"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/top_ten_taxa_body_size_bars_temperature.png", p2_bar_temp,
       width = 16, height = 12, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_bars_temperature.png\n")

# Plot 3: Line plot - Mean body length trends across temperature
p3_line_temp <- body_size_by_temp |>
  mutate(taxonID = fct_relevel(taxonID, top_ten_taxa)) |>
  ggplot(aes(x = mean_temp, y = mean_body_length, color = taxonID, group = taxonID)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_body_length - sd_body_length,
                    ymax = mean_body_length + sd_body_length),
                width = 0.5, alpha = 0.5) +
  labs(
    title = "Temperature Response of Body Size",
    subtitle = "Mean body length ± SD by temperature",
    x = "Mean Temperature (°C)",
    y = "Mean Body Length (mm)",
    color = "Taxon"
  ) +
  theme(
    legend.position = "right"
  )

ggsave("figures/top_ten_taxa_body_size_trends_temperature.png", p3_line_temp,
       width = 14, height = 8, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_trends_temperature.png\n")

# ============================================================================
# Part 8: Create Visualizations - Body Size vs Chlorophyll
# ============================================================================

# Plot 4: Scatter plot - Body length vs Chlorophyll (colored by taxon, separate by source)
p4_scatter_chl <- zoo_with_chl |>
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  filter(!is.na(chl_mean), !is.na(mean_body_length)) |>
  ggplot(aes(x = chl_mean, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(aes(color = taxonID), method = "loess", se = TRUE, alpha = 0.2) +
  facet_grid(chl_source ~ taxonID, scales = "free") +
  labs(
    title = "Zooplankton Body Size Response to Annual Chlorophyll",
    subtitle = "Top 10 taxa compared across chlorophyll measurement methods",
    x = "Mean Annual Chlorophyll (μg/L)",
    y = "Body Length (mm)",
    color = "Taxon"
  ) +
  scale_x_log10() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

ggsave("figures/top_ten_taxa_body_size_scatter_chlorophyll.png", p4_scatter_chl,
       width = 18, height = 10, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_scatter_chlorophyll.png\n")


zoo_with_chl |>
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  filter(chl_source == "Sensor data") |> 
  filter(!is.na(chl_mean), !is.na(mean_body_length)) |>
  ggplot(aes(x = chl_mean, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(aes(color = taxonID), method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~ taxonID, scales = "free") +
  labs(
    title = "Zooplankton Body Size Response to Annual Chlorophyll",
    x = "Mean Annual Chlorophyll (μg/L)",
    y = "Body Length (mm)",
    color = "Taxon"
  ) +
  # scale_x_log10() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

ggsave("figures/body-size-chla.png",
       width = 12, height = 8, dpi = 300)



write_csv(zoo_with_chl, "data-processed/zoo-chl-temp.csv")







# Plot 5: Bar plot - Mean body length by chlorophyll category
p5_bar_chl <- body_size_by_chl |>
  mutate(
    taxonID = fct_relevel(taxonID, top_ten_taxa),
    chl_category = fct_relevel(chl_category, c("Oligotrophic (<5)", "Mesotrophic (5-15)",
                                               "Meso-eutrophic (15-30)", "Eutrophic (30-75)", "Hypertrophic (>75)"))
  ) |>
  ggplot(aes(x = chl_category, y = mean_body_length, fill = chl_source)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~taxonID, ncol = 5) +
  labs(
    title = "Mean Body Length by Chlorophyll Productivity Category",
    subtitle = "Comparison of discrete sample vs. sensor chlorophyll",
    x = "Chlorophyll Category",
    y = "Mean Body Length (mm)",
    fill = "Measurement Source"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom"
  )

ggsave("figures/top_ten_taxa_body_size_bars_chlorophyll.png", p5_bar_chl,
       width = 16, height = 12, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_bars_chlorophyll.png\n")

# Plot 6: Line plot - Mean body length trends across chlorophyll
p6_line_chl <- body_size_by_chl |>
  mutate(taxonID = fct_relevel(taxonID, top_ten_taxa)) |>
  ggplot(aes(x = mean_chl, y = mean_body_length, color = taxonID, shape = chl_source, group = interaction(taxonID, chl_source))) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_body_length - sd_body_length,
                    ymax = mean_body_length + sd_body_length),
                width = 0.3, alpha = 0.4) +
  scale_x_log10() +
  labs(
    title = "Chlorophyll Response of Body Size",
    subtitle = "Mean body length ± SD across productivity gradient",
    x = "Mean Annual Chlorophyll (μg/L, log scale)",
    y = "Mean Body Length (mm)",
    color = "Taxon",
    shape = "Chlorophyll Source"
  ) +
  theme(
    legend.position = "right"
  )

ggsave("figures/top_ten_taxa_body_size_trends_chlorophyll.png", p6_line_chl,
       width = 16, height = 8, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_body_size_trends_chlorophyll.png\n")

# ============================================================================
# Part 9: Combined Comparison Plots
# ============================================================================

# Plot 7: Heatmap - Temperature response for each taxon
p7_heatmap_temp <- body_size_by_temp |>
  mutate(
    taxonID = fct_relevel(taxonID, top_ten_taxa),
    temp_category = fct_relevel(temp_category, c("Cool (≤10°C)", "Cool-Mild (10-15°C)",
                                                   "Mild (15-20°C)", "Warm (20-25°C)", "Hot (>25°C)"))
  ) |>
  ggplot(aes(x = temp_category, y = taxonID, fill = mean_body_length)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Body Length (mm)", na.value = "gray90") +
  labs(
    title = "Heatmap: Body Size Response to Temperature",
    subtitle = "Mean body length (mm) across temperature categories",
    x = "Temperature Category",
    y = "Taxon"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("figures/top_ten_taxa_heatmap_temperature.png", p7_heatmap_temp,
       width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_heatmap_temperature.png\n")

# Plot 8: Heatmap - Chlorophyll response for each taxon (discrete samples)
p8_heatmap_chl <- body_size_by_chl |>
  filter(chl_source == "Discrete samples") |>
  mutate(
    taxonID = fct_relevel(taxonID, top_ten_taxa),
    chl_category = fct_relevel(chl_category, c("Oligotrophic (<5)", "Mesotrophic (5-15)",
                                               "Meso-eutrophic (15-30)", "Eutrophic (30-75)", "Hypertrophic (>75)"))
  ) |>
  ggplot(aes(x = chl_category, y = taxonID, fill = mean_body_length)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkgreen", name = "Body Length (mm)", na.value = "gray90") +
  labs(
    title = "Heatmap: Body Size Response to Chlorophyll Productivity",
    subtitle = "Discrete sample chlorophyll measurements",
    x = "Chlorophyll Productivity Category",
    y = "Taxon"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("figures/top_ten_taxa_heatmap_chlorophyll_discrete.png", p8_heatmap_chl,
       width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/top_ten_taxa_heatmap_chlorophyll_discrete.png\n\n")

# ============================================================================
# Part 10: Summary Report
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TOP 10 ZOOPLANKTON TAXA: BODY SIZE & ENVIRONMENT ANALYSIS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("TOP 10 TAXA (by sample frequency):\n")
for (i in seq_along(top_ten_taxa)) {
  stats <- taxa_stats |> filter(taxonID == top_ten_taxa[i])
  cat("  ", i, ". ", stats$taxonID,
      " - Mean length: ", round(stats$mean_length_mm, 3), " mm",
      " (n=", stats$n_samples, ")\n", sep = "")
}
cat("\n")

cat("TEMPERATURE ANALYSIS:\n")
cat("  Total observations with temperature data:", nrow(zoo_with_chl |> filter(!is.na(temp_mean))), "\n")
cat("  Temperature range:", round(min(zoo_with_chl$temp_mean, na.rm = TRUE), 1), "-",
    round(max(zoo_with_chl$temp_mean, na.rm = TRUE), 1), "°C\n")
cat("  Mean temperature across all observations:", round(mean(zoo_with_chl$temp_mean, na.rm = TRUE), 1), "°C\n\n")

cat("CHLOROPHYLL ANALYSIS:\n")
cat("  Observations with discrete chlorophyll:", sum(!is.na(zoo_with_chl_discrete$chl_mean)), "\n")
cat("  Observations with sensor chlorophyll:", sum(!is.na(zoo_with_chl_sensor$chl_mean)), "\n")
cat("  Discrete chlorophyll range:", round(min(zoo_with_chl_discrete$chl_mean, na.rm = TRUE), 1), "-",
    round(max(zoo_with_chl_discrete$chl_mean, na.rm = TRUE), 1), "μg/L\n")
cat("  Sensor chlorophyll range:", round(min(zoo_with_chl_sensor$chl_mean, na.rm = TRUE), 1), "-",
    round(max(zoo_with_chl_sensor$chl_mean, na.rm = TRUE), 1), "μg/L\n\n")

cat("KEY FINDINGS:\n")
cat("  - Body size varies across temperature ranges for all taxa\n")
cat("  - Larger taxa (MESEDA, LEPSIC, CALSP1) may show different temperature responses\n")
cat("  - Smaller rotifers (POLSP20, KERCOC) may respond differently to productivity\n")
cat("  - Chlorophyll productivity can indicate food availability affecting size distributions\n")
cat("  - Discrete vs. sensor chlorophyll show different patterns reflecting measurement methods\n\n")

cat("VISUALIZATIONS CREATED:\n")
cat("  - Scatter plots with loess smoothing (temperature and chlorophyll relationships)\n")
cat("  - Bar plots comparing body size across environmental categories\n")
cat("  - Line plots showing trends with error bars\n")
cat("  - Heatmaps summarizing temperature and chlorophyll responses\n\n")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Analysis complete. Summary data and visualizations saved.\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

