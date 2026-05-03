# Merge Monthly Temperature with Zooplankton Body Size Data
# Purpose: Combine adults-only body size data with monthly temperature for analysis
# Date: 2026-05-02
#
# INPUTS:
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Raw daily temperature data from NEON)
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Adults-only body size summary from script 05)
#
# OUTPUTS:
#   - data-processed/temperature_monthly_summary.csv
#     (Monthly temperature aggregates by site)
#   - data-processed/body_size_monthly_summary.csv
#     (Monthly body size aggregates by site)
#   - data-processed/body_size_temperature_merged.csv
#     (Full merged dataset with all columns)
#   - data-processed/body_size_temperature_analysis.csv
#     (Analysis-ready dataset with key variables only, used by script 16)
#   - figures/temperature_*.png (5 temperature visualizations)

library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)

# ============================================================================
# Part 1: Load Data
# ============================================================================

cat("Loading data...\n\n")

# Load temperature data (raw, will create monthly summaries)
temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

# Load adults-only body size summary
body_size <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv")

cat("Data loaded:\n")
cat("  Temperature records:", nrow(temp_raw), "\n")
cat("  Body size records:", nrow(body_size), "\n\n")

# ============================================================================
# Part 2: Create Monthly Temperature Summary
# ============================================================================

cat("Creating monthly temperature summary...\n\n")

temp_monthly <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) |>
  group_by(siteID, year, month) |>
  summarise(
    temp_mean_monthly = mean(meanTemp, na.rm = TRUE),
    temp_sd_monthly = sd(meanTemp, na.rm = TRUE),
    temp_max_monthly = max(maxTemp, na.rm = TRUE),
    temp_min_monthly = min(minTemp, na.rm = TRUE),
    n_days_temp = n(),
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Monthly temperature summary created:\n")
cat("  Shape:", nrow(temp_monthly), "site-month combinations\n")
cat("  Columns:", paste(colnames(temp_monthly), collapse = ", "), "\n\n")

# ============================================================================
# Part 3: Prepare Body Size Data for Merging
# ============================================================================

cat("Preparing body size data for merging...\n\n")

body_size_for_merge <- body_size |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  select(
    siteID, namedLocation, year, month, collectDate,
    taxonID,
    mean_body_length, max_body_length,
    mean_body_width, count_per_liter, sampler_type,
    aquatic_site_type
  ) |>
  arrange(siteID, year, month, collectDate)

# Create summary by site-year-month
body_size_monthly <- body_size_for_merge |>
  group_by(siteID, namedLocation, year, month) |>
  summarise(
    n_samples = n(),
    n_taxa = n_distinct(taxonID),
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    sd_body_length = sd(mean_body_length, na.rm = TRUE),
    max_body_length = mean(max_body_length, na.rm = TRUE),
    mean_body_width = mean(mean_body_width, na.rm = TRUE),
    mean_count_per_liter = mean(count_per_liter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Body size monthly summary created:\n")
cat("  Shape:", nrow(body_size_monthly), "site-month combinations\n")
cat("  Columns:", paste(colnames(body_size_monthly), collapse = ", "), "\n\n")

# ============================================================================
# Part 4: Merge Temperature and Body Size
# ============================================================================

cat("Merging temperature and body size data...\n\n")

merged_data <- body_size_monthly |>
  left_join(
    temp_monthly,
    by = c("siteID", "year", "month")
  ) |>
  arrange(siteID, year, month)

cat("Merged dataset created:\n")
cat("  Shape:", nrow(merged_data), "observations x", ncol(merged_data), "variables\n\n")

# Check for missing temperature data
na_temp <- sum(is.na(merged_data$temp_mean_monthly))
pct_na <- round(na_temp / nrow(merged_data) * 100, 1)

cat("Data completeness:\n")
cat("  Records with temperature data:", nrow(merged_data) - na_temp, "out of", nrow(merged_data), "\n")
cat("  Coverage:", 100 - pct_na, "%\n\n")

if (pct_na > 0) {
  cat("Records missing temperature data (before 2017-08-19):\n")
  missing_temp <- merged_data |>
    filter(is.na(temp_mean_monthly)) |>
    group_by(siteID, year) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(siteID, year)
  print(missing_temp)
  cat("\n")
}

# ============================================================================
# Part 5: Summary Statistics
# ============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("MERGED DATA SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Body Size (Mean Length by Site):\n")
body_size_by_site <- merged_data |>
  group_by(siteID, namedLocation) |>
  summarise(
    n_observations = n(),
    mean_length_avg = round(mean(mean_body_length, na.rm = TRUE), 4),
    mean_length_sd = round(sd(mean_body_length, na.rm = TRUE), 4),
    mean_length_range = paste0(
      round(min(mean_body_length, na.rm = TRUE), 4), " - ",
      round(max(mean_body_length, na.rm = TRUE), 4)
    ),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(body_size_by_site)

cat("\n\nTemperature (Mean Monthly by Site):\n")
temp_by_site <- merged_data |>
  group_by(siteID, namedLocation) |>
  summarise(
    n_observations = n(),
    temp_avg = round(mean(temp_mean_monthly, na.rm = TRUE), 2),
    temp_sd = round(sd(temp_mean_monthly, na.rm = TRUE), 2),
    temp_range = paste0(
      round(min(temp_min_monthly, na.rm = TRUE), 2), " - ",
      round(max(temp_max_monthly, na.rm = TRUE), 2)
    ),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(temp_by_site)

# ============================================================================
# Part 6: Create Analysis-Ready Dataset
# ============================================================================

cat("\n\nPreparing analysis-ready dataset...\n\n")

# Create final dataset with just the key variables
analysis_data <- merged_data |>
  select(
    siteID, namedLocation, year, month,
    n_samples, n_taxa,
    mean_body_length, sd_body_length, max_body_length,
    mean_body_width, mean_count_per_liter,
    temp_mean_monthly, temp_sd_monthly,
    temp_max_monthly, temp_min_monthly,
    n_days_temp
  ) |>
  # Add a date column for reference (using first day of month)
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    .after = month
  ) |>
  arrange(siteID, year, month) |>
  # Add a flag for records with complete temperature data
  mutate(has_temperature = !is.na(temp_mean_monthly))

cat("Analysis-ready dataset:\n")
cat("  Shape:", nrow(analysis_data), "observations x", ncol(analysis_data), "variables\n\n")

# Show first few rows by site
cat("Sample of data (first 3 months per site):\n\n")
for (site in unique(analysis_data$siteID)) {
  site_data <- analysis_data |>
    filter(siteID == site) |>
    head(3)
  cat(site, ":\n")
  print(site_data |> select(siteID, date, mean_body_length, temp_mean_monthly, n_samples))
  cat("\n")
}

# ============================================================================
# Part 7: Save Datasets
# ============================================================================

cat("\nSaving datasets...\n\n")

write_csv(temp_monthly, "data-processed/temperature_monthly_summary.csv")
cat("✓ Saved: data-processed/temperature_monthly_summary.csv\n")

write_csv(body_size_monthly, "data-processed/body_size_monthly_summary.csv")
cat("✓ Saved: data-processed/body_size_monthly_summary.csv\n")

write_csv(merged_data, "data-processed/body_size_temperature_merged.csv")
cat("✓ Saved: data-processed/body_size_temperature_merged.csv\n")

write_csv(analysis_data, "data-processed/body_size_temperature_analysis.csv")
cat("✓ Saved: data-processed/body_size_temperature_analysis.csv\n")

# ============================================================================
# Part 8: Ready for Analysis
# ============================================================================

# ============================================================================
# Part 9: Visualize Temperature Across Sites
# ============================================================================

cat("Creating temperature visualizations...\n\n")

# Set up theme
theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  ))

# Plot 1: Temperature distribution by site (boxplot)
p1_temp_dist <- analysis_data |>
  filter(has_temperature) |>
  ggplot(aes(x = reorder(siteID, temp_mean_monthly, FUN = median), y = temp_mean_monthly, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  labs(
    title = "Temperature Distribution Across Lake Sites",
    x = "Site ID",
    y = "Monthly Mean Temperature (°C)",
    subtitle = "Box plots show monthly temperature range (2017-2024)"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/temperature_distribution_by_site.png", p1_temp_dist, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/temperature_distribution_by_site.png\n")

# Plot 2: Temperature time series by site
p2_temp_timeseries <- analysis_data |>
  filter(has_temperature) |>
  ggplot(aes(x = date, y = temp_mean_monthly, color = siteID, group = siteID)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_point(alpha = 0.5, size = 1.5) +
  facet_wrap(~siteID, scales = "free_y", ncol = 2) +
  labs(
    title = "Temperature Time Series by Site",
    x = "Date",
    y = "Monthly Mean Temperature (°C)",
    subtitle = "2017-2024 data"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("figures/temperature_timeseries_by_site.png", p2_temp_timeseries, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/temperature_timeseries_by_site.png\n")

# Plot 3: Seasonal temperature patterns (violin plot by month)
p3_temp_seasonal <- analysis_data |>
  filter(has_temperature) |>
  mutate(month_name = factor(month.abb[month], levels = month.abb)) |>
  ggplot(aes(x = month_name, y = temp_mean_monthly, fill = siteID)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.15, alpha = 0.7) +
  facet_wrap(~siteID, ncol = 2) +
  labs(
    title = "Seasonal Temperature Patterns by Site",
    x = "Month",
    y = "Monthly Mean Temperature (°C)",
    subtitle = "Violin plots show distribution across all years"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("figures/temperature_seasonal_patterns.png", p3_temp_seasonal, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/temperature_seasonal_patterns.png\n")

# Plot 4: Temperature variability (SD) by site
p4_temp_variability <- analysis_data |>
  filter(has_temperature) |>
  ggplot(aes(x = reorder(siteID, temp_sd_monthly, FUN = median), y = temp_sd_monthly, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  labs(
    title = "Temperature Variability by Site",
    x = "Site ID",
    y = "Monthly Temperature Std Dev (°C)",
    subtitle = "Shows within-month temperature variation"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/temperature_variability_by_site.png", p4_temp_variability, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/temperature_variability_by_site.png\n")

# Plot 5: Temperature min/max range by site
p5_temp_range <- analysis_data |>
  filter(has_temperature) |>
  ggplot(aes(x = reorder(siteID, temp_max_monthly, FUN = median), fill = siteID)) +
  geom_errorbar(aes(ymin = temp_min_monthly, ymax = temp_max_monthly),
                width = 0.2, alpha = 0.7, size = 1) +
  geom_point(aes(y = temp_mean_monthly), color = "black", size = 2, alpha = 0.6) +
  facet_wrap(~year, ncol = 3) +
  labs(
    title = "Temperature Min-Max Range by Site and Year",
    x = "Site ID",
    y = "Temperature (°C)",
    subtitle = "Lines show min-max, points show monthly mean"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("figures/temperature_minmax_range.png", p5_temp_range, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/temperature_minmax_range.png\n")

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("DATA READY FOR ANALYSIS!\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("You now have a monthly-level dataset combining:\n\n")
cat("BODY SIZE VARIABLES:\n")
cat("  - mean_body_length: average zooplankton body length (mm)\n")
cat("  - sd_body_length: variability in body size\n")
cat("  - max_body_length: average maximum body length\n")
cat("  - mean_count_per_liter: zooplankton abundance\n")
cat("  - n_samples: number of samples in that month\n\n")

cat("TEMPERATURE VARIABLES:\n")
cat("  - temp_mean_monthly: average monthly temperature (°C)\n")
cat("  - temp_sd_monthly: variability within the month\n")
cat("  - temp_max_monthly: highest temperature recorded\n")
cat("  - temp_min_monthly: lowest temperature recorded\n\n")

cat("KEY DATASET: data-processed/body_size_temperature_analysis.csv\n")
cat("  Use this for your hypothesis testing!\n\n")

cat("NEXT STEPS:\n")
cat("  1. Explore correlations between temperature and body size\n")
cat("  2. Fit regression models (body size ~ temperature)\n")
cat("  3. Compare across sites and seasons\n")
cat("  4. Now add food supply data when ready!\n")
