# Phytoplankton Chlorophyll Sensor Data Analysis
# Purpose: Analyze daily chlorophyll sensor measurements during zooplankton sampling months
#          Compare sensor-based chlorophyll patterns with discrete sample data (Script 20)
# Date: 2026-05-04
#
# INPUTS:
#   - data-raw/NEON_daily_summaries/NEON_daily_sensor_chlorophyll_stats.csv
#     (Daily chlorophyll sensor measurements: meanChlorophyll, max, min)
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Body size data with collection dates to identify sampling months)
#
# OUTPUTS:
#   - data-processed/sensor_chlorophyll_during_zooplankton_sampling.csv
#     (Daily sensor chlorophyll data filtered to zooplankton sampling months)
#   - stats-tables/sensor_chlorophyll_summary_by_site_month.csv
#     (Monthly sensor chlorophyll summaries by site)
#   - figures/sensor_chlorophyll_*.png (5+ visualization figures)
#
# NOTE: Sensor chlorophyll = continuous daily measurements
#       vs. Script 20 discrete samples = episodic sample-based measurements

library(tidyverse)
library(readr)
library(lubridate)
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
# Part 1: Load Data
# ============================================================================

cat("Loading sensor chlorophyll data...\n\n")

sensor_chl_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_sensor_chlorophyll_stats.csv")

cat("Raw sensor chlorophyll data:\n")
cat("  Records:", nrow(sensor_chl_raw), "\n")
cat("  Sites:", n_distinct(sensor_chl_raw$siteID), "\n")
cat("  Date range:", min(sensor_chl_raw$date), "to", max(sensor_chl_raw$date), "\n\n")

# Load zooplankton body size data to identify sampling months
cat("Loading zooplankton body size data (to identify sampling months)...\n\n")

zoo_body_size <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv")

# Get the unique months when zooplankton are sampled (by site and month)
zoo_sampling_months <- zoo_body_size |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  select(siteID, year, month) |>
  distinct() |>
  arrange(siteID, year, month)

cat("Zooplankton sampling months identified:\n")
cat("  Site-month combinations:", nrow(zoo_sampling_months), "\n")
cat("  Unique sites:", n_distinct(zoo_sampling_months$siteID), "\n\n")

# ============================================================================
# Part 2: Filter Sensor Chlorophyll to Zooplankton Sampling Months
# ============================================================================

cat("Filtering sensor chlorophyll data to zooplankton sampling months...\n\n")

sensor_chl_processed <- sensor_chl_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) |>
  # Filter to zooplankton sites
  filter(siteID %in% c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")) |>
  # Inner join to keep only sensor data from months when zooplankton were sampled
  inner_join(
    zoo_sampling_months,
    by = c("siteID", "year", "month")
  ) |>
  select(siteID, year, month, date, meanChlorophyll, maxChlorophyll, minChlorophyll)

cat("Sensor chlorophyll data filtered to zooplankton sampling months:\n")
cat("  Records retained:", nrow(sensor_chl_processed), "\n")
cat("  Percent of original:", round(nrow(sensor_chl_processed) / nrow(sensor_chl_raw) * 100, 1), "%\n\n")

# ============================================================================
# Part 3: Summary Statistics
# ============================================================================

cat("Calculating sensor chlorophyll summary statistics...\n\n")

# By site-month
sensor_chl_by_site_month <- sensor_chl_processed |>
  group_by(siteID, year, month) |>
  summarise(
    n_days = n(),
    chl_mean = mean(meanChlorophyll, na.rm = TRUE),
    chl_sd = sd(meanChlorophyll, na.rm = TRUE),
    chl_min = min(minChlorophyll, na.rm = TRUE),
    chl_max = max(maxChlorophyll, na.rm = TRUE),
    chl_daily_cv = sd(meanChlorophyll, na.rm = TRUE) / mean(meanChlorophyll, na.rm = TRUE),  # Coefficient of variation
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Sensor chlorophyll summary by site-month:\n")
cat("  Unique site-month combinations:", nrow(sensor_chl_by_site_month), "\n\n")

# By site (overall)
sensor_chl_by_site <- sensor_chl_by_site_month |>
  group_by(siteID) |>
  summarise(
    n_sampling_months = n(),
    chl_mean_overall = mean(chl_mean, na.rm = TRUE),
    chl_sd_overall = sd(chl_mean, na.rm = TRUE),
    chl_min_overall = min(chl_min, na.rm = TRUE),
    chl_max_overall = max(chl_max, na.rm = TRUE),
    chl_variability = mean(chl_daily_cv, na.rm = TRUE),  # Average daily variability
    .groups = "drop"
  ) |>
  arrange(desc(chl_mean_overall))

cat("Sensor chlorophyll summary by site:\n")
print(sensor_chl_by_site)
cat("\n")

# By month (across all sites)
sensor_chl_by_month <- sensor_chl_processed |>
  group_by(month) |>
  summarise(
    n_days = n(),
    chl_mean = mean(meanChlorophyll, na.rm = TRUE),
    chl_sd = sd(meanChlorophyll, na.rm = TRUE),
    chl_min = min(minChlorophyll, na.rm = TRUE),
    chl_max = max(maxChlorophyll, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month]
  ) |>
  arrange(month)

cat("Sensor chlorophyll summary by month (across all sites):\n")
cat("  Mean:", round(mean(sensor_chl_processed$meanChlorophyll, na.rm = TRUE), 2), "μg/L\n")
cat("  SD:", round(sd(sensor_chl_processed$meanChlorophyll, na.rm = TRUE), 2), "μg/L\n")
cat("  Range:", round(min(sensor_chl_processed$minChlorophyll, na.rm = TRUE), 2), "-",
    round(max(sensor_chl_processed$maxChlorophyll, na.rm = TRUE), 2), "μg/L\n\n")

# ============================================================================
# Part 4: Save Processed Data
# ============================================================================

cat("Saving processed data...\n\n")

write_csv(sensor_chl_processed, "data-processed/sensor_chlorophyll_during_zooplankton_sampling.csv")
cat("✓ Saved: data-processed/sensor_chlorophyll_during_zooplankton_sampling.csv\n")

write_csv(sensor_chl_by_site_month, "stats-tables/sensor_chlorophyll_summary_by_site_month.csv")
cat("✓ Saved: stats-tables/sensor_chlorophyll_summary_by_site_month.csv\n\n")

# ============================================================================
# Part 5: Create Visualizations
# ============================================================================

cat("Creating visualizations...\n\n")

# Plot 1: Sensor chlorophyll by site (boxplot)
p1_by_site <- sensor_chl_processed |>
  mutate(siteID = fct_reorder(siteID, meanChlorophyll, .fun = median)) |>
  ggplot(aes(x = siteID, y = meanChlorophyll, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
  labs(
    title = "Daily Sensor Chlorophyll by Site",
    subtitle = "Data from zooplankton sampling months only",
    x = "Site",
    y = "Chlorophyll (μg/L)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/sensor_chlorophyll_by_site.png", p1_by_site, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/sensor_chlorophyll_by_site.png\n")

# Plot 2: Sensor chlorophyll by month (seasonal pattern)
p2_by_month <- sensor_chl_by_month |>
  ggplot(aes(x = month, y = chl_mean, fill = month_name)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = chl_mean - chl_sd, ymax = chl_mean + chl_sd),
                width = 0.3, alpha = 0.7) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    limits = c(0.5, 12.5)
  ) +
  labs(
    title = "Seasonal Pattern of Sensor Chlorophyll",
    subtitle = "Mean ± SD across all sites during zooplankton sampling",
    x = "Month",
    y = "Chlorophyll (μg/L)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/sensor_chlorophyll_seasonal_pattern.png", p2_by_month, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/sensor_chlorophyll_seasonal_pattern.png\n")

# Plot 3: Sensor chlorophyll over time (time series by site with daily resolution)
p3_time_series <- sensor_chl_processed |>
  ggplot(aes(x = date, y = meanChlorophyll, color = siteID)) +
  geom_line(size = 0.6, alpha = 0.7) +
  facet_wrap(~siteID, scales = "free_y", ncol = 3) +
  labs(
    title = "Daily Sensor Chlorophyll Over Time by Site",
    subtitle = "High-frequency daily measurements during zooplankton sampling months",
    x = "Date",
    y = "Chlorophyll (μg/L)",
    color = "Site"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/sensor_chlorophyll_time_series_by_site.png", p3_time_series, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/sensor_chlorophyll_time_series_by_site.png\n")

# Plot 4: Daily variability (max - min)
sensor_chl_processed_var <- sensor_chl_processed |>
  mutate(
    daily_range = maxChlorophyll - minChlorophyll
  )

p4_variability <- sensor_chl_processed_var |>
  ggplot(aes(x = siteID, y = daily_range, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 2) +
  labs(
    title = "Daily Chlorophyll Variability (Max - Min)",
    subtitle = "Sensor range during zooplankton sampling months",
    x = "Site",
    y = "Daily Range (μg/L)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/sensor_chlorophyll_daily_variability.png", p4_variability, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/sensor_chlorophyll_daily_variability.png\n")

# Plot 5: Heatmap - mean daily chlorophyll by site and month
sensor_chl_heatmap_data <- sensor_chl_by_site_month |>
  mutate(month_name = month.abb[month]) |>
  select(siteID, month_name, chl_mean) |>
  pivot_wider(
    names_from = month_name,
    values_from = chl_mean,
    values_fill = NA
  )

p5_heatmap <- sensor_chl_by_site_month |>
  mutate(month_name = month.abb[month]) |>
  ggplot(aes(x = month_name, y = siteID, fill = chl_mean)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Mean Chlorophyll\n(μg/L)", na.value = "gray90") +
  scale_x_discrete(limits = month.abb) +
  labs(
    title = "Sensor Chlorophyll Heatmap: Site × Month",
    subtitle = "Daily mean chlorophyll during zooplankton sampling months",
    x = "Month",
    y = "Site"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("figures/sensor_chlorophyll_heatmap_site_month.png", p5_heatmap, width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/sensor_chlorophyll_heatmap_site_month.png\n\n")

# ============================================================================
# Part 6: Summary Report
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SENSOR CHLOROPHYLL ANALYSIS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("DATA RETAINED:\n")
cat("  Original sensor chlorophyll records:", nrow(sensor_chl_raw), "\n")
cat("  Filtered to zoo sampling months:", nrow(sensor_chl_processed), "\n")
cat("  Retention rate:", round(nrow(sensor_chl_processed) / nrow(sensor_chl_raw) * 100, 1), "%\n\n")

cat("OVERALL SENSOR CHLOROPHYLL PATTERNS:\n")
cat("  Mean:", round(mean(sensor_chl_processed$meanChlorophyll, na.rm = TRUE), 2), "μg/L\n")
cat("  Median:", round(median(sensor_chl_processed$meanChlorophyll, na.rm = TRUE), 2), "μg/L\n")
cat("  SD:", round(sd(sensor_chl_processed$meanChlorophyll, na.rm = TRUE), 2), "μg/L\n")
cat("  Range:", round(min(sensor_chl_processed$minChlorophyll, na.rm = TRUE), 2), "-",
    round(max(sensor_chl_processed$maxChlorophyll, na.rm = TRUE), 2), "μg/L\n\n")

cat("DAILY VARIABILITY:\n")
cat("  Mean daily range (max-min):", round(mean(sensor_chl_processed_var$daily_range, na.rm = TRUE), 2), "μg/L\n")
cat("  This shows diel (daily) cycle variability in chlorophyll from photosynthesis\n\n")

cat("BY SITE (ranked by mean sensor chlorophyll):\n")
print(sensor_chl_by_site)
cat("\n")

cat("SEASONAL PATTERN:\n")
cat("  Highest chlorophyll month:", sensor_chl_by_month$month_name[which.max(sensor_chl_by_month$chl_mean)],
    "(", round(max(sensor_chl_by_month$chl_mean), 2), "μg/L )\n")
cat("  Lowest chlorophyll month:", sensor_chl_by_month$month_name[which.min(sensor_chl_by_month$chl_mean)],
    "(", round(min(sensor_chl_by_month$chl_mean), 2), "μg/L )\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("  - Sensor chlorophyll = continuous daily measurements (high temporal resolution)\n")
cat("  - vs. Script 20 discrete samples = episodic sampling (lower frequency)\n")
cat("  - Daily variability reflects photosynthetic diel cycle\n")
cat("  - High max-min range = strong daily photosynthesis cycle\n")
cat("  - Can be compared with zooplankton body size and oxygen dynamics\n\n")

cat("COMPARISON WITH SCRIPT 20 (Discrete Samples):\n")
cat("  Script 20: Episodic sampling (few samples per month per site)\n")
cat("  Script 21: Continuous sensor (daily measurements)\n")
cat("  Both can be merged with body size data for comprehensive food availability analysis\n\n")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Analysis complete. Data and figures saved.\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

