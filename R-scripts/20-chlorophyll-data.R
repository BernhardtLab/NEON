# Phytoplankton Chlorophyll Analysis
# Purpose: Analyze chlorophyll concentrations during zooplankton sampling months
#          Compare chlorophyll patterns across sites and seasons
# Date: 2026-05-04
#
# INPUTS:
#   - data-raw/NEON_daily_summaries/NEON_phyto_chlorophyll.csv
#     (Raw chlorophyll measurements by sample)
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Body size data with collection dates to identify sampling months)
#
# OUTPUTS:
#   - data-processed/chlorophyll_during_zooplankton_sampling.csv
#     (Chlorophyll data filtered to zooplankton sampling months)
#   - stats-tables/chlorophyll_summary_by_site_month.csv
#     (Monthly chlorophyll summaries by site)
#   - figures/chlorophyll_*.png (7 visualization figures)
#     * chlorophyll_by_site.png - Boxplot by site
#     * chlorophyll_seasonal_pattern.png - Mean by month
#     * chlorophyll_time_series_by_site.png - Temporal trends
#     * chlorophyll_distribution.png - Histogram by site
#     * chlorophyll_heatmap_site_month.png - Heatmap
#     * chlorophyll_vs_temperature_by_site.png - Scatter by site with loess
#     * chlorophyll_vs_temperature_all_sites.png - Combined scatter plot

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

cat("Loading chlorophyll data...\n\n")

chl_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_phyto_chlorophyll.csv")

cat("Raw chlorophyll data:\n")
cat("  Records:", nrow(chl_raw), "\n")
cat("  Sites:", n_distinct(chl_raw$siteID), "\n")
cat("  Date range:", min(chl_raw$collectDate), "to", max(chl_raw$collectDate), "\n\n")

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
# Part 2: Filter Chlorophyll to Zooplankton Sampling Months
# ============================================================================

cat("Filtering chlorophyll data to zooplankton sampling months...\n\n")

chl_processed <- chl_raw |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  # Filter to zooplankton sites
  filter(siteID %in% c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")) |>
  # Inner join to keep only chlorophyll data from months when zooplankton were sampled
  inner_join(
    zoo_sampling_months,
    by = c("siteID", "year", "month")
  ) |>
  select(siteID, year, month, collectDate, chlorophyllMicrogramsPerLiter)

cat("Chlorophyll data filtered to zooplankton sampling months:\n")
cat("  Records retained:", nrow(chl_processed), "\n")
cat("  Percent of original:", round(nrow(chl_processed) / nrow(chl_raw) * 100, 1), "%\n\n")

# ============================================================================
# Part 3: Summary Statistics
# ============================================================================

cat("Calculating chlorophyll summary statistics...\n\n")

# By site-month
chl_by_site_month <- chl_processed |>
  group_by(siteID, year, month) |>
  summarise(
    n_samples = n(),
    chl_mean = mean(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_sd = sd(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_min = min(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_max = max(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Chlorophyll summary by site-month:\n")
cat("  Unique site-month combinations:", nrow(chl_by_site_month), "\n\n")

# By site (overall)
chl_by_site <- chl_by_site_month |>
  group_by(siteID) |>
  summarise(
    n_sampling_events = n(),
    chl_mean_overall = mean(chl_mean, na.rm = TRUE),
    chl_sd_overall = sd(chl_mean, na.rm = TRUE),
    chl_min_overall = min(chl_min, na.rm = TRUE),
    chl_max_overall = max(chl_max, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(chl_mean_overall))

cat("Chlorophyll summary by site:\n")
print(chl_by_site)
cat("\n")

# By month (across all sites)
chl_by_month <- chl_processed |>
  group_by(month) |>
  summarise(
    n_samples = n(),
    chl_mean = mean(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_sd = sd(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_min = min(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    chl_max = max(chlorophyllMicrogramsPerLiter, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month]
  ) |>
  arrange(month)

cat("Chlorophyll summary by month (across all sites):\n")
cat("  Mean:", round(mean(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n")
cat("  SD:", round(sd(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n")
cat("  Range:", round(min(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "-",
    round(max(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n\n")

# ============================================================================
# Part 3.5: Merge with Monthly Temperature Data
# ============================================================================

cat("Loading monthly temperature data and merging with chlorophyll...\n\n")

# Load monthly temperature summary
temp_monthly <- read_csv("data-processed/temperature_monthly_summary.csv")

cat("Monthly temperature data:\n")
cat("  Records:", nrow(temp_monthly), "\n")
cat("  Sites:", n_distinct(temp_monthly$siteID), "\n\n")

# Merge chlorophyll with temperature at monthly level
chl_temp_by_site_month <- chl_by_site_month |>
  left_join(
    temp_monthly |> select(siteID, year, month, temp_mean_monthly, temp_sd_monthly),
    by = c("siteID", "year", "month")
  )

cat("Merged chlorophyll-temperature data:\n")
cat("  Site-month combinations with both:", sum(!is.na(chl_temp_by_site_month$temp_mean_monthly) & !is.na(chl_temp_by_site_month$chl_mean)), "\n\n")

# ============================================================================
# Part 4: Save Processed Data
# ============================================================================

cat("Saving processed data...\n\n")

write_csv(chl_processed, "data-processed/chlorophyll_during_zooplankton_sampling.csv")
cat("✓ Saved: data-processed/chlorophyll_during_zooplankton_sampling.csv\n")

write_csv(chl_by_site_month, "stats-tables/chlorophyll_summary_by_site_month.csv")
cat("✓ Saved: stats-tables/chlorophyll_summary_by_site_month.csv\n\n")

# ============================================================================
# Part 5: Create Visualizations
# ============================================================================

cat("Creating visualizations...\n\n")

# Plot 1: Chlorophyll by site (boxplot)
p1_by_site <- chl_processed |>
  mutate(siteID = fct_reorder(siteID, chlorophyllMicrogramsPerLiter, .fun = median)) |>
  ggplot(aes(x = siteID, y = chlorophyllMicrogramsPerLiter, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 2) +
  labs(
    title = "Chlorophyll Concentrations by Site",
    subtitle = "Data from zooplankton sampling months only",
    x = "Site",
    y = "Chlorophyll (μg/L)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/chlorophyll_by_site.png", p1_by_site, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/chlorophyll_by_site.png\n")

# Plot 2: Chlorophyll by month (seasonal pattern)
p2_by_month <- chl_by_month |>
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
    title = "Seasonal Pattern of Chlorophyll",
    subtitle = "Mean ± SD across all sites during zooplankton sampling",
    x = "Month",
    y = "Chlorophyll (μg/L)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/chlorophyll_seasonal_pattern.png", p2_by_month, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/chlorophyll_seasonal_pattern.png\n")

# Plot 3: Chlorophyll over time (time series by site)
p3_time_series <- chl_by_site_month |>
  mutate(
    date = as.Date(paste(year, month, "15", sep = "-"))
  ) |>
  ggplot(aes(x = date, y = chl_mean, color = siteID, group = siteID)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~siteID, scales = "free_y", ncol = 3) +
  labs(
    title = "Chlorophyll Over Time by Site",
    subtitle = "Monthly mean during zooplankton sampling months",
    x = "Date",
    y = "Chlorophyll (μg/L)",
    color = "Site"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/chlorophyll_time_series_by_site.png", p3_time_series, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/chlorophyll_time_series_by_site.png\n")

# Plot 4: Chlorophyll distribution (histogram with density)
p4_distribution <- chl_processed |>
  ggplot(aes(x = chlorophyllMicrogramsPerLiter, fill = siteID)) +
  geom_histogram(alpha = 0.5, bins = 30) +
  geom_density(alpha = 0.3) +
  facet_wrap(~siteID, ncol = 3) +
  labs(
    title = "Distribution of Chlorophyll Concentrations",
    subtitle = "During zooplankton sampling months",
    x = "Chlorophyll (μg/L)",
    y = "Frequency"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/chlorophyll_distribution.png", p4_distribution, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/chlorophyll_distribution.png\n")

# Plot 5: Heatmap - chlorophyll by site and month
chl_heatmap_data <- chl_by_site_month |>
  mutate(month_name = month.abb[month]) |>
  pivot_wider(
    names_from = month_name,
    values_from = chl_mean,
    values_fill = NA
  ) |>
  column_to_rownames("siteID") |>
  as.matrix()

# Reorder months properly
month_order <- month.abb
chl_heatmap_data <- chl_heatmap_data[, colnames(chl_heatmap_data) %in% month_order]
chl_heatmap_data <- chl_heatmap_data[, match(colnames(chl_heatmap_data), month_order)]

p5_heatmap <- chl_by_site_month |>
  mutate(month_name = month.abb[month]) |>
  ggplot(aes(x = month_name, y = siteID, fill = chl_mean)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Chlorophyll\n(μg/L)", na.value = "gray90") +
  scale_x_discrete(limits = month.abb) +
  labs(
    title = "Chlorophyll Heatmap: Site × Month",
    subtitle = "Mean chlorophyll during zooplankton sampling months",
    x = "Month",
    y = "Site"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("figures/chlorophyll_heatmap_site_month.png", p5_heatmap, width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/chlorophyll_heatmap_site_month.png\n")

# Plot 6: Chlorophyll vs Temperature scatter plot by site
p6_chl_temp_scatter <- chl_temp_by_site_month |>
  filter(!is.na(temp_mean_monthly), !is.na(chl_mean)) |>
  ggplot(aes(x = temp_mean_monthly, y = chl_mean, color = siteID, size = n_samples)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(color = siteID), method = "loess", se = TRUE, alpha = 0.2) +
  facet_wrap(~siteID, scales = "free", ncol = 3) +
  labs(
    title = "Chlorophyll vs Temperature by Site",
    subtitle = "Monthly data during zooplankton sampling months",
    x = "Temperature (°C)",
    y = "Chlorophyll (μg/L)",
    color = "Site",
    size = "Number of\nSamples"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/chlorophyll_vs_temperature_by_site.png", p6_chl_temp_scatter, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/chlorophyll_vs_temperature_by_site.png\n")

# Plot 7: Chlorophyll vs Temperature combined across all sites
p7_chl_temp_all <- chl_temp_by_site_month |>
  filter(!is.na(temp_mean_monthly), !is.na(chl_mean)) |>
  ggplot(aes(x = temp_mean_monthly, y = chl_mean, color = siteID)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(
    title = "Chlorophyll vs Temperature Relationship",
    subtitle = "All sites combined, monthly data during zooplankton sampling months",
    x = "Temperature (°C)",
    y = "Chlorophyll (μg/L)",
    color = "Site",
    shape = "Site"
  ) +
  theme(
    legend.position = "right"
  )

ggsave("figures/chlorophyll_vs_temperature_all_sites.png", p7_chl_temp_all, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/chlorophyll_vs_temperature_all_sites.png\n\n")

# ============================================================================
# Part 6: Summary Report
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("CHLOROPHYLL ANALYSIS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("DATA RETAINED:\n")
cat("  Original chlorophyll records:", nrow(chl_raw), "\n")
cat("  Filtered to zoo sampling months:", nrow(chl_processed), "\n")
cat("  Retention rate:", round(nrow(chl_processed) / nrow(chl_raw) * 100, 1), "%\n\n")

cat("OVERALL CHLOROPHYLL PATTERNS:\n")
cat("  Mean:", round(mean(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n")
cat("  Median:", round(median(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n")
cat("  SD:", round(sd(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n")
cat("  Range:", round(min(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "-",
    round(max(chl_processed$chlorophyllMicrogramsPerLiter, na.rm = TRUE), 2), "μg/L\n\n")

cat("BY SITE (ranked by mean chlorophyll):\n")
print(chl_by_site)
cat("\n")

cat("SEASONAL PATTERN:\n")
cat("  Highest chlorophyll month:", chl_by_month$month_name[which.max(chl_by_month$chl_mean)],
    "(", round(max(chl_by_month$chl_mean), 2), "μg/L )\n")
cat("  Lowest chlorophyll month:", chl_by_month$month_name[which.min(chl_by_month$chl_mean)],
    "(", round(min(chl_by_month$chl_mean), 2), "μg/L )\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("  - Chlorophyll represents photosynthetic organisms (phytoplankton)\n")
cat("  - This is different from AFDM (ash-free dry mass of ALGAE ONLY)\n")
cat("  - Chlorophyll is a proxy for phytoplankton productivity\n")
cat("  - High chlorophyll = productive lake ecosystem\n")
cat("  - Temperature relationship: warmer temperatures typically support higher primary productivity\n")
cat("  - Site-specific temperature-chlorophyll relationships reflect local limnological conditions\n")
cat("  - Can compare with zooplankton body size responses\n\n")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Analysis complete. Data and figures saved.\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

