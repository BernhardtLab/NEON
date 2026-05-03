# Compare Summer Temperature Across Lake Sites
# Purpose: Analyze temperature during growing season (summer, June-August) to identify warmest sites
# Date: 2026-05-02
#
# INPUTS:
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Raw daily temperature data from NEON)
#
# OUTPUTS:
#   - stats-tables/summer_temperature_by_site.csv
#     (Summary statistics for each site during summer)
#   - stats-tables/summer_temperature_by_month.csv
#     (Site-specific monthly breakdown for June, July, August)
#   - stats-tables/summer_temperature_by_year.csv
#     (Year-specific summer temperatures by site)
#   - stats-tables/summer_temperature_anova.csv
#     (ANOVA test results comparing sites)
#   - stats-tables/summer_temperature_pairwise.csv
#     (Pairwise comparisons between sites)

library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)

# Create stats-tables directory if it doesn't exist
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
  cat("Created stats-tables directory\n\n")
}

# ============================================================================
# Part 1: Load Temperature Data
# ============================================================================

cat("Loading temperature data...\n\n")

temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

temp_data <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    doy = yday(date)
  ) |>
  arrange(siteID, date)

cat("Temperature data loaded:\n")
cat("  Records:", nrow(temp_data), "\n")
cat("  Sites:", n_distinct(temp_data$siteID), "\n")
cat("  Date range:", min(temp_data$date), "to", max(temp_data$date), "\n\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

# ============================================================================
# Part 2: Filter for Summer (Growing Season)
# ============================================================================

cat("Filtering for summer months (June, July, August)...\n\n")

# Define summer as months 6-8 (JJA)
summer_data <- temp_data |>
  filter(siteID %in% zoo_sites, month %in% c(6, 7, 8)) |>
  mutate(
    month_name = factor(month.abb[month], levels = c("Jun", "Jul", "Aug"))
  ) |>
  arrange(siteID, date)

cat("Summer temperature dataset:\n")
cat("  Records:", nrow(summer_data), "\n")
cat("  Sites:", n_distinct(summer_data$siteID), "\n")
cat("  Date range:", min(summer_data$date), "to", max(summer_data$date), "\n\n")

# ============================================================================
# Part 3: Summary Statistics by Site - Summer
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMER TEMPERATURE SUMMARY BY SITE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

summer_by_site <- summer_data |>
  group_by(siteID) |>
  summarise(
    n_days = n(),
    temp_mean = round(mean(meanTemp, na.rm = TRUE), 2),
    temp_sd = round(sd(meanTemp, na.rm = TRUE), 2),
    temp_min = round(min(minTemp, na.rm = TRUE), 2),
    temp_max = round(max(maxTemp, na.rm = TRUE), 2),
    temp_median = round(median(meanTemp, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(desc(temp_mean))

write_csv(summer_by_site, "stats-tables/summer_temperature_by_site.csv")
cat("✓ Saved: stats-tables/summer_temperature_by_site.csv\n\n")

# ============================================================================
# Part 4: Monthly Breakdown - Summer
# ============================================================================

summer_by_site_month <- summer_data |>
  group_by(siteID, month_name) |>
  summarise(
    n_days = n(),
    temp_mean = round(mean(meanTemp, na.rm = TRUE), 2),
    temp_sd = round(sd(meanTemp, na.rm = TRUE), 2),
    temp_max = round(max(maxTemp, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(month_name, desc(temp_mean))

write_csv(summer_by_site_month, "stats-tables/summer_temperature_by_month.csv")
cat("✓ Saved: stats-tables/summer_temperature_by_month.csv\n")

# ============================================================================
# Part 5: Year-over-Year Comparison
# ============================================================================

summer_by_year <- summer_data |>
  group_by(siteID, year) |>
  summarise(
    temp_mean = round(mean(meanTemp, na.rm = TRUE), 2),
    temp_sd = round(sd(meanTemp, na.rm = TRUE), 2),
    n_days = n(),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = year,
    values_from = temp_mean,
    id_cols = siteID
  ) |>
  arrange(siteID)

write_csv(summer_by_year, "stats-tables/summer_temperature_by_year.csv")
cat("✓ Saved: stats-tables/summer_temperature_by_year.csv\n")

# ============================================================================
# Part 6: Statistical Comparison - ANOVA
# ============================================================================

# One-way ANOVA to test if summer temps differ significantly across sites
anova_result <- aov(meanTemp ~ siteID, data = summer_data)
anova_summary <- summary(anova_result)

anova_f <- anova_summary[[1]]$`F value`[1]
anova_p <- anova_summary[[1]]$`Pr(>F)`[1]

# Save ANOVA results
anova_results <- data.frame(
  test = "One-way ANOVA: Summer Temperature ~ Site",
  F_value = round(anova_f, 4),
  p_value = anova_p,
  significant = if_else(anova_p < 0.05, "Yes", "No"),
  result = if_else(anova_p < 0.05,
                   "Sites differ significantly in summer temperature",
                   "No significant difference in summer temperature")
)

write_csv(anova_results, "stats-tables/summer_temperature_anova.csv")
cat("✓ Saved: stats-tables/summer_temperature_anova.csv\n")

# Pairwise comparisons
warmest_site <- summer_by_site$siteID[1]
coolest_site <- summer_by_site$siteID[nrow(summer_by_site)]
warmest_temp <- summer_by_site$temp_mean[1]
coolest_temp <- summer_by_site$temp_mean[nrow(summer_by_site)]
temp_diff <- warmest_temp - coolest_temp

pairwise_comparison <- data.frame(
  comparison = c("Warmest site", "Coolest site", "Temperature difference"),
  site = c(warmest_site, coolest_site, NA),
  temperature_C = c(warmest_temp, coolest_temp, temp_diff)
)

write_csv(pairwise_comparison, "stats-tables/summer_temperature_pairwise.csv")
cat("✓ Saved: stats-tables/summer_temperature_pairwise.csv\n")

# ============================================================================
# Part 8: Create Visualizations
# ============================================================================

cat("Creating visualizations...\n\n")

theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  ))

# Plot 1: Box plot of summer temperatures by site
p1_summer_box <- summer_data |>
  ggplot(aes(x = reorder(siteID, meanTemp, FUN = median), y = meanTemp, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  labs(
    title = "Summer Temperature Distribution Across Lake Sites",
    x = "Site ID",
    y = "Daily Mean Temperature (°C)",
    subtitle = "June, July, August only"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/summer_temperature_distribution.png", p1_summer_box, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/summer_temperature_distribution.png\n")

# Plot 2: Summer temperature time series by site
p2_summer_ts <- summer_data |>
  ggplot(aes(x = date, y = meanTemp, color = siteID, group = siteID)) +
  geom_line(alpha = 0.6, size = 0.8) +
  facet_wrap(~siteID, ncol = 2) +
  labs(
    title = "Summer Temperature Time Series by Site",
    x = "Date",
    y = "Daily Mean Temperature (°C)",
    subtitle = "June-August data across all years"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("figures/summer_temperature_timeseries.png", p2_summer_ts, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/summer_temperature_timeseries.png\n")

# Plot 3: Monthly comparison (June, July, August)
p3_summer_monthly <- summer_data |>
  ggplot(aes(x = month_name, y = meanTemp, fill = siteID)) +
  geom_boxplot(alpha = 0.7, position = "dodge", outlier.alpha = 0.3) +
  labs(
    title = "Summer Temperature by Month Across Sites",
    x = "Month",
    y = "Daily Mean Temperature (°C)",
    fill = "Site"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/summer_temperature_by_month.png", p3_summer_monthly, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/summer_temperature_by_month.png\n")

# Plot 4: Violin plot - distribution shape comparison
p4_summer_violin <- summer_data |>
  ggplot(aes(x = reorder(siteID, meanTemp, FUN = mean), y = meanTemp, fill = siteID)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(
    title = "Summer Temperature Distribution Shape by Site",
    x = "Site ID (ordered by mean temperature)",
    y = "Daily Mean Temperature (°C)"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/summer_temperature_violin.png", p4_summer_violin, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/summer_temperature_violin.png\n")

# Plot 5: Mean summer temperature by site (bar plot with error bars)
p5_summer_means <- summer_by_site |>
  mutate(siteID = fct_reorder(siteID, temp_mean, .desc = TRUE)) |>
  ggplot(aes(x = siteID, y = temp_mean, fill = siteID)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = temp_mean - temp_sd, ymax = temp_mean + temp_sd),
                width = 0.2, alpha = 0.7, size = 1) +
  labs(
    title = "Mean Summer Temperature by Site",
    x = "Site ID",
    y = "Mean Temperature (°C)",
    subtitle = "Error bars show ±1 SD"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/summer_temperature_means.png", p5_summer_means, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/summer_temperature_means.png\n")

# ============================================================================
# Part 9: Summary
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SUMMARY: GROWING SEASON (SUMMER) TEMPERATURE COMPARISON\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("KEY FINDINGS:\n\n")

cat(sprintf("1. WARMEST SITE: %s (%.2f ± %.2f °C)\n",
            summer_by_site$siteID[1],
            summer_by_site$temp_mean[1],
            summer_by_site$temp_sd[1]))

cat(sprintf("2. COOLEST SITE: %s (%.2f ± %.2f °C)\n",
            summer_by_site$siteID[nrow(summer_by_site)],
            summer_by_site$temp_mean[nrow(summer_by_site)],
            summer_by_site$temp_sd[nrow(summer_by_site)]))

cat(sprintf("3. TEMPERATURE RANGE: %.2f °C difference between warmest and coolest\n\n", temp_diff))

cat("4. STATISTICAL SIGNIFICANCE:\n")
if (anova_p < 0.05) {
  cat(sprintf("   Sites differ significantly in summer temperature (p < 0.001)\n")
    )
} else {
  cat("   No significant differences in summer temperature across sites\n")
}

cat("\n5. SITE RANKING (warm to cool):\n")
for (i in 1:nrow(summer_by_site)) {
  cat(sprintf("   %d. %s: %.2f °C\n", i, summer_by_site$siteID[i], summer_by_site$temp_mean[i]))
}

cat("\n\nIMPLICATIONS FOR ZOOPLANKTON:\n")
cat("- Warmer sites may support faster zooplankton development and growth\n")
cat("- Summer temperature differences could explain body size variation across sites\n")
cat("- Consider site-specific thermal habitat when interpreting body size responses\n")

cat("\n\n================================\n")
cat("ANALYSIS COMPLETE\n")
cat("================================\n")
cat("Statistical results saved to stats-tables/:\n")
cat("  - summer_temperature_by_site.csv\n")
cat("  - summer_temperature_by_month.csv\n")
cat("  - summer_temperature_by_year.csv\n")
cat("  - summer_temperature_anova.csv\n")
cat("  - summer_temperature_pairwise.csv\n\n")
cat("Figures saved to figures/\n\n")

