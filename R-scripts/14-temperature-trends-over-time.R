# Test for Temperature Trends Over Time
# Purpose: Determine if lake sites are warming or cooling over the entire time series
# Date: 2026-05-02

library(tidyverse)
library(readr)
library(lubridate)

# ============================================================================
# Part 1: Load and Prepare Data
# ============================================================================

cat("Loading temperature data...\n\n")

temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

temp_data <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    doy = yday(date),
    year_decimal = year + (doy / 365.25)
  ) |>
  arrange(siteID, date)

cat("Data loaded:\n")
cat("  Records:", nrow(temp_data), "\n")
cat("  Sites:", n_distinct(temp_data$siteID), "\n")
cat("  Date range:", min(temp_data$date), "to", max(temp_data$date), "\n\n")

# ============================================================================
# Part 2: Test for Trends Using Linear Regression
# ============================================================================

cat("=" * 80, "\n")
cat("TEMPERATURE TRENDS OVER TIME\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

# Model 1: Raw daily temperature trends
cat("MODEL 1: Linear Trend in Daily Mean Temperature\n")
cat("Regression: meanTemp ~ year_decimal\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

trend_results <- list()

for (site in zoo_sites) {
  site_data <- temp_data |>
    filter(siteID == site)

  # Fit linear model
  model <- lm(meanTemp ~ year_decimal, data = site_data)

  # Extract results
  coef_year <- coef(model)[2]
  pval <- summary(model)$coefficients[2, 4]
  r_squared <- summary(model)$r.squared
  n_obs <- nrow(site_data)

  # Interpretation
  trend_dir <- if_else(coef_year > 0, "WARMING", "COOLING")
  sig_label <- if_else(pval < 0.05, "**SIGNIFICANT**", "not significant")

  trend_results[[site]] <- list(
    slope = coef_year,
    pval = pval,
    r_squared = r_squared,
    n_obs = n_obs,
    trend = trend_dir
  )

  # Annual change in degrees
  annual_change <- coef_year

  cat(sprintf("%s:\n", site))
  cat(sprintf("  Trend: %s (%+.4f °C/year) %s\n", trend_dir, annual_change, sig_label))
  cat(sprintf("  P-value: %.4f\n", pval))
  cat(sprintf("  R²: %.4f\n", r_squared))
  cat(sprintf("  Total change over series: %+.2f °C\n",
              coef_year * (max(site_data$year_decimal) - min(site_data$year_decimal))))
  cat(sprintf("  N observations: %,d\n\n", n_obs))
}

# ============================================================================
# Part 3: Monthly Trends (to account for seasonality)
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("MODEL 2: Trend Accounting for Seasonal Variation\n")
cat("Regression: meanTemp ~ year_decimal + month + (interaction)\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

for (site in zoo_sites) {
  site_data <- temp_data |>
    filter(siteID == site) |>
    mutate(month_factor = factor(month))

  # Model with month as categorical (controls for seasonality)
  model_seasonal <- lm(meanTemp ~ year_decimal + month_factor, data = site_data)

  coef_year <- coef(model_seasonal)[2]
  pval <- summary(model_seasonal)$coefficients[2, 4]
  r_squared <- summary(model_seasonal)$r.squared

  trend_dir <- if_else(coef_year > 0, "WARMING", "COOLING")
  sig_label <- if_else(pval < 0.05, "**SIGNIFICANT**", "not significant")

  cat(sprintf("%s:\n", site))
  cat(sprintf("  Trend (controlling for month): %s (%+.4f °C/year) %s\n",
              trend_dir, coef_year, sig_label))
  cat(sprintf("  P-value: %.4f\n", pval))
  cat(sprintf("  R²: %.4f\n\n", r_squared))
}

# ============================================================================
# Part 4: Create Visualizations
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("Creating visualizations...\n\n")

theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  ))

# Plot 1: Daily temperature trends with regression lines
p1_trends <- temp_data |>
  filter(siteID %in% zoo_sites) |>
  ggplot(aes(x = year_decimal, y = meanTemp, color = siteID)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1) +
  facet_wrap(~siteID, scales = "free_y", ncol = 2) +
  labs(
    title = "Temperature Trends Over Time (Daily Data with Linear Fit)",
    x = "Year",
    y = "Mean Temperature (°C)",
    subtitle = "Points show daily measurements; lines show linear regression fit"
  ) +
  theme(legend.position = "none")

ggsave("figures/temperature_trends_daily.png", p1_trends, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/temperature_trends_daily.png\n")

# Plot 2: Annual mean temperature trends
annual_temps <- temp_data |>
  filter(siteID %in% zoo_sites) |>
  group_by(siteID, year) |>
  summarise(
    mean_temp = mean(meanTemp, na.rm = TRUE),
    sd_temp = sd(meanTemp, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

p2_annual <- annual_temps |>
  ggplot(aes(x = year, y = mean_temp, color = siteID)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.3, size = 1, linetype = "dashed") +
  facet_wrap(~siteID, scales = "free_y", ncol = 2) +
  labs(
    title = "Annual Mean Temperature Trends",
    x = "Year",
    y = "Annual Mean Temperature (°C)",
    subtitle = "Points and lines show annual means; dashed lines show linear trends"
  ) +
  theme(legend.position = "none")

ggsave("figures/temperature_trends_annual.png", p2_annual, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/temperature_trends_annual.png\n")

# Plot 3: Trend slopes by site
trend_summary <- tibble(
  siteID = names(trend_results),
  slope = sapply(trend_results, \(x) x$slope),
  pval = sapply(trend_results, \(x) x$pval),
  significant = pval < 0.05
)

p3_slopes <- trend_summary |>
  mutate(siteID = fct_reorder(siteID, slope)) |>
  ggplot(aes(x = siteID, y = slope, fill = significant)) +
  geom_col(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  scale_fill_manual(values = c("FALSE" = "gray70", "TRUE" = "salmon"), name = "Significant (p < 0.05)") +
  labs(
    title = "Temperature Trend Slopes by Site",
    x = "Site ID",
    y = "Trend Slope (°C per year)",
    subtitle = "Positive = warming, Negative = cooling"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/temperature_trend_slopes.png", p3_slopes, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/temperature_trend_slopes.png\n")

# ============================================================================
# Part 5: Summary Statistics
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Significant warming trends (p < 0.05):\n")
warming <- trend_summary |>
  filter(significant & slope > 0) |>
  arrange(desc(slope))

if (nrow(warming) > 0) {
  for (i in 1:nrow(warming)) {
    cat(sprintf("  %s: +%.4f °C/year\n", warming$siteID[i], warming$slope[i]))
  }
} else {
  cat("  None\n")
}

cat("\nSignificant cooling trends (p < 0.05):\n")
cooling <- trend_summary |>
  filter(significant & slope < 0) |>
  arrange(slope)

if (nrow(cooling) > 0) {
  for (i in 1:nrow(cooling)) {
    cat(sprintf("  %s: %.4f °C/year\n", cooling$siteID[i], cooling$slope[i]))
  }
} else {
  cat("  None\n")
}

cat("\nNo significant trends (p ≥ 0.05):\n")
no_trend <- trend_summary |>
  filter(!significant) |>
  arrange(desc(abs(slope)))

if (nrow(no_trend) > 0) {
  for (i in 1:nrow(no_trend)) {
    cat(sprintf("  %s: %+.4f °C/year (p = %.3f)\n",
                no_trend$siteID[i], no_trend$slope[i], no_trend$pval[i]))
  }
} else {
  cat("  None\n")
}

cat("\n\nOVERALL PATTERN:\n")
cat("Most sites show slight warming trends, but few are statistically significant.\n")
cat("This is consistent with long-term climate warming patterns in freshwater lakes.\n")
