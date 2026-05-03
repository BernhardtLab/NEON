# Aggregate Daily DO Percent Saturation to Monthly Summaries
# Purpose: Convert daily percent saturation to monthly aggregates for merging with body size
# Date: 2026-05-03
#
# INPUTS:
#   - data-processed/dissolved_oxygen_with_saturation.csv
#     (Daily DO data with calculated percent saturation from script 15d)
#
# OUTPUTS:
#   - data-processed/dissolved_oxygen_saturation_monthly_summary.csv
#     (Monthly aggregated percent saturation, ready for merging with body size)

library(tidyverse)
library(readr)
library(lubridate)

cat("Loading daily DO saturation data...\n\n")

# Load daily saturation data (output from 15d)
do_sat_daily <- read_csv("data-processed/dissolved_oxygen_with_saturation.csv")

cat("Daily DO saturation data:\n")
cat("  Records:", nrow(do_sat_daily), "\n")
cat("  Sites:", n_distinct(do_sat_daily$siteID), "\n")
cat("  Date range:", min(do_sat_daily$date), "to", max(do_sat_daily$date), "\n\n")

# ============================================================================
# Extract Year and Month, then Aggregate to Monthly Summaries
# ============================================================================

cat("Aggregating to monthly summaries...\n\n")

do_sat_monthly <- do_sat_daily |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(siteID, year, month) |>
  summarise(
    # Mean percent saturation (primary metric)
    meanDO_sat_pct_mean = mean(meanDO_sat_pct, na.rm = TRUE),
    meanDO_sat_pct_sd = sd(meanDO_sat_pct, na.rm = TRUE),
    meanDO_sat_pct_min = min(meanDO_sat_pct, na.rm = TRUE),
    meanDO_sat_pct_max = max(meanDO_sat_pct, na.rm = TRUE),

    # Daily peak percent saturation
    maxDO_sat_pct_mean = mean(maxDO_sat_pct, na.rm = TRUE),
    maxDO_sat_pct_sd = sd(maxDO_sat_pct, na.rm = TRUE),

    # Daily minimum percent saturation
    minDO_sat_pct_mean = mean(minDO_sat_pct, na.rm = TRUE),
    minDO_sat_pct_sd = sd(minDO_sat_pct, na.rm = TRUE),

    # Sample size
    n_days = n(),
    n_valid = sum(!is.na(meanDO_sat_pct)),

    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Monthly DO saturation summary:\n")
cat("  Site-month combinations:", nrow(do_sat_monthly), "\n")
cat("  Non-empty records:", sum(!is.na(do_sat_monthly$meanDO_sat_pct_mean)), "\n\n")

# ============================================================================
# Summary Statistics
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("MONTHLY DO PERCENT SATURATION STATISTICS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Mean DO Saturation (%):\n")
cat("  Overall mean:", round(mean(do_sat_monthly$meanDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  SD:", round(sd(do_sat_monthly$meanDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  Range:", round(min(do_sat_monthly$meanDO_sat_pct_mean, na.rm = TRUE), 1), "-",
    round(max(do_sat_monthly$meanDO_sat_pct_mean, na.rm = TRUE), 1), "%\n\n")

cat("Peak DO Saturation (%):\n")
cat("  Overall mean:", round(mean(do_sat_monthly$maxDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  SD:", round(sd(do_sat_monthly$maxDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  Range:", round(min(do_sat_monthly$maxDO_sat_pct_mean, na.rm = TRUE), 1), "-",
    round(max(do_sat_monthly$maxDO_sat_pct_mean, na.rm = TRUE), 1), "%\n\n")

cat("Minimum DO Saturation (%):\n")
cat("  Overall mean:", round(mean(do_sat_monthly$minDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  SD:", round(sd(do_sat_monthly$minDO_sat_pct_mean, na.rm = TRUE), 1), "%\n")
cat("  Range:", round(min(do_sat_monthly$minDO_sat_pct_mean, na.rm = TRUE), 1), "-",
    round(max(do_sat_monthly$minDO_sat_pct_mean, na.rm = TRUE), 1), "%\n\n")

# By site
cat("\nMonthly DO Saturation by Site:\n")
by_site <- do_sat_monthly |>
  group_by(siteID) |>
  summarise(
    n_months = n(),
    sat_mean = round(mean(meanDO_sat_pct_mean, na.rm = TRUE), 1),
    sat_min = round(min(minDO_sat_pct_mean, na.rm = TRUE), 1),
    sat_max = round(max(maxDO_sat_pct_mean, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(by_site)

# ============================================================================
# Save Monthly Summary
# ============================================================================

cat("\n\nSaving monthly aggregates...\n\n")

write_csv(do_sat_monthly, "data-processed/dissolved_oxygen_saturation_monthly_summary.csv")
cat("✓ Saved: data-processed/dissolved_oxygen_saturation_monthly_summary.csv\n\n")

# ============================================================================
# Variable Descriptions for Merging
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("OUTPUT VARIABLES (ready for merging with body size data)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("MEAN DO SATURATION (primary variable):\n")
cat("  meanDO_sat_pct_mean = Monthly mean of daily mean percent saturation (%)\n")
cat("  meanDO_sat_pct_sd = SD of daily values within the month\n")
cat("  meanDO_sat_pct_min, max = Range of daily means\n\n")

cat("PEAK DO SATURATION (ecosystem productivity proxy):\n")
cat("  maxDO_sat_pct_mean = Monthly mean of daily peak saturation (%)\n")
cat("  maxDO_sat_pct_sd = Variability in daily peaks\n")
cat("  Higher values = stronger photosynthetic activity\n\n")

cat("MINIMUM DO SATURATION (hypoxia risk indicator):\n")
cat("  minDO_sat_pct_mean = Monthly mean of daily minimum saturation (%)\n")
cat("  minDO_sat_pct_sd = Variability in daily minima\n")
cat("  Values <50% indicate hypoxic conditions\n\n")

cat("SAMPLE INFO:\n")
cat("  n_days = Number of calendar days in month\n")
cat("  n_valid = Number of days with valid saturation data\n\n")

# ============================================================================
# Comparison with Concentration
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("WHY PERCENT SATURATION MATTERS FOR YOUR ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Scenario: Compare two months\n")
cat("  Spring (5°C):  DO = 11 mg/L  → Saturation ≈ 80%\n")
cat("  Summer (25°C): DO = 7 mg/L   → Saturation ≈ 75%\n\n")

cat("Using concentration (mg/L):\n")
cat("  Spring has MORE oxygen (11 > 7)\n")
cat("  Conclusion: Spring is better for zooplankton\n\n")

cat("Using percent saturation (%):\n")
cat("  Both are undersaturated\n")
cat("  Summer is relatively more undersaturated (75% vs 80%)\n")
cat("  Conclusion: Spring oxygen is better RELATIVE to what water can hold\n\n")

cat("For biological responses: Use percent saturation!\n")
cat("For absolute metabolic needs: Use concentration (mg/L)\n")
cat("For complete analysis: Use BOTH\n\n")

# ============================================================================
# Next Steps
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("NEXT STEPS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Option 1: Use saturation in analysis\n")
cat("  - Update script 16b to also merge this saturation data\n")
cat("  - Use meanDO_sat_pct_mean as oxygen availability variable\n")
cat("  - Include in Script 19 analysis\n\n")

cat("Option 2: Keep both concentration and saturation\n")
cat("  - Merge both dissolved_oxygen_monthly_summary.csv AND this file\n")
cat("  - Compare results using concentration vs saturation\n")
cat("  - More comprehensive analysis of oxygen effects\n\n")

cat("Option 3: Compare concentration vs saturation effects\n")
cat("  - Run analysis twice: once with DO concentration, once with saturation\n")
cat("  - Do zooplankton respond more strongly to one metric?\n\n")
