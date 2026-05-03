# Prepare Temperature Data for Zooplankton Analysis
# Purpose: Load lake temperature data and create summaries matched to zooplankton sampling dates
# Date: 2026-05-02

library(tidyverse)
library(readr)
library(lubridate)

# ============================================================================
# Part 1: Load and Explore Data
# ============================================================================

cat("Loading temperature data...\n")
temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

cat("Temperature data loaded:\n")
cat("  Shape:", nrow(temp_raw), "records x", ncol(temp_raw), "columns\n")
cat("  Sites:", n_distinct(temp_raw$siteID), "sites\n")
cat("  Date range:", min(temp_raw$date), "to", max(temp_raw$date), "\n\n")

# Load zooplankton data for reference
zoo_raw <- read_csv("data-processed/zooplankton_2014_2026.csv")

cat("Zooplankton data loaded:\n")
cat("  Shape:", nrow(zoo_raw), "records x", ncol(zoo_raw), "columns\n")
cat("  Sites:", n_distinct(zoo_raw$siteID), "sites\n")
cat("  Date range:", min(zoo_raw$collectDate), "to", max(zoo_raw$collectDate), "\n\n")

# ============================================================================
# Part 2: Prepare Temperature Data
# ============================================================================

cat("Preparing temperature data...\n\n")

# Convert date to proper date format
temp_clean <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    week = week(date)
  ) |>
  arrange(siteID, date)

temp_clean |> 
  ggplot(aes(x = date, y = maxTemp, color = siteID)) + geom_point()


# Get unique sampling dates for each site from zooplankton data
zoo_sampling_dates <- zoo_raw |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  distinct(siteID, collectDate, year, month) |>
  arrange(siteID, collectDate)

cat("Unique zooplankton sampling dates by site:\n")
zoo_sampling_dates |>
  group_by(siteID) |>
  summarise(
    n_unique_dates = n_distinct(collectDate),
    first_date = min(collectDate),
    last_date = max(collectDate),
    .groups = "drop"
  ) |>
  print()

# ============================================================================
# Part 3: Create Multiple Temperature Summaries
# ============================================================================

cat("\n\nCreating temperature summaries for different time windows...\n\n")

# Summary 1: Daily temperature on exact collection date
temp_by_date <- temp_clean |>
  select(siteID, date, meanTemp, maxTemp, minTemp) |>
  rename(temp_mean_daily = meanTemp,
         temp_max_daily = maxTemp,
         temp_min_daily = minTemp) |>
  rename(collectDate = date)

# Summary 2: Weekly average (7 days prior to and including sample date)
temp_weekly <- temp_clean |>
  group_by(siteID, year, week) |>
  summarise(
    temp_mean_weekly = mean(meanTemp, na.rm = TRUE),
    temp_sd_weekly = sd(meanTemp, na.rm = TRUE),
    temp_max_weekly = max(maxTemp, na.rm = TRUE),
    temp_min_weekly = min(minTemp, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

# Summary 3: Monthly average
temp_monthly <- temp_clean |>
  group_by(siteID, year, month) |>
  summarise(
    temp_mean_monthly = mean(meanTemp, na.rm = TRUE),
    temp_sd_monthly = sd(meanTemp, na.rm = TRUE),
    temp_max_monthly = max(maxTemp, na.rm = TRUE),
    temp_min_monthly = min(minTemp, na.rm = TRUE),
    n_days_monthly = n(),
    .groups = "drop"
  )

# Summary 4: 30-day rolling window average (leading up to each collection date)
# This is more complex - calculate for each sampling date
temp_30day <- expand_grid(
  siteID = unique(zoo_sampling_dates$siteID),
  collectDate = unique(zoo_sampling_dates$collectDate)
) |>
  left_join(
    temp_clean |> select(siteID, date, meanTemp, maxTemp, minTemp),
    by = "siteID"
  ) |>
  filter(date <= collectDate & date > collectDate - 30) |>
  group_by(siteID, collectDate) |>
  summarise(
    temp_mean_30day = mean(meanTemp, na.rm = TRUE),
    temp_sd_30day = sd(meanTemp, na.rm = TRUE),
    temp_max_30day = max(maxTemp, na.rm = TRUE),
    temp_min_30day = min(minTemp, na.rm = TRUE),
    n_days_30day = n(),
    .groups = "drop"
  )

# ============================================================================
# Part 4: Merge Temperature with Zooplankton Sampling Dates
# ============================================================================

cat("Merging temperature data with zooplankton sampling schedule...\n\n")

# Create a zooplankton sampling summary
zoo_sampling_summary <- zoo_raw |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  group_by(siteID, collectDate, year, month) |>
  summarise(
    n_records = n(),
    n_taxa = n_distinct(taxonID),
    .groups = "drop"
  )

# Merge all temperature summaries
temperature_by_sampling <- zoo_sampling_summary |>
  # Daily temperature
  left_join(temp_by_date, by = c("siteID", "collectDate")) |>
  # Monthly temperature
  left_join(temp_monthly, by = c("siteID", "year", "month")) |>
  # 30-day temperature
  left_join(temp_30day, by = c("siteID", "collectDate")) |>
  arrange(siteID, collectDate)

cat("Temperature-sampling merged dataset:\n")
cat("  Shape:", nrow(temperature_by_sampling), "records x", ncol(temperature_by_sampling), "columns\n")
cat("  Columns:", paste(colnames(temperature_by_sampling), collapse = ", "), "\n\n")

# Check data completeness
cat("Data completeness for temperature-sampling dataset:\n")
temp_completeness <- temperature_by_sampling |>
  summarise(
    across(starts_with("temp_"), ~ round(sum(!is.na(.)) / n() * 100, 1), .names = "{.col}_coverage")
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_complete")

print(temp_completeness)

# ============================================================================
# Part 5: Summary Statistics
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("TEMPERATURE SUMMARY STATISTICS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# By site
cat("Temperature ranges by site (from daily data):\n")
temp_by_site <- temp_clean |>
  group_by(siteID) |>
  summarise(
    mean_temp = round(mean(meanTemp, na.rm = TRUE), 2),
    sd_temp = round(sd(meanTemp, na.rm = TRUE), 2),
    min_temp = round(min(minTemp, na.rm = TRUE), 2),
    max_temp = round(max(maxTemp, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(temp_by_site)

# ============================================================================
# Part 6: Save Temperature Datasets
# ============================================================================

cat("\n\nSaving temperature datasets...\n\n")

# Save daily temperature data
write_csv(temp_by_date, "data-processed/temperature_daily.csv")
cat("✓ Saved: data-processed/temperature_daily.csv\n")

# Save monthly temperature data
write_csv(temp_monthly, "data-processed/temperature_monthly.csv")
cat("✓ Saved: data-processed/temperature_monthly.csv\n")

# Save 30-day rolling window
write_csv(temp_30day, "data-processed/temperature_30day_rolling.csv")
cat("✓ Saved: data-processed/temperature_30day_rolling.csv\n")

# Save merged temperature-sampling dataset
write_csv(temperature_by_sampling, "data-processed/zooplankton_temperature_merged.csv")
cat("✓ Saved: data-processed/zooplankton_temperature_merged.csv\n")

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("NEXT STEPS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
cat("You now have temperature data summarized at multiple time scales:\n\n")
cat("1. Daily temperature - use for immediate environment at sampling time\n")
cat("2. Weekly temperature - use for short-term thermal conditions\n")
cat("3. Monthly temperature - use for seasonal patterns\n")
cat("4. 30-day rolling - use for recent thermal history\n\n")
cat("All are merged with zooplankton sampling dates in:\n")
cat("  data-processed/zooplankton_temperature_merged.csv\n\n")
cat("Next: Merge with body size data and analyze relationships!\n")
