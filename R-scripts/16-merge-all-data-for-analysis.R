# Merge All Data: Body Size, Temperature, and Food Supply
# Purpose: Create final analysis-ready dataset combining:
#   - Zooplankton body size (monthly, adults only)
#   - Lake temperature (monthly)
#   - Food supply (nutrients and dissolved oxygen, monthly)
# Date: 2026-05-02

library(tidyverse)
library(readr)
library(lubridate)

# ============================================================================
# Part 1: Load All Datasets
# ============================================================================

cat("Loading all prepared datasets...\n\n")

# Body size + temperature (already merged)
body_temp <- read_csv("data-processed/body_size_temperature_analysis.csv")

cat("Body size + temperature dataset:\n")
cat("  Shape:", nrow(body_temp), "observations x", ncol(body_temp), "variables\n")
cat("  Date range:", min(body_temp$date), "to", max(body_temp$date), "\n\n")

# Nutrients
nutrients_monthly <- read_csv("data-processed/nutrients_monthly_summary.csv")

cat("Nutrients monthly summary:\n")
cat("  Shape:", nrow(nutrients_monthly), "site-month combinations\n\n")

# Dissolved oxygen
do_monthly <- read_csv("data-processed/dissolved_oxygen_monthly_summary.csv")

cat("Dissolved oxygen monthly summary:\n")
cat("  Shape:", nrow(do_monthly), "site-month combinations\n\n")

# ============================================================================
# Part 2: Merge Food Supply with Body Size + Temperature
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Merging all datasets...\n\n")

# Prepare body_temp for merging (extract year and month from date)
body_temp_for_merge <- body_temp |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  select(-date)  # Will add back after merging

# Merge nutrients
merged_nutrients <- body_temp_for_merge |>
  left_join(
    nutrients_monthly,
    by = c("siteID", "year", "month")
  )

cat("After adding nutrients:\n")
cat("  Shape:", nrow(merged_nutrients), "observations x", ncol(merged_nutrients), "variables\n")
na_nutrients <- sum(is.na(merged_nutrients$NO3_mean))
cat("  Records with nutrient data:", nrow(merged_nutrients) - na_nutrients, "out of", nrow(merged_nutrients), "\n")
cat("  Coverage:", round((nrow(merged_nutrients) - na_nutrients) / nrow(merged_nutrients) * 100, 1), "%\n\n")

# Merge dissolved oxygen
merged_all <- merged_nutrients |>
  left_join(
    do_monthly,
    by = c("siteID", "year", "month")
  )

cat("After adding dissolved oxygen:\n")
cat("  Shape:", nrow(merged_all), "observations x", ncol(merged_all), "variables\n")
na_do <- sum(is.na(merged_all$meanDO_avg))
cat("  Records with DO data:", nrow(merged_all) - na_do, "out of", nrow(merged_all), "\n")
cat("  Coverage:", round((nrow(merged_all) - na_do) / nrow(merged_all) * 100, 1), "%\n\n")

# Add date column back
final_dataset <- merged_all |>
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    .after = month
  ) |>
  arrange(siteID, year, month)

cat("Final merged dataset:\n")
cat("  Shape:", nrow(final_dataset), "observations x", ncol(final_dataset), "variables\n\n")

# ============================================================================
# Part 3: Data Completeness Assessment
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DATA COMPLETENESS BY SITE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

completeness <- final_dataset |>
  group_by(siteID) |>
  summarise(
    n_records = n(),
    temp_coverage = round(sum(!is.na(temp_mean_monthly)) / n() * 100, 1),
    body_size_coverage = round(sum(!is.na(mean_body_length)) / n() * 100, 1),
    nutrients_coverage = round(sum(!is.na(NO3_mean)) / n() * 100, 1),
    do_coverage = round(sum(!is.na(meanDO_avg)) / n() * 100, 1),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(completeness)

# ============================================================================
# Part 4: Explore Variables for Analysis
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("AVAILABLE VARIABLES FOR ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("DEPENDENT VARIABLE (Zooplankton Body Size):\n")
cat("  - mean_body_length: Mean body length across taxa and samples (mm)\n")
cat("  - max_body_length: Average maximum body length\n\n")

cat("TEMPERATURE VARIABLES:\n")
cat("  - temp_mean_monthly: Monthly mean temperature (°C)\n")
cat("  - temp_sd_monthly: Monthly variability in temperature\n")
cat("  - temp_max_monthly: Monthly maximum temperature\n")
cat("  - temp_min_monthly: Monthly minimum temperature\n\n")

cat("FOOD SUPPLY VARIABLES - Nutrients:\n")
cat("  - NO3_mean: Nitrate concentration (mg/L)\n")
cat("  - NH4_mean: Ammonium concentration (mg/L)\n")
cat("  - TDN_mean: Total dissolved nitrogen (mg/L)\n")
cat("  - TN_mean: Total nitrogen (mg/L)\n")
cat("  - OrthoP_mean: Orthophosphate concentration (mg/L)\n")
cat("  - TDP_mean: Total dissolved phosphorus (mg/L)\n")
cat("  - TP_mean: Total phosphorus (mg/L)\n\n")

cat("FOOD SUPPLY VARIABLES - Productivity:\n")
cat("  - meanDO_avg: Average dissolved oxygen (mg/L, proxy for photosynthesis)\n")
cat("  - meanDO_sd: Variability in dissolved oxygen\n")
cat("  - maxDO_avg: Average daily maximum DO\n")
cat("  - minDO_avg: Average daily minimum DO\n\n")

cat("SAMPLE SIZE VARIABLES:\n")
cat("  - n_samples: Number of zooplankton samples in month\n")
cat("  - n_taxa: Number of taxa in samples\n")
cat("  - mean_count_per_liter: Zooplankton abundance\n\n")

# ============================================================================
# Part 5: Summary Statistics for Analysis Variables
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY STATISTICS FOR KEY ANALYSIS VARIABLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

analysis_vars <- final_dataset |>
  filter(!is.na(mean_body_length) & !is.na(temp_mean_monthly) & !is.na(NO3_mean)) |>
  select(
    mean_body_length, temp_mean_monthly, TN_mean, TP_mean,
    meanDO_avg, mean_count_per_liter
  )

cat("Records with complete body size + temperature + nutrient data:", nrow(analysis_vars), "\n\n")

cat("BODY SIZE:\n")
cat("  Mean:", round(mean(analysis_vars$mean_body_length, na.rm = TRUE), 4), "mm\n")
cat("  SD:", round(sd(analysis_vars$mean_body_length, na.rm = TRUE), 4), "mm\n")
cat("  Range:", round(min(analysis_vars$mean_body_length, na.rm = TRUE), 4), "-",
    round(max(analysis_vars$mean_body_length, na.rm = TRUE), 4), "mm\n\n")

cat("TEMPERATURE:\n")
cat("  Mean:", round(mean(analysis_vars$temp_mean_monthly, na.rm = TRUE), 2), "°C\n")
cat("  SD:", round(sd(analysis_vars$temp_mean_monthly, na.rm = TRUE), 2), "°C\n")
cat("  Range:", round(min(analysis_vars$temp_mean_monthly, na.rm = TRUE), 2), "-",
    round(max(analysis_vars$temp_mean_monthly, na.rm = TRUE), 2), "°C\n\n")

cat("TOTAL NITROGEN:\n")
cat("  Mean:", round(mean(analysis_vars$TN_mean, na.rm = TRUE), 4), "mg/L\n")
cat("  SD:", round(sd(analysis_vars$TN_mean, na.rm = TRUE), 4), "mg/L\n\n")

cat("TOTAL PHOSPHORUS:\n")
cat("  Mean:", round(mean(analysis_vars$TP_mean, na.rm = TRUE), 4), "mg/L\n")
cat("  SD:", round(sd(analysis_vars$TP_mean, na.rm = TRUE), 4), "mg/L\n\n")

cat("DISSOLVED OXYGEN:\n")
cat("  Mean:", round(mean(analysis_vars$meanDO_avg, na.rm = TRUE), 2), "mg/L\n")
cat("  SD:", round(sd(analysis_vars$meanDO_avg, na.rm = TRUE), 2), "mg/L\n\n")

# ============================================================================
# Part 6: Save Final Analysis Dataset
# ============================================================================

cat("\n\nSaving final analysis dataset...\n\n")

write_csv(final_dataset, "data-processed/zooplankton_body_size_temp_food_supply_analysis.csv")
cat("✓ Saved: data-processed/zooplankton_body_size_temp_food_supply_analysis.csv\n")

# Also save subset with complete cases for regression
complete_cases <- analysis_vars |>
  mutate(
    siteID = final_dataset$siteID[!is.na(final_dataset$mean_body_length) &
                                   !is.na(final_dataset$temp_mean_monthly) &
                                   !is.na(final_dataset$NO3_mean)],
    date = final_dataset$date[!is.na(final_dataset$mean_body_length) &
                              !is.na(final_dataset$temp_mean_monthly) &
                              !is.na(final_dataset$NO3_mean)]
  ) |>
  select(siteID, date, everything()) |>
  arrange(siteID, date)

write_csv(complete_cases, "data-processed/zooplankton_analysis_complete_cases.csv")
cat("✓ Saved: data-processed/zooplankton_analysis_complete_cases.csv (complete cases only)\n")

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("READY FOR ANALYSIS!\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Main dataset: data-processed/zooplankton_body_size_temp_food_supply_analysis.csv\n")
cat("Complete cases: data-processed/zooplankton_analysis_complete_cases.csv\n\n")

cat("HYPOTHESIS TESTING:\n")
cat("Test: Does zooplankton body size decrease with temperature increase and food supply decrease?\n\n")

cat("RECOMMENDED ANALYSES:\n")
cat("1. Correlation matrix of body size, temperature, nutrients, DO\n")
cat("2. Linear regression: body_size ~ temperature + nutrients + DO\n")
cat("3. Site-specific regressions to test consistency\n")
cat("4. Temporal patterns: do relationships change over time?\n")
cat("5. Interaction terms: does food supply modify temperature effect?\n\n")
