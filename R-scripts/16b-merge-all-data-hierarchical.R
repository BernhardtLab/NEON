# Merge All Data with Hierarchical Matching for Temperature and Dissolved Oxygen
# Purpose: Create final analysis-ready dataset combining all three data sources
#   with HIERARCHICAL MATCHING for maximum data retention
#   - Zooplankton body size (monthly, adults only)
#   - Lake temperature (monthly, hierarchical fallback)
#   - Lake dissolved oxygen (monthly, hierarchical fallback)
#   - Food supply: nutrients, and algal AFDM (monthly - ALGAE ONLY)
# Date: 2026-05-03
#
# INPUTS:
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Adults-only body size summary from script 05)
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Raw daily temperature data for all months)
#   - data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv
#     (Raw daily dissolved oxygen data)
#   - data-processed/nutrients_monthly_summary.csv
#     (Monthly nutrient aggregates from script 15)
#   - data-processed/phytoplankton_afdm_monthly_summary.csv
#     (Monthly algal ash-free dry mass (AFDM) from script 15c - ALGAE ONLY, optional)
#
# OUTPUTS:
#   - data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv
#     (Full analysis-ready dataset with hierarchical temperature and DO matching)
#   - data-processed/zooplankton_analysis_complete_cases_hierarchical.csv
#     (Subset with complete cases for regression analysis)
#   - stats-tables/hierarchical_merge_matching_summary.csv
#     (Summary of matching success rates for temp and DO by site)

library(tidyverse)
library(readr)
library(lubridate)

# Create directories if needed
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
}

# ============================================================================
# Part 1: Load All Datasets
# ============================================================================

cat("Loading all datasets...\n\n")

# Zooplankton body size with date info
zoo_body_size <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv")

zoo_body_size_dated <- zoo_body_size |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  )

# Nutrients (already aggregated monthly)
nutrients_monthly <- read_csv("data-processed/nutrients_monthly_summary.csv")

cat("Datasets loaded:\n")
cat("  Body size records:", nrow(zoo_body_size), "\n")
cat("  Nutrient records:", nrow(nutrients_monthly), "\n")

# Load raw temperature and DO data for hierarchical matching
temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

temp_data <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )

cat("  Temperature records:", nrow(temp_data), "\n")

do_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv")

do_data <- do_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )

cat("  DO records:", nrow(do_data), "\n\n")

# Algal ash-free dry mass (AFDM) - if available
if (file.exists("data-processed/phytoplankton_afdm_monthly_summary.csv")) {
  afdm_monthly <- read_csv("data-processed/phytoplankton_afdm_monthly_summary.csv")
  has_afdm <- TRUE
  cat("  AFDM records:", nrow(afdm_monthly), "(Algal ash-free dry mass - ALGAE ONLY)\n\n")
} else {
  has_afdm <- FALSE
  cat("  AFDM file not found (optional)\n\n")
}

# ============================================================================
# Part 2: Create Hierarchical Lookup Tables for TEMPERATURE
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("BUILDING HIERARCHICAL TEMPERATURE LOOKUP TABLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Level 1: Exact month/year match
temp_level1 <- temp_data |>
  group_by(siteID, year, month) |>
  summarise(
    temp_mean_l1 = mean(meanTemp, na.rm = TRUE),
    temp_sd_l1 = sd(meanTemp, na.rm = TRUE),
    n_days_l1 = n(),
    .groups = "drop"
  ) |>
  mutate(temp_match_type = "exact_month_year")

cat("Temperature Level 1 (exact month/year):", nrow(temp_level1), "site-month-year combos\n")

# Level 2: Same month across all years
temp_level2 <- temp_data |>
  group_by(siteID, month) |>
  summarise(
    temp_mean_l2 = mean(meanTemp, na.rm = TRUE),
    temp_sd_l2 = sd(meanTemp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(temp_match_type = "same_month_all_years")

cat("Temperature Level 2 (same month any year):", nrow(temp_level2), "site-month combos\n\n")

# ============================================================================
# Part 3: Create Hierarchical Lookup Tables for DISSOLVED OXYGEN
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("BUILDING HIERARCHICAL DISSOLVED OXYGEN LOOKUP TABLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Level 1: Exact month/year match
do_level1 <- do_data |>
  group_by(siteID, year, month) |>
  summarise(
    do_mean_l1 = mean(meanDO, na.rm = TRUE),
    do_sd_l1 = sd(meanDO, na.rm = TRUE),
    do_max_l1 = mean(maxDO, na.rm = TRUE),
    do_min_l1 = mean(minDO, na.rm = TRUE),
    n_days_do_l1 = n(),
    .groups = "drop"
  ) |>
  mutate(do_match_type = "exact_month_year")

cat("DO Level 1 (exact month/year):", nrow(do_level1), "site-month-year combos\n")

# Level 2: Same month across all years
do_level2 <- do_data |>
  group_by(siteID, month) |>
  summarise(
    do_mean_l2 = mean(meanDO, na.rm = TRUE),
    do_sd_l2 = sd(meanDO, na.rm = TRUE),
    do_max_l2 = mean(maxDO, na.rm = TRUE),
    do_min_l2 = mean(minDO, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(do_match_type = "same_month_all_years")

cat("DO Level 2 (same month any year):", nrow(do_level2), "site-month combos\n\n")

# ============================================================================
# Part 4: Hierarchical Matching for Body Size Data
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("APPLYING HIERARCHICAL MATCHING FOR TEMPERATURE AND OXYGEN\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

zoo_filtered <- zoo_body_size_dated |>
  filter(siteID %in% zoo_sites) |>
  select(siteID, taxonID, collectDate, year, month, mean_body_length, max_body_length, count_per_liter, n_samples = count_per_liter)

# ============================================================================
# TEMPERATURE MATCHING - LEVEL 1
# ============================================================================

cat("Matching Temperature Level 1 (exact month/year)...")

zoo_with_temp <- zoo_filtered |>
  left_join(
    temp_level1 |> select(siteID, year, month, temp_mean_l1, temp_sd_l1, temp_match_type),
    by = c("siteID", "year", "month"),
    suffix = c("", "_temp_l1")
  ) |>
  mutate(
    temp_mean = temp_mean_l1,
    temp_sd = temp_sd_l1,
    temp_match_type = temp_match_type,
    temp_mean_l1 = NULL,
    temp_sd_l1 = NULL
  ) |>
  select(-ends_with("_temp_l1"))

temp_l1_matched <- sum(!is.na(zoo_with_temp$temp_mean))
cat(" ", temp_l1_matched, "matches\n")

# ============================================================================
# TEMPERATURE MATCHING - LEVEL 2
# ============================================================================

cat("Matching Temperature Level 2 (same month any year)...")

zoo_with_temp <- zoo_with_temp |>
  left_join(
    temp_level2 |> select(siteID, month, temp_mean_l2, temp_sd_l2, temp_match_type_l2 = temp_match_type),
    by = c("siteID", "month")
  ) |>
  mutate(
    temp_mean = if_else(is.na(temp_mean), temp_mean_l2, temp_mean),
    temp_sd = if_else(is.na(temp_sd), temp_sd_l2, temp_sd),
    temp_match_type = if_else(is.na(temp_match_type), temp_match_type_l2, temp_match_type)
  ) |>
  select(-temp_mean_l2, -temp_sd_l2, -temp_match_type_l2)

temp_l2_matched <- sum(!is.na(zoo_with_temp$temp_mean)) - temp_l1_matched
cat(" ", temp_l2_matched, "additional matches\n")

# ============================================================================
# DISSOLVED OXYGEN MATCHING - LEVEL 1
# ============================================================================

cat("Matching DO Level 1 (exact month/year)...")

zoo_with_temp_do <- zoo_with_temp |>
  left_join(
    do_level1 |> select(siteID, year, month, do_mean_l1, do_sd_l1, do_max_l1, do_min_l1, do_match_type),
    by = c("siteID", "year", "month"),
    suffix = c("", "_do_l1")
  ) |>
  mutate(
    do_mean = do_mean_l1,
    do_sd = do_sd_l1,
    do_max = do_max_l1,
    do_min = do_min_l1,
    do_match_type = do_match_type,
    do_mean_l1 = NULL,
    do_sd_l1 = NULL,
    do_max_l1 = NULL,
    do_min_l1 = NULL
  ) |>
  select(-ends_with("_do_l1"))

do_l1_matched <- sum(!is.na(zoo_with_temp_do$do_mean))
cat(" ", do_l1_matched, "matches\n")

# ============================================================================
# DISSOLVED OXYGEN MATCHING - LEVEL 2
# ============================================================================

cat("Matching DO Level 2 (same month any year)...")

zoo_with_temp_do <- zoo_with_temp_do |>
  left_join(
    do_level2 |> select(siteID, month, do_mean_l2, do_sd_l2, do_max_l2, do_min_l2, do_match_type_l2 = do_match_type),
    by = c("siteID", "month")
  ) |>
  mutate(
    do_mean = if_else(is.na(do_mean), do_mean_l2, do_mean),
    do_sd = if_else(is.na(do_sd), do_sd_l2, do_sd),
    do_max = if_else(is.na(do_max), do_max_l2, do_max),
    do_min = if_else(is.na(do_min), do_min_l2, do_min),
    do_match_type = if_else(is.na(do_match_type), do_match_type_l2, do_match_type)
  ) |>
  select(-do_mean_l2, -do_sd_l2, -do_max_l2, -do_min_l2, -do_match_type_l2)

do_l2_matched <- sum(!is.na(zoo_with_temp_do$do_mean)) - do_l1_matched
cat(" ", do_l2_matched, "additional matches\n\n")

# ============================================================================
# Part 5: Merge Nutrients Data
# ============================================================================

cat("Merging nutrients data...")

body_size_with_env <- zoo_with_temp_do |>
  left_join(
    nutrients_monthly,
    by = c("siteID", "year", "month")
  )

nutrients_matched <- sum(!is.na(body_size_with_env$NO3_mean))
cat(" ", nutrients_matched, "records with nutrients\n")

# ============================================================================
# Part 6: Merge Algal Ash-Free Dry Mass AFDM (if available)
# ============================================================================

if (has_afdm) {
  cat("Merging algal ash-free dry mass (AFDM per volume)...")

  body_size_with_env <- body_size_with_env |>
    left_join(
      afdm_monthly,
      by = c("siteID", "year", "month")
    )

  afdm_matched <- sum(!is.na(body_size_with_env$afdm_mean))
  cat(" ", afdm_matched, "records with AFDM\n")
  cat("  (afdm_mean = monthly mean ash-free dry mass per volume, in μg/L)\n")
}

# ============================================================================
# Part 7: Create Analysis-Ready Dataset
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("CREATING FINAL ANALYSIS DATASETS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Add date column back
final_dataset <- body_size_with_env |>
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    .after = month
  ) |>
  arrange(siteID, year, month)

cat("Final merged dataset:\n")
cat("  Total records:", nrow(final_dataset), "\n")
cat("  Variables:", ncol(final_dataset), "\n\n")

# ============================================================================
# Part 8: Data Completeness Assessment
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DATA COMPLETENESS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

completeness <- final_dataset |>
  group_by(siteID) |>
  summarise(
    n_records = n(),
    temp_coverage = round(sum(!is.na(temp_mean)) / n() * 100, 1),
    do_coverage = round(sum(!is.na(do_mean)) / n() * 100, 1),
    body_size_coverage = round(sum(!is.na(mean_body_length)) / n() * 100, 1),
    nutrients_coverage = round(sum(!is.na(NO3_mean)) / n() * 100, 1),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(completeness)

# ============================================================================
# Part 9: Temperature and DO Matching Summary
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("HIERARCHICAL MATCHING SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("TEMPERATURE MATCHING:\n")
temp_match_summary <- final_dataset |>
  filter(!is.na(temp_match_type)) |>
  group_by(siteID, temp_match_type) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = temp_match_type, values_from = n, values_fill = 0)

print(temp_match_summary)

cat("\n\nDISSOLVED OXYGEN MATCHING:\n")
do_match_summary <- final_dataset |>
  filter(!is.na(do_match_type)) |>
  group_by(siteID, do_match_type) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = do_match_type, values_from = n, values_fill = 0)

print(do_match_summary)

# Save matching summary
matching_summary <- data.frame(
  metric = c(
    "Total body size records",
    "Temperature matched",
    "Temperature match rate",
    "DO matched",
    "DO match rate",
    "Nutrients matched",
    "Records with temp + body size",
    "Records with DO + body size"
  ),
  value = c(
    nrow(final_dataset),
    sum(!is.na(final_dataset$temp_mean)),
    sprintf("%.1f%%", sum(!is.na(final_dataset$temp_mean)) / nrow(final_dataset) * 100),
    sum(!is.na(final_dataset$do_mean)),
    sprintf("%.1f%%", sum(!is.na(final_dataset$do_mean)) / nrow(final_dataset) * 100),
    sum(!is.na(final_dataset$NO3_mean)),
    sum(!is.na(final_dataset$mean_body_length) & !is.na(final_dataset$temp_mean)),
    sum(!is.na(final_dataset$mean_body_length) & !is.na(final_dataset$do_mean))
  )
)

write_csv(matching_summary, "stats-tables/hierarchical_merge_matching_summary.csv")
cat("\n✓ Saved: stats-tables/hierarchical_merge_matching_summary.csv\n")

# ============================================================================
# Part 10: Save Final Datasets
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SAVING ANALYSIS DATASETS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Full dataset with hierarchical matching
write_csv(final_dataset, "data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv")
cat("✓ Saved: data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv\n")

# Complete cases for regression (body size + temp + DO + nutrients)
complete_cases <- final_dataset |>
  filter(
    !is.na(mean_body_length) &
    !is.na(temp_mean) &
    !is.na(do_mean) &
    !is.na(NO3_mean)
  ) |>
  select(siteID, date, year, month, mean_body_length, max_body_length,
         temp_mean, temp_sd, temp_match_type,
         do_mean, do_sd, do_max, do_min, do_match_type,
         NO3_mean, TN_mean, TP_mean, everything())

write_csv(complete_cases, "data-processed/zooplankton_analysis_complete_cases_hierarchical.csv")
cat("✓ Saved: data-processed/zooplankton_analysis_complete_cases_hierarchical.csv\n")
cat("  ├─ Complete cases:", nrow(complete_cases), "records\n")
cat("  └─ Variables:", ncol(complete_cases), "\n\n")

# ============================================================================
# Final Summary
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("HIERARCHICAL MERGE COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("KEY STATISTICS:\n")
cat(sprintf("  Total body size observations:        %d\n", nrow(final_dataset)))
cat(sprintf("  Temperature matched (hierarchical):  %d (%.1f%%)\n",
            sum(!is.na(final_dataset$temp_mean)),
            sum(!is.na(final_dataset$temp_mean)) / nrow(final_dataset) * 100))
cat(sprintf("  DO matched (hierarchical):           %d (%.1f%%)\n",
            sum(!is.na(final_dataset$do_mean)),
            sum(!is.na(final_dataset$do_mean)) / nrow(final_dataset) * 100))
cat(sprintf("  Nutrients matched:                   %d (%.1f%%)\n",
            sum(!is.na(final_dataset$NO3_mean)),
            sum(!is.na(final_dataset$NO3_mean)) / nrow(final_dataset) * 100))
cat(sprintf("  Complete cases (all variables):      %d (%.1f%%)\n\n",
            nrow(complete_cases),
            nrow(complete_cases) / nrow(final_dataset) * 100))

cat("OUTPUT DATASETS:\n")
cat("  1. zooplankton_body_size_temp_food_supply_hierarchical.csv\n")
cat("     └─ Full dataset with hierarchical temperature and DO matching\n")
cat("  2. zooplankton_analysis_complete_cases_hierarchical.csv\n")
cat("     └─ Subset for regression analysis (complete cases only)\n")
cat("  3. hierarchical_merge_matching_summary.csv\n")
cat("     └─ Summary of matching success rates by site\n\n")

cat("READY FOR ANALYSIS!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

