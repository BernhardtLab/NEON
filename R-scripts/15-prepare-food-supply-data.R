# Prepare Food Supply Data for Zooplankton Analysis
# Purpose: Load and prepare phytoplankton/food supply data for merging with zooplankton body size and temperature
# Date: 2026-05-02

library(tidyverse)
library(readr)
library(lubridate)

# ============================================================================
# Part 1: Load Nutrients Data (Nitrogen & Phosphorus)
# ============================================================================

cat("Loading nutrients data...\n")

nutrients_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_nutrients.csv")

cat("Nutrients data loaded:\n")
cat("  Total shape:", nrow(nutrients_raw), "records x", ncol(nutrients_raw), "columns\n")
cat("  Sites:", n_distinct(nutrients_raw$siteID), "unique sites\n")
cat("  Date range:", min(nutrients_raw$collectDate, na.rm = TRUE), "to", max(nutrients_raw$collectDate, na.rm = TRUE), "\n\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

nutrients_zoo <- nutrients_raw |>
  filter(siteID %in% zoo_sites) |>
  select(-`Unnamed: 0`, -X) |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  ) |>
  arrange(siteID, collectDate)

cat("Focal zooplankton sites coverage:\n")
cat("  Records:", nrow(nutrients_zoo), "\n")
cat("  Sites:", n_distinct(nutrients_zoo$siteID), "\n")
cat("  Date range:", min(nutrients_zoo$collectDate), "to", max(nutrients_zoo$collectDate), "\n")
cat("  Columns: NO3, NH4, TDN, TN, OrthoP, TDP, TP\n\n")

# ============================================================================
# Part 2: Load Dissolved Oxygen Data (proxy for primary productivity)
# ============================================================================

cat("Loading dissolved oxygen data...\n")

do_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv")

cat("Dissolved oxygen data loaded:\n")
cat("  Total shape:", nrow(do_raw), "records x", ncol(do_raw), "columns\n")
cat("  Sites:", n_distinct(do_raw$siteID), "unique sites\n")
cat("  Date range:", min(do_raw$date, na.rm = TRUE), "to", max(do_raw$date, na.rm = TRUE), "\n\n")

do_zoo <- do_raw |>
  filter(siteID %in% zoo_sites) |>
  select(-`Unnamed: 0`) |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) |>
  arrange(siteID, date)

cat("Focal zooplankton sites coverage:\n")
cat("  Records:", nrow(do_zoo), "\n")
cat("  Sites:", n_distinct(do_zoo$siteID), "\n")
cat("  Date range:", min(do_zoo$date), "to", max(do_zoo$date), "\n")
cat("  Columns: meanDO, maxDO, minDO, meanDOsat, maxDOsat, minDOsat\n\n")

# ============================================================================
# Part 3: Create Monthly Summaries for Food Supply Data
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("Creating monthly food supply summaries...\n\n")

# Monthly nutrients summary
nutrients_monthly <- nutrients_zoo |>
  group_by(siteID, year, month) |>
  summarise(
    n_nutrient_samples = n(),
    NO3_mean = mean(NO3, na.rm = TRUE),
    NO3_sd = sd(NO3, na.rm = TRUE),
    NH4_mean = mean(NH4, na.rm = TRUE),
    NH4_sd = sd(NH4, na.rm = TRUE),
    TDN_mean = mean(TDN, na.rm = TRUE),
    TDN_sd = sd(TDN, na.rm = TRUE),
    TN_mean = mean(TN, na.rm = TRUE),
    TN_sd = sd(TN, na.rm = TRUE),
    OrthoP_mean = mean(OrthoP, na.rm = TRUE),
    OrthoP_sd = sd(OrthoP, na.rm = TRUE),
    TDP_mean = mean(TDP, na.rm = TRUE),
    TDP_sd = sd(TDP, na.rm = TRUE),
    TP_mean = mean(TP, na.rm = TRUE),
    TP_sd = sd(TP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Nutrients monthly summary:\n")
cat("  Shape:", nrow(nutrients_monthly), "site-month combinations\n")
cat("  Coverage: ", sum(!is.na(nutrients_monthly$NO3_mean)), "non-empty records\n\n")

# Monthly dissolved oxygen summary
do_monthly <- do_zoo |>
  group_by(siteID, year, month) |>
  summarise(
    n_do_samples = n(),
    meanDO_avg = mean(meanDO, na.rm = TRUE),
    meanDO_sd = sd(meanDO, na.rm = TRUE),
    maxDO_avg = mean(maxDO, na.rm = TRUE),
    minDO_avg = mean(minDO, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Dissolved oxygen monthly summary:\n")
cat("  Shape:", nrow(do_monthly), "site-month combinations\n")
cat("  Coverage: ", sum(!is.na(do_monthly$meanDO_avg)), "non-empty records\n\n")

# ============================================================================
# Part 4: Summary Statistics by Site
# ============================================================================

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("FOOD SUPPLY SUMMARY STATISTICS BY SITE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("NUTRIENTS (by site):\n")
nutrients_by_site <- nutrients_zoo |>
  group_by(siteID) |>
  summarise(
    n_samples = n(),
    NO3_mean = round(mean(NO3, na.rm = TRUE), 4),
    TN_mean = round(mean(TN, na.rm = TRUE), 4),
    TP_mean = round(mean(TP, na.rm = TRUE), 4),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(nutrients_by_site)

cat("\n\nDISSOLVED OXYGEN (by site):\n")
do_by_site <- do_zoo |>
  group_by(siteID) |>
  summarise(
    n_samples = n(),
    meanDO_mean = round(mean(meanDO, na.rm = TRUE), 2),
    meanDO_sd = round(sd(meanDO, na.rm = TRUE), 2),
    meanDO_range = paste0(
      round(min(meanDO, na.rm = TRUE), 2), " - ",
      round(max(meanDO, na.rm = TRUE), 2)
    ),
    .groups = "drop"
  ) |>
  arrange(siteID)

print(do_by_site)

# ============================================================================
# Part 5: Save Food Supply Datasets
# ============================================================================

cat("\n\nSaving food supply datasets...\n\n")

write_csv(nutrients_monthly, "data-processed/nutrients_monthly_summary.csv")
cat("✓ Saved: data-processed/nutrients_monthly_summary.csv\n")

write_csv(do_monthly, "data-processed/dissolved_oxygen_monthly_summary.csv")
cat("✓ Saved: data-processed/dissolved_oxygen_monthly_summary.csv\n")

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("FOOD SUPPLY DATA READY FOR MERGING!\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("You now have food supply data at monthly resolution:\n\n")
cat("NUTRIENTS METRICS:\n")
cat("  - NO3: Nitrate (mg/L)\n")
cat("  - NH4: Ammonium (mg/L)\n")
cat("  - TDN: Total Dissolved Nitrogen (mg/L)\n")
cat("  - TN: Total Nitrogen (mg/L)\n")
cat("  - OrthoP: Orthophosphate (mg/L)\n")
cat("  - TDP: Total Dissolved Phosphorus (mg/L)\n")
cat("  - TP: Total Phosphorus (mg/L)\n\n")

cat("DISSOLVED OXYGEN METRICS (productivity proxy):\n")
cat("  - meanDO_avg: Average daily mean DO (mg/L)\n")
cat("  - meanDO_sd: Variability in mean DO\n")
cat("  - maxDO_avg: Average daily maximum DO\n")
cat("  - minDO_avg: Average daily minimum DO\n\n")

cat("NEXT STEP:\n")
cat("  Merge nutrients + DO + temperature + body size into analysis dataset\n")
cat("  Then test: body size ~ temperature + food supply\n\n")
