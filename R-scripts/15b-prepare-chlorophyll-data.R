# Prepare Phytoplankton Biomass Data for Zooplankton Analysis
# Purpose: Load algal biomass (ash-free dry mass) as direct food supply indicator
# Date: 2026-05-02
#
# INPUTS:
#   - data-raw/MicroAlgae_Collection_NeonData.Robj
#     (R object containing microalgae data, extracted as NeonData$alg_biomass)
#
# OUTPUTS:
#   - data-processed/phytoplankton_biomass_monthly_summary.csv
#     (Monthly phytoplankton biomass aggregates by site)
#   - stats-tables/food_supply_phytoplankton_biomass_by_site.csv
#     (Site-level phytoplankton biomass statistics)

library(tidyverse)
library(readr)
library(lubridate)

# Create stats-tables directory if it doesn't exist
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
}

# ============================================================================
# Part 1: Load Microalgae R Object
# ============================================================================

cat("Loading microalgae chlorophyll data...\n\n")

# Load the R object
load("data-raw/MicroAlgae_Collection_NeonData.Robj")

# Extract the algal biomass data (main chlorophyll data)
cat("Objects loaded from R file:\n")
cat("  - NeonData (list with 9 elements)\n")
cat("  - Key data: NeonData$alg_biomass\n\n")

alg_biomass <- NeonData$alg_biomass

cat("Algal Biomass Data:\n")
cat("  Class:", class(alg_biomass), "\n")
cat("  Dimensions:", dim(alg_biomass), "\n\n")

# Explore structure
cat("Column names:\n")
print(colnames(alg_biomass))

cat("\n\nFirst few rows:\n")
print(head(alg_biomass, 3))

alg_sites <- alg_biomass |> 
distinct(siteID) |>
  pull(siteID) |>
  sort()




# ============================================================================
# Part 2: Prepare Chlorophyll Data
# ============================================================================

cat("\n\nPreparing chlorophyll data...\n\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

# Clean and prepare the algal biomass data
chlorophyll_raw <- alg_biomass |>
  as_tibble() |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate),
    siteID = as.character(siteID)
  ) |>
  filter(siteID %in% zoo_sites)

cat("Chlorophyll data for focal sites:\n")
cat("  Records:", nrow(chlorophyll_raw), "\n")
cat("  Sites:", n_distinct(chlorophyll_raw$siteID), "\n")
cat("  Date range:", min(chlorophyll_raw$collectDate), "to", max(chlorophyll_raw$collectDate), "\n\n")

# Check which columns might contain chlorophyll data
cat("Available columns that might contain chlorophyll:\n")
potential_chla_cols <- colnames(alg_biomass)[grepl("chl|Chl|CHL|phyc|Phyc|bio|Bio", colnames(alg_biomass), ignore.case = TRUE)]
print(potential_chla_cols)

cat("\n")

# ============================================================================
# Part 3: Create Monthly Phytoplankton Biomass Summary
# ============================================================================

cat("Creating monthly phytoplankton biomass summary...\n\n")

# Use ash-free dry mass as the primary biomass indicator
biomass_col <- "adjAshFreeDryMass"

# Check if column exists
if (biomass_col %in% colnames(chlorophyll_raw)) {
  cat("Using '", biomass_col, "' as primary phytoplankton biomass indicator\n")
  cat("(Ash-free dry mass in micrograms per liter)\n\n")

  # Create monthly summary
  biomass_monthly <- chlorophyll_raw |>
    filter(!is.na(!!sym(biomass_col))) |>
    group_by(siteID, year, month) |>
    summarise(
      n_samples = n(),
      biomass_mean = mean(!!sym(biomass_col), na.rm = TRUE),
      biomass_sd = sd(!!sym(biomass_col), na.rm = TRUE),
      biomass_min = min(!!sym(biomass_col), na.rm = TRUE),
      biomass_max = max(!!sym(biomass_col), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(siteID, year, month)

  cat("Monthly phytoplankton biomass summary:\n")
  cat("  Shape:", nrow(biomass_monthly), "site-month combinations\n")
  cat("  Non-empty records:", sum(!is.na(biomass_monthly$biomass_mean)), "\n\n")

  # Save monthly summary
  write_csv(biomass_monthly, "data-processed/phytoplankton_biomass_monthly_summary.csv")
  cat("✓ Saved: data-processed/phytoplankton_biomass_monthly_summary.csv\n\n")

  # Create site-level summary
  biomass_by_site <- chlorophyll_raw |>
    filter(!is.na(!!sym(biomass_col))) |>
    group_by(siteID) |>
    summarise(
      n_samples = n(),
      biomass_mean = mean(!!sym(biomass_col), na.rm = TRUE),
      biomass_sd = sd(!!sym(biomass_col), na.rm = TRUE),
      biomass_min = min(!!sym(biomass_col), na.rm = TRUE),
      biomass_max = max(!!sym(biomass_col), na.rm = TRUE),
      date_min = min(collectDate, na.rm = TRUE),
      date_max = max(collectDate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(siteID)

  write_csv(biomass_by_site, "stats-tables/food_supply_phytoplankton_biomass_by_site.csv")
  cat("✓ Saved: stats-tables/food_supply_phytoplankton_biomass_by_site.csv\n\n")

} else {
  cat("⚠ Column '", biomass_col, "' not found\n")
  cat("Available columns: \n")
  print(colnames(chlorophyll_raw))

  # Save raw data for inspection
  write_csv(
    chlorophyll_raw |> head(100),
    "data-processed/algae_biomass_raw_sample.csv"
  )
  cat("✓ Saved sample data for inspection: data-processed/algae_biomass_raw_sample.csv\n\n")
}

# ============================================================================
# Part 4: Summary
# ============================================================================

cat("================================\n")
cat("PHYTOPLANKTON BIOMASS DATA PREPARED\n")
cat("================================\n")
cat("Monthly summary: data-processed/phytoplankton_biomass_monthly_summary.csv\n")
cat("Summary statistics: stats-tables/food_supply_phytoplankton_biomass_by_site.csv\n\n")
cat("DATA:\n")
cat("  - adjAshFreeDryMass: Ash-free dry mass (μg/L)\n")
cat("  - Direct measure of phytoplankton/algal biomass\n")
cat("  - Primary food resource for zooplankton\n\n")
cat("NEXT STEP:\n")
cat("  Merge biomass with existing food supply data (nutrients, DO)\n")
cat("  Update script 16 to include biomass in final analysis dataset\n\n")
