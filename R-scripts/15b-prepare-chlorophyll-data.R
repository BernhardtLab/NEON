# Prepare Phytoplankton Biomass Data (Algal Ash-Free Dry Mass) for Zooplankton Analysis
# Purpose: Load algal ash-free dry mass (AFDM) as direct food supply indicator
#          NOTE: This is ALGAE ONLY, not total biomass; AFDM = organic matter content
# Date: 2026-05-02
#
# INPUTS:
#   - data-raw/MicroAlgae_Collection_NeonData.Robj
#     (R object containing microalgae data, extracted as NeonData$alg_biomass)
#     Contains ALGAE ONLY ash-free dry mass measurements (μg/L)
#
# OUTPUTS:
#   - data-processed/phytoplankton_afdm_monthly_summary.csv
#     (Monthly algae ash-free dry mass (AFDM) aggregates by site)
#   - stats-tables/food_supply_phytoplankton_afdm_by_site.csv
#     (Site-level algae ash-free dry mass (AFDM) statistics)

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

cat("Creating monthly algal ash-free dry mass (AFDM) summary...\n\n")

# Use ash-free dry mass as the primary algal biomass indicator
afdm_col <- "adjAshFreeDryMass"

# Check if column exists
if (afdm_col %in% colnames(chlorophyll_raw)) {
  cat("Using '", afdm_col, "' as primary algal biomass indicator\n")
  cat("(Ash-free dry mass (AFDM) = organic/live algal biomass in micrograms per liter)\n")
  cat("(Note: This is ALGAE ONLY, not total phytoplankton or other organisms)\n\n")

  # Create monthly summary
  afdm_monthly <- chlorophyll_raw |>
    filter(!is.na(!!sym(afdm_col))) |>
    group_by(siteID, year, month) |>
    summarise(
      n_samples = n(),
      afdm_mean = mean(!!sym(afdm_col), na.rm = TRUE),
      afdm_sd = sd(!!sym(afdm_col), na.rm = TRUE),
      afdm_min = min(!!sym(afdm_col), na.rm = TRUE),
      afdm_max = max(!!sym(afdm_col), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(siteID, year, month)

  cat("Monthly algal AFDM summary:\n")
  cat("  Shape:", nrow(afdm_monthly), "site-month combinations\n")
  cat("  Non-empty records:", sum(!is.na(afdm_monthly$afdm_mean)), "\n\n")

  # Save monthly summary
  write_csv(afdm_monthly, "data-processed/phytoplankton_afdm_monthly_summary.csv")
  cat("✓ Saved: data-processed/phytoplankton_afdm_monthly_summary.csv\n\n")

  # Create site-level summary
  afdm_by_site <- chlorophyll_raw |>
    filter(!is.na(!!sym(afdm_col))) |>
    group_by(siteID) |>
    summarise(
      n_samples = n(),
      afdm_mean = mean(!!sym(afdm_col), na.rm = TRUE),
      afdm_sd = sd(!!sym(afdm_col), na.rm = TRUE),
      afdm_min = min(!!sym(afdm_col), na.rm = TRUE),
      afdm_max = max(!!sym(afdm_col), na.rm = TRUE),
      date_min = min(collectDate, na.rm = TRUE),
      date_max = max(collectDate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(siteID)

  write_csv(afdm_by_site, "stats-tables/food_supply_phytoplankton_afdm_by_site.csv")
  cat("✓ Saved: stats-tables/food_supply_phytoplankton_afdm_by_site.csv\n\n")

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
cat("ALGAL ASH-FREE DRY MASS (AFDM) DATA PREPARED\n")
cat("================================\n")
cat("Monthly summary: data-processed/phytoplankton_afdm_monthly_summary.csv\n")
cat("Summary statistics: stats-tables/food_supply_phytoplankton_afdm_by_site.csv\n\n")
cat("DATA DESCRIPTION:\n")
cat("  - ORGANISM: Algae/Phytoplankton (MICROALGAE ONLY)\n")
cat("  - MEASUREMENT: adjAshFreeDryMass (AFDM)\n")
cat("  - UNITS: Micrograms per liter (μg/L)\n")
cat("  - INTERPRETATION: Organic matter content of algal cells\n")
cat("    └─ Ash-free = inorganic minerals removed\n")
cat("    └─ Dry mass = water removed\n")
cat("    └─ Direct measure of algal biomass available for zooplankton\n")
cat("  - PRIMARY USE: Direct food resource for zooplankton grazing\n\n")
cat("NEXT STEP:\n")
cat("  Merge AFDM with existing food supply data (nutrients, DO)\n")
cat("  Update script 16 to include algal AFDM in final analysis dataset\n\n")
