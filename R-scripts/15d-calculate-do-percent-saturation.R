# Calculate Dissolved Oxygen Percent Saturation
# Purpose: Compute DO percent saturation from measured DO concentration and temperature
#          using Garcia-Gordon equation for freshwater systems
# Date: 2026-05-03
#
# INPUTS:
#   - data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv
#     (Daily DO concentration: meanDO, maxDO, minDO in mg/L)
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Daily temperature: meanTemp, maxTemp, minTemp in °C)
#
# OUTPUTS:
#   - data-processed/dissolved_oxygen_with_saturation.csv
#     (Daily DO data with calculated percent saturation columns)
#
# METHOD:
#   Garcia-Gordon Equation for freshwater DO saturation
#   Widely used in limnology and aquatic ecology
#   Valid range: 0-35°C (covers all freshwater lake scenarios)

library(tidyverse)
library(readr)
library(lubridate)

# ============================================================================
# Part 1: Load Data
# ============================================================================

cat("Loading dissolved oxygen and temperature data...\n\n")

# Load DO concentration data
do_conc <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv") |>
  select(siteID, date, meanDO, maxDO, minDO) |>
  mutate(date = as.Date(date))

cat("DO concentration data:\n")
cat("  Records:", nrow(do_conc), "\n")
cat("  Sites:", n_distinct(do_conc$siteID), "\n")
cat("  Date range:", min(do_conc$date), "to", max(do_conc$date), "\n\n")

# Load temperature data
temp_data <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv") |>
  select(siteID, date, meanTemp, maxTemp, minTemp) |>
  mutate(date = as.Date(date))

cat("Temperature data:\n")
cat("  Records:", nrow(temp_data), "\n")
cat("  Sites:", n_distinct(temp_data$siteID), "\n")
cat("  Date range:", min(temp_data$date), "to", max(temp_data$date), "\n\n")

# ============================================================================
# Part 2: Merge DO and Temperature by Site and Date
# ============================================================================

cat("Merging DO and temperature data...\n\n")

do_temp_merged <- do_conc |>
  left_join(temp_data, by = c("siteID", "date"))

cat("Merged dataset:\n")
cat("  Records:", nrow(do_temp_merged), "\n")
cat("  Complete cases (both DO and temp):", sum(!is.na(do_temp_merged$meanDO) & !is.na(do_temp_merged$meanTemp)), "\n\n")

# ============================================================================
# Part 3: Calculate DO Saturation at Each Temperature
# ============================================================================

cat("Calculating DO saturation using Garcia-Gordon equation...\n\n")

# Garcia-Gordon equation for freshwater DO saturation (mg/L)
# Reference: Garcia, H. E., and L. I. Gordon, 1992.
# Oxygen solubility in seawater: Better fitting equations.
# Limnol. Oceanogr., 37(6): 1307-1312.
#
# For freshwater (salinity = 0):
# ln(DO_sat) = A0 + A1*(100/T) + A2*ln(100/T) + A3*(100/T)^2
# where T is absolute temperature (K)

# Coefficients for freshwater
A0 <- -173.4292
A1 <- 249.6339
A2 <- 143.3483
A3 <- -21.8492

# Function to calculate saturation DO (mg/L)
calculate_saturation_do <- function(temp_celsius) {
  # Convert to absolute temperature (Kelvin)
  T_K <- temp_celsius + 273.15

  # Garcia-Gordon equation
  ln_do_sat <- A0 + A1 * (100 / T_K) + A2 * log(100 / T_K) + A3 * (100 / T_K)^2

  # Convert back from natural log
  do_sat <- exp(ln_do_sat)

  return(do_sat)
}

# Calculate saturation DO for mean, max, and min temperatures
do_with_saturation <- do_temp_merged |>
  mutate(
    # Saturation DO concentration at each temperature (mg/L)
    meanDO_sat_conc = calculate_saturation_do(meanTemp),
    maxDO_sat_conc = calculate_saturation_do(maxTemp),
    minDO_sat_conc = calculate_saturation_do(minTemp),

    # Percent saturation: (measured / saturation) * 100
    meanDO_sat_pct = if_else(!is.na(meanDO) & !is.na(meanDO_sat_conc),
                              (meanDO / meanDO_sat_conc) * 100,
                              NA_real_),
    maxDO_sat_pct = if_else(!is.na(maxDO) & !is.na(maxDO_sat_conc),
                             (maxDO / maxDO_sat_conc) * 100,
                             NA_real_),
    minDO_sat_pct = if_else(!is.na(minDO) & !is.na(minDO_sat_conc),
                             (minDO / minDO_sat_conc) * 100,
                             NA_real_)
  )

cat("Saturation calculations complete.\n\n")

# Check results
cat("DO Percent Saturation Summary:\n")
cat("  Mean saturation (mean DO):\n")
cat("    Mean:", round(mean(do_with_saturation$meanDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    SD:", round(sd(do_with_saturation$meanDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    Range:", round(min(do_with_saturation$meanDO_sat_pct, na.rm = TRUE), 1), "-",
    round(max(do_with_saturation$meanDO_sat_pct, na.rm = TRUE), 1), "%\n\n")

cat("  Max saturation (peak DO):\n")
cat("    Mean:", round(mean(do_with_saturation$maxDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    SD:", round(sd(do_with_saturation$maxDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    Range:", round(min(do_with_saturation$maxDO_sat_pct, na.rm = TRUE), 1), "-",
    round(max(do_with_saturation$maxDO_sat_pct, na.rm = TRUE), 1), "%\n\n")

cat("  Min saturation (low DO):\n")
cat("    Mean:", round(mean(do_with_saturation$minDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    SD:", round(sd(do_with_saturation$minDO_sat_pct, na.rm = TRUE), 1), "%\n")
cat("    Range:", round(min(do_with_saturation$minDO_sat_pct, na.rm = TRUE), 1), "-",
    round(max(do_with_saturation$minDO_sat_pct, na.rm = TRUE), 1), "%\n\n")

# ============================================================================
# Part 4: Interpretation Guide
# ============================================================================

cat("\nINTERPRETATION:\n")
cat("  100% saturation = water in equilibrium with atmosphere\n")
cat("  >100% saturation = supersaturated (photosynthesis, gas injection)\n")
cat("  <100% saturation = undersaturated (respiration, warming)\n")
cat("  <50% saturation = hypoxic conditions developing\n")
cat("  <20% saturation = severe hypoxia (lethal for most aquatic organisms)\n\n")

# ============================================================================
# Part 5: Save Results
# ============================================================================

cat("Saving results...\n\n")

write_csv(do_with_saturation, "data-processed/dissolved_oxygen_with_saturation.csv")
cat("✓ Saved: data-processed/dissolved_oxygen_with_saturation.csv\n\n")

cat("Output columns:\n")
cat("  - siteID, date: Site and date identifiers\n")
cat("  - meanDO, maxDO, minDO: DO concentration (mg/L) from raw data\n")
cat("  - meanTemp, maxTemp, minTemp: Temperature (°C) from raw data\n")
cat("  - meanDO_sat_conc, maxDO_sat_conc, minDO_sat_conc:\n")
cat("    Saturation DO concentration at measured temperature (mg/L)\n")
cat("  - meanDO_sat_pct, maxDO_sat_pct, minDO_sat_pct:\n")
cat("    Percent saturation (%) - use this for comparisons across temps!\n\n")

# ============================================================================
# Part 6: Example Usage
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("NEXT STEPS:\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("1. Review the saturation percentages to understand your DO regime\n")
cat("2. For ecological analysis, use meanDO_sat_pct instead of meanDO when comparing\n")
cat("   observations from different temperatures\n")
cat("3. Update script 15 to merge this saturation data with monthly food supply data\n\n")

cat("Example interpretation:\n")
cat("  - If spring DO = 8 mg/L at 5°C (80% sat) and\n")
cat("  - Summer DO = 6 mg/L at 25°C (75% sat)\n")
cat("  - Percent saturation shows the summer condition is actually worse relative\n")
cat("    to oxygen availability (more stressed than concentration alone suggests)\n\n")
