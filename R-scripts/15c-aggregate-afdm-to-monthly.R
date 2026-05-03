# Aggregate Raw AFDM Data to Monthly Summaries
# Purpose: Convert raw phytos_afdm.csv samples to monthly aggregates for analysis
# Date: 2026-05-03
#
# INPUTS:
#   - data-processed/phytos_afdm.csv
#     (Raw individual algal samples with domainFilterVolume and afdm_per_volume)
#
# OUTPUTS:
#   - data-processed/phytoplankton_afdm_monthly_summary.csv
#     (Monthly aggregated AFDM data, ready for merging with body size)

library(tidyverse)
library(readr)
library(lubridate)

cat("Loading raw AFDM data...\n\n")

afdm_raw <- read_csv("data-processed/phytos_afdm.csv")

cat("Raw AFDM data:\n")
cat("  Records:", nrow(afdm_raw), "\n")
cat("  Columns:", ncol(afdm_raw), "\n\n")

# ============================================================================
# Extract date info and clean column names
# ============================================================================

cat("Processing data...\n\n")

afdm_processed <- afdm_raw |>
  mutate(
    collectDate = as.Date(collectDate.x),
    year = year(collectDate),
    month = month(collectDate),
    siteID = siteID.x
  ) |>
  select(siteID, year, month, collectDate, afdm_per_volume, adjAshFreeDryMass, domainFilterVolume)

# ============================================================================
# Aggregate to Monthly Summaries
# ============================================================================

cat("Aggregating to monthly summaries...\n\n")

afdm_monthly <- afdm_processed |>
  filter(!is.na(afdm_per_volume)) |>
  group_by(siteID, year, month) |>
  summarise(
    n_samples = n(),
    afdm_mean = mean(afdm_per_volume, na.rm = TRUE),  # Mean ash-free dry mass PER VOLUME (μg/L)
    afdm_sd = sd(afdm_per_volume, na.rm = TRUE),      # SD of AFDM per volume
    afdm_min = min(afdm_per_volume, na.rm = TRUE),    # Min AFDM per volume
    afdm_max = max(afdm_per_volume, na.rm = TRUE),    # Max AFDM per volume
    .groups = "drop"
  ) |>
  arrange(siteID, year, month)

cat("Monthly AFDM summary:\n")
cat("  Site-month combinations:", nrow(afdm_monthly), "\n")
cat("  Non-empty records:", sum(!is.na(afdm_monthly$afdm_mean)), "\n\n")

# ============================================================================
# Save Monthly Summary
# ============================================================================

write_csv(afdm_monthly, "data-processed/phytoplankton_afdm_monthly_summary.csv")
cat("✓ Saved: data-processed/phytoplankton_afdm_monthly_summary.csv\n\n")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("AGGREGATION COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Output:\n")
cat("  File: phytoplankton_afdm_monthly_summary.csv\n")
cat("  Records:", nrow(afdm_monthly), "\n")
cat("  Variables: siteID, year, month, n_samples, afdm_mean, afdm_sd, afdm_min, afdm_max\n\n")

cat("DATA INTERPRETATION:\n")
cat("  ✓ afdm_mean = Monthly mean ash-free dry mass PER VOLUME (μg/L)\n")
cat("  ✓ Calculated from domainFilterVolume (actual volume filtered)\n")
cat("  ✓ Ready for merging with zooplankton body size as food supply proxy\n\n")

cat("Next step: Run script 16b to merge with body size and temperature data\n")
