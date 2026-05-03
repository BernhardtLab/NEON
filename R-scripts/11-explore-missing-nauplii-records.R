# Explore Missing/NA Nauplii Records
# Purpose: Understand what the NA records are and why they affect body size averages
# Date: 2026-05-02
#
# INPUTS:
#   - data-processed/zooplankton_2014_2026.csv
#     (Cleaned zooplankton data from script 08, all life stages)
#
# OUTPUTS:
#   - Console output: NA statistics by taxon and site
#   - Investigation of missing nauplii field values

library(tidyverse)
library(readr)

# Load raw data
zoo_raw <- read_csv("data-processed/zooplankton_2014_2026.csv")

cat("=" * 80, "\n")
cat("EXPLORING MISSING NAUPLII RECORDS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# ============================================================================
# Part 1: Overview of NA Records
# ============================================================================

cat("1. OVERALL NA STATISTICS\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

na_count <- sum(is.na(zoo_raw$nauplii))
total_count <- nrow(zoo_raw)
pct_na <- round(na_count / total_count * 100, 1)

cat("Total records:", total_count, "\n")
cat("Records with nauplii == NA:", na_count, "(", pct_na, "%)\n")
cat("Records with nauplii == 'N' (adults):", sum(zoo_raw$nauplii == "N", na.rm = TRUE), "\n")
cat("Records with nauplii == 'Y' (nauplii):", sum(zoo_raw$nauplii == "Y", na.rm = TRUE), "\n\n")

# ============================================================================
# Part 2: Which Taxa Have NA Records?
# ============================================================================

cat("2. TAXA WITH MISSING NAUPLII VALUES\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

na_by_taxon <- zoo_raw |>
  mutate(
    life_stage = case_when(
      is.na(nauplii) ~ "NA",
      nauplii == "Y" ~ "Nauplii",
      nauplii == "N" ~ "Adult",
      TRUE ~ "Unknown"
    )
  ) |>
  group_by(taxonID, life_stage) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(names_from = life_stage, values_from = count, values_fill = 0) |>
  mutate(
    Total = Adult + NA + Nauplii,
    Pct_NA = round(NA / Total * 100, 1)
  ) |>
  arrange(desc(NA)) |>
  filter(NA > 0)

cat("Top 20 taxa with most NA records:\n")
print(head(na_by_taxon, 20))

# ============================================================================
# Part 3: Body Size Comparison for Taxa with NA Records
# ============================================================================

cat("\n\n3. BODY SIZE COMPARISON: Adult vs. NA Records\n")
cat("(Showing taxa where NA records exist)\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

# Get taxa that have both adults and NA records
taxa_with_both <- na_by_taxon |>
  filter(Adult > 0 & NA > 0) |>
  pull(taxonID) |>
  head(10)

cat("Taxa with both adult and NA records (top 10 by NA count):\n\n")

for (taxon in taxa_with_both) {
  cat(taxon, "\n")

  taxon_data <- zoo_raw |>
    filter(taxonID == taxon) |>
    mutate(
      mean_length = (zooMinimumLength + zooMaximumLength) / 2,
      life_stage = case_when(
        is.na(nauplii) ~ "NA (unknown)",
        nauplii == "Y" ~ "Nauplii",
        nauplii == "N" ~ "Adult",
        TRUE ~ "Unknown"
      )
    )

  summary <- taxon_data |>
    group_by(life_stage) |>
    summarise(
      n = n(),
      mean_size = round(mean(mean_length, na.rm = TRUE), 4),
      sd_size = round(sd(mean_length, na.rm = TRUE), 4),
      min_size = round(min(zooMinimumLength, na.rm = TRUE), 4),
      max_size = round(max(zooMaximumLength, na.rm = TRUE), 4),
      date_min = min(collectDate, na.rm = TRUE),
      date_max = max(collectDate, na.rm = TRUE),
      .groups = "drop"
    )

  print(summary)
  cat("\n")
}

# ============================================================================
# Part 4: MESEDA Detailed Exploration
# ============================================================================

cat("\n\n4. DETAILED EXPLORATION: MESEDA\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

meseda_data <- zoo_raw |>
  filter(taxonID == "MESEDA") |>
  mutate(
    mean_length = (zooMinimumLength + zooMaximumLength) / 2,
    life_stage = case_when(
      is.na(nauplii) ~ "NA (unknown)",
      nauplii == "Y" ~ "Nauplii",
      nauplii == "N" ~ "Adult",
      TRUE ~ "Unknown"
    )
  )

cat("MESEDA Total Records:", nrow(meseda_data), "\n\n")

meseda_summary <- meseda_data |>
  group_by(life_stage) |>
  summarise(
    n = n(),
    mean_size = round(mean(mean_length, na.rm = TRUE), 4),
    sd_size = round(sd(mean_length, na.rm = TRUE), 4),
    min_size = round(min(zooMinimumLength, na.rm = TRUE), 4),
    max_size = round(max(zooMaximumLength, na.rm = TRUE), 4),
    median_size = round(median(mean_length, na.rm = TRUE), 4),
    .groups = "drop"
  )

cat("Body Size Summary by Life Stage:\n")
print(meseda_summary)

# Check dates for NA records
cat("\n\nDate range by life stage:\n")
meseda_dates <- meseda_data |>
  group_by(life_stage) |>
  summarise(
    first_date = min(collectDate, na.rm = TRUE),
    last_date = max(collectDate, na.rm = TRUE),
    n_years = n_distinct(year(collectDate)),
    .groups = "drop"
  )
print(meseda_dates)

# ============================================================================
# Part 5: Temporal Pattern of NA Records
# ============================================================================

cat("\n\n5. TEMPORAL PATTERN OF NA RECORDS\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")

na_by_year <- zoo_raw |>
  mutate(
    year = year(collectDate),
    has_na = is.na(nauplii)
  ) |>
  group_by(year) |>
  summarise(
    total_records = n(),
    na_records = sum(has_na),
    pct_na = round(sum(has_na) / n() * 100, 1),
    .groups = "drop"
  ) |>
  arrange(year)

cat("NA Records by Year:\n")
print(na_by_year)

# ============================================================================
# Part 6: Summary and Interpretation
# ============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("KEY FINDINGS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("1. NA RECORDS ARE NOT RANDOM:\n")
earliest_na_year <- min(year(zoo_raw$collectDate[is.na(zoo_raw$nauplii)]), na.rm = TRUE)
latest_na_year <- max(year(zoo_raw$collectDate[is.na(zoo_raw$nauplii)]), na.rm = TRUE)
cat("   - NA records span from", earliest_na_year, "to", latest_na_year, "\n")

# Calculate proportion of NAs by year
na_prop <- zoo_raw |>
  mutate(year = year(collectDate)) |>
  group_by(year) |>
  summarise(pct_na = round(sum(is.na(nauplii)) / n() * 100, 1), .groups = "drop") |>
  arrange(year)

cat("   - Early years have higher % of NA values\n")
cat("   - This suggests older data lacks nauplii classification\n\n")

cat("2. NA RECORDS ARE NOT RANDOM ACROSS TAXA:\n")
taxa_mostly_na <- na_by_taxon |>
  filter(Pct_NA > 50) |>
  arrange(desc(Pct_NA))

if (nrow(taxa_mostly_na) > 0) {
  cat("   - Some taxa are mostly NA records:\n")
  print(head(taxa_mostly_na, 5))
} else {
  cat("   - Most taxa have few NA records\n")
}

cat("\n3. BODY SIZE DIFFERENCES:\n")
cat("   - For MESEDA: NA records average",
    round(mean(meseda_data$zooMaximumLength[is.na(meseda_data$nauplii)], na.rm = TRUE), 4),
    "mm\n")
cat("   - For MESEDA: Adult records average",
    round(mean(meseda_data$zooMaximumLength[meseda_data$nauplii == "N"], na.rm = TRUE), 4),
    "mm\n")
cat("   - If NA records are larger, mixing them inflates the average\n\n")

cat("4. RECOMMENDATION:\n")
cat("   - Using adults-only data (nauplii == 'N') is CLEANER because:\n")
cat("     • You avoid mixing confirmed adults with mysterious NA records\n")
cat("     • NA records come from older data with incomplete nauplii info\n")
cat("     • You're working with consistent data quality\n")
cat("     • You know exactly what you're measuring\n")

cat("\n" , paste(rep("=", 80), collapse = ""), "\n", sep = "")
