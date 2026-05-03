# Zooplankton Life Stage Analysis
# Purpose: Separate adults from nauplii, examine composition, and compare body size patterns
# Date: 2026-05-02

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)

# Load the raw zooplankton data
zoo_raw <- read_csv("data-processed/zooplankton_2014_2026.csv")

# ============================================================================
# Part 1: Overall Life Stage Breakdown
# ============================================================================
cat("============================================================================\n")
cat("ZOOPLANKTON LIFE STAGE ANALYSIS\n")
cat("============================================================================\n")

cat("\n1. OVERALL LIFE STAGE BREAKDOWN\n")
cat("-" * 70, "\n")

overall_breakdown <- zoo_raw |>
  mutate(life_stage = if_else(nauplii == "Y", "Nauplii (larvae)", "Adult/Copepodite")) |>
  group_by(life_stage) |>
  summarise(
    count = n(),
    pct_of_total = round(n() / nrow(zoo_raw) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(count))

print(overall_breakdown)

cat("\nNote: Missing life stage info =", nrow(zoo_raw) - sum(overall_breakdown$count), "records\n")

# ============================================================================
# Part 2: Taxa Composition - Nauplii vs. Adult
# ============================================================================
cat("\n\n2. TOP 20 TAXA: NAUPLII vs. ADULT COMPOSITION\n")
cat("-" * 70, "\n")

taxa_composition <- zoo_raw |>
  mutate(life_stage = if_else(nauplii == "Y", "Nauplii", "Adult")) |>
  group_by(taxonID, life_stage) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(names_from = life_stage, values_from = count, values_fill = 0) |>
  mutate(
    Total = Adult + Nauplii,
    Pct_Nauplii = if_else(Total > 0, round(Nauplii / Total * 100, 1), 0),
    Pct_Adult = if_else(Total > 0, round(Adult / Total * 100, 1), 0)
  ) |>
  arrange(desc(Total)) |>
  head(20)

print(taxa_composition)

# ============================================================================
# Part 3: Body Size Statistics by Life Stage
# ============================================================================
cat("\n\n3. BODY SIZE RANGES BY LIFE STAGE\n")
cat("-" * 70, "\n")

body_size_by_stage <- zoo_raw |>
  mutate(
    life_stage = if_else(nauplii == "Y", "Nauplii", "Adult"),
    mean_length = (zooMinimumLength + zooMaximumLength) / 2
  ) |>
  group_by(life_stage) |>
  summarise(
    n_records = n(),
    mean_body_length_mm = round(mean(mean_length, na.rm = TRUE), 4),
    sd_body_length_mm = round(sd(mean_length, na.rm = TRUE), 4),
    min_length_mm = round(min(zooMinimumLength, na.rm = TRUE), 4),
    max_length_mm = round(max(zooMaximumLength, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(body_size_by_stage)

# ============================================================================
# Part 4: Body Size Comparison for Top 10 Taxa
# ============================================================================
cat("\n\n4. BODY SIZE COMPARISON: TOP 10 TAXA (by frequency)\n")
cat("-" * 70, "\n")

top_taxa <- zoo_raw |>
  group_by(taxonID) |>
  summarise(total = n(), .groups = "drop") |>
  arrange(desc(total)) |>
  head(10) |>
  pull(taxonID)

for (taxon in top_taxa) {
  cat("\n", taxon, "\n", sep = "")

  taxon_comparison <- zoo_raw |>
    filter(taxonID == taxon) |>
    mutate(
      life_stage = if_else(nauplii == "Y", "Nauplii", "Adult"),
      mean_length = (zooMinimumLength + zooMaximumLength) / 2
    ) |>
    group_by(life_stage) |>
    summarise(
      n = n(),
      mean_length_mm = round(mean(mean_length, na.rm = TRUE), 4),
      sd_length = round(sd(mean_length, na.rm = TRUE), 4),
      min_length_mm = round(min(zooMinimumLength, na.rm = TRUE), 4),
      max_length_mm = round(max(zooMaximumLength, na.rm = TRUE), 4),
      .groups = "drop"
    ) |>
    arrange(life_stage)

  print(taxon_comparison)
  cat("\n")
}

# ============================================================================
# Part 5: Create Separate Datasets for Analysis
# ============================================================================
cat("\n\n5. CREATING SEPARATE DATASETS\n")
cat("-" * 70, "\n")

# Adult-only dataset
zoo_adults <- zoo_raw |>
  filter(nauplii == "N") |>
  mutate(mean_body_length = (zooMinimumLength + zooMaximumLength) / 2,
         max_body_length = zooMaximumLength)

cat("Adults-only dataset:", nrow(zoo_adults), "records\n")
cat("  Unique taxa:", n_distinct(zoo_adults$taxonID), "\n")
cat("  Unique sites:", n_distinct(zoo_adults$siteID), "\n")
cat("  Date range:", min(zoo_adults$collectDate), "to", max(zoo_adults$collectDate), "\n")

# Nauplii-only dataset
zoo_nauplii <- zoo_raw |>
  filter(nauplii == "Y") |>
  mutate(mean_body_length = (zooMinimumLength + zooMaximumLength) / 2,
         max_body_length = zooMaximumLength)

cat("\nNauplii-only dataset:", nrow(zoo_nauplii), "records\n")
cat("  Unique taxa:", n_distinct(zoo_nauplii$taxonID), "\n")
cat("  Unique sites:", n_distinct(zoo_nauplii$siteID), "\n")
cat("  Date range:", min(zoo_nauplii$collectDate), "to", max(zoo_nauplii$collectDate), "\n")

# Save the separated datasets
write_csv(zoo_adults, "data-processed/zooplankton_adults_2014_2026.csv")
cat("\n✓ Adult-only dataset saved to: data-processed/zooplankton_adults_2014_2026.csv\n")

write_csv(zoo_nauplii, "data-processed/zooplankton_nauplii_2014_2026.csv")
cat("✓ Nauplii-only dataset saved to: data-processed/zooplankton_nauplii_2014_2026.csv\n")

# ============================================================================
# Part 6: Compare Visualization Patterns - Adults vs. Mixed
# ============================================================================
cat("\n\n6. VISUALIZATION COMPARISON: All Data vs. Adults Only\n")
cat("-" * 70, "\n")

# Prepare summary datasets for comparison
# All data (mixed)
mixed_body_size <- zoo_raw |>
  mutate(
    life_stage = if_else(nauplii == "Y", "Nauplii", "Adult"),
    mean_body_length = (zooMinimumLength + zooMaximumLength) / 2
  ) |>
  group_by(siteID, collectDate, taxonID) |>
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) |>
  mutate(data_source = "All (Mixed)")

# Adults only
adults_body_size <- zoo_adults |>
  group_by(siteID, collectDate, taxonID) |>
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) |>
  mutate(data_source = "Adults Only")

# Get top 10 taxa overall
top_taxa_list <- zoo_raw |>
  group_by(taxonID) |>
  summarise(total = n(), .groups = "drop") |>
  arrange(desc(total)) |>
  head(10) |>
  pull(taxonID)

# Create comparison plot
comparison_data <- bind_rows(mixed_body_size, adults_body_size) |>
  filter(taxonID %in% top_taxa_list)

p_comparison <- ggplot(comparison_data, aes(x = collectDate, y = mean_body_length, color = data_source)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.8, size = 0.8) +
  facet_wrap(~ taxonID, scales = "free_y", ncol = 5) +
  scale_color_manual(values = c("All (Mixed)" = "#E69F00", "Adults Only" = "#0072B2"), name = "Data Source") +
  labs(
    title = "Effect of Life Stage Separation on Body Size Patterns",
    subtitle = "Orange = all data (mixed life stages), Blue = adults only",
    x = "Collection Date",
    y = "Mean Body Length (mm)"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 9, face = "bold")
  )

ggsave("figures/zooplankton_life_stage_comparison.png", p_comparison, width = 16, height = 10, dpi = 300)
cat("\n✓ Saved: figures/zooplankton_life_stage_comparison.png\n")

# ============================================================================
# Part 7: Statistical Summary
# ============================================================================
cat("\n\n7. STATISTICAL IMPACT OF LIFE STAGE SEPARATION\n")
cat("-" * 70, "\n")

# Calculate average body size by taxon for each group
mixed_avg <- zoo_raw |>
  mutate(mean_body_length = (zooMinimumLength + zooMaximumLength) / 2) |>
  group_by(taxonID) |>
  summarise(
    mixed_mean = mean(mean_body_length, na.rm = TRUE),
    mixed_sd = sd(mean_body_length, na.rm = TRUE),
    mixed_n = n(),
    .groups = "drop"
  )

adults_avg <- zoo_adults |>
  group_by(taxonID) |>
  summarise(
    adults_mean = mean(mean_body_length, na.rm = TRUE),
    adults_sd = sd(mean_body_length, na.rm = TRUE),
    adults_n = n(),
    .groups = "drop"
  )

comparison_stats <- mixed_avg |>
  left_join(adults_avg, by = "taxonID") |>
  filter(!is.na(adults_mean)) |>
  mutate(
    size_difference_pct = round((mixed_mean - adults_mean) / adults_mean * 100, 1),
    size_difference_mm = round(mixed_mean - adults_mean, 4)
  ) |>
  select(taxonID, mixed_mean, adults_mean, size_difference_mm, size_difference_pct, mixed_n, adults_n) |>
  arrange(desc(abs(size_difference_pct))) |>
  head(15)

cat("\nTop 15 taxa with largest differences between mixed vs. adults-only:\n")
print(comparison_stats)

cat("\n\nInterpretation:\n")
cat("  - Positive % = Mixed data has LARGER average sizes (nauplii inflating estimates)\n")
cat("  - Negative % = Mixed data has SMALLER average sizes (nauplii diluting estimates)\n")
cat("  - Larger differences = more important to separate by life stage\n")

cat("\n============================================================================\n")
cat("Analysis complete. Two new datasets created:\n")
cat("  - data-processed/zooplankton_adults_2014_2026.csv\n")
cat("  - data-processed/zooplankton_nauplii_2014_2026.csv\n")
cat("============================================================================\n")
