# Comparison: Mixed Life Stages vs. Adults-Only Analysis
# Purpose: Side-by-side comparison showing how body size patterns differ
#          when nauplii (larvae) are included vs. excluded
# Date: 2026-05-02
#
# INPUTS:
#   - data-processed/zooplankton_2014_2026.csv
#     (Cleaned zooplankton data from script 08, all life stages)
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Adults-only summary from script 05)
#
# OUTPUTS:
#   - Multiple PNG comparison figures showing mixed vs adults-only body size patterns

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(patchwork)

# ============================================================================
# Part 1: Prepare Both Datasets
# ============================================================================

cat("Preparing comparison datasets...\n\n")

# Load raw data (all records, mixed life stages)
zoo_raw <- read_csv("data-processed/zooplankton_2014_2026.csv")

# Create MIXED summary (includes both adults and nauplii)
zoo_mixed_summary <- zoo_raw |>
  mutate(mean_body_length = (zooMinimumLength + zooMaximumLength) / 2) |>
  group_by(siteID, collectDate, taxonID) |>
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    max_body_length = mean(zooMaximumLength, na.rm = TRUE),
    count_per_liter = first(countPerL),
    n_records = n(),
    .groups = "drop"
  ) |>
  mutate(
    data_source = "Mixed (Adults + Nauplii)",
    collectDate = as.Date(collectDate)
  ) |>
  arrange(siteID, collectDate, taxonID)

# Load ADULTS-ONLY summary (pre-calculated)
zoo_adults_summary <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv") |>
  mutate(
    data_source = "Adults Only",
    collectDate = as.Date(collectDate)
  )

# Get top 10 taxa (from mixed data for consistency)
top_taxa <- zoo_mixed_summary |>
  group_by(taxonID) |>
  summarise(total_samples = n(), .groups = "drop") |>
  arrange(desc(total_samples)) |>
  head(10) |>
  pull(taxonID)

cat("Top 10 taxa (by frequency in full dataset):\n")
print(top_taxa)
cat("\n")

# ============================================================================
# Part 2: Summary Statistics Comparison
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY STATISTICS: MIXED vs. ADULTS-ONLY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Overall body size statistics
cat("Overall Body Size (all taxa combined):\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

mixed_stats <- zoo_mixed_summary |>
  summarise(
    n_samples = n(),
    mean_length = round(mean(mean_body_length, na.rm = TRUE), 4),
    sd_length = round(sd(mean_body_length, na.rm = TRUE), 4),
    min_length = round(min(mean_body_length, na.rm = TRUE), 4),
    max_length = round(max(mean_body_length, na.rm = TRUE), 4)
  )

adults_stats <- zoo_adults_summary |>
  summarise(
    n_samples = n(),
    mean_length = round(mean(mean_body_length, na.rm = TRUE), 4),
    sd_length = round(sd(mean_body_length, na.rm = TRUE), 4),
    min_length = round(min(mean_body_length, na.rm = TRUE), 4),
    max_length = round(max(mean_body_length, na.rm = TRUE), 4)
  )

cat("MIXED DATA:\n")
print(mixed_stats)
cat("\nADULTS-ONLY DATA:\n")
print(adults_stats)

# Calculate the difference
cat("\nDIFFERENCE (Mixed - Adults):\n")
cat("  Sample count difference:", mixed_stats$n_samples - adults_stats$n_samples, "\n")
cat("  Mean body length difference:", round(mixed_stats$mean_length - adults_stats$mean_length, 4), "mm\n")
cat("  % difference in mean:", round((mixed_stats$mean_length - adults_stats$mean_length) / adults_stats$mean_length * 100, 1), "%\n")

# ============================================================================
# Part 3: Per-Taxon Comparison (Top 10)
# ============================================================================

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("PER-TAXON COMPARISON: Top 10 Taxa\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

mixed_by_taxon <- zoo_mixed_summary |>
  filter(taxonID %in% top_taxa) |>
  group_by(taxonID) |>
  summarise(
    n_samples_mixed = n(),
    mean_length_mixed = round(mean(mean_body_length, na.rm = TRUE), 4),
    .groups = "drop"
  )

adults_by_taxon <- zoo_adults_summary |>
  filter(taxonID %in% top_taxa) |>
  group_by(taxonID) |>
  summarise(
    n_samples_adults = n(),
    mean_length_adults = round(mean(mean_body_length, na.rm = TRUE), 4),
    .groups = "drop"
  )

taxon_comparison <- mixed_by_taxon |>
  full_join(adults_by_taxon, by = "taxonID") |>
  mutate(
    length_difference_mm = round(mean_length_mixed - mean_length_adults, 4),
    length_pct_difference = round((mean_length_mixed - mean_length_adults) / mean_length_adults * 100, 1),
    sample_difference = n_samples_mixed - n_samples_adults
  ) |>
  arrange(desc(abs(length_pct_difference)))

cat("Taxon | Mixed Samples | Mixed Mean | Adult Samples | Adult Mean | Diff (mm) | Diff (%) | Sample Diff\n")
cat(paste(rep("-", 110), collapse = ""), "\n")
for (i in 1:nrow(taxon_comparison)) {
  row <- taxon_comparison[i, ]
  cat(sprintf("%-10s | %13d | %10.4f | %13d | %10.4f | %9.4f | %8.1f | %10d\n",
              row$taxonID,
              row$n_samples_mixed,
              row$mean_length_mixed,
              row$n_samples_adults,
              row$mean_length_adults,
              row$length_difference_mm,
              row$length_pct_difference,
              row$sample_difference))
}

# ============================================================================
# Part 4: Visualization Comparison - Time Series
# ============================================================================

cat("\n\nCreating comparison visualizations...\n")

# Combine data for visualization
viz_data <- bind_rows(
  zoo_mixed_summary |>
    filter(taxonID %in% top_taxa) |>
    select(taxonID, collectDate, mean_body_length, data_source),
  zoo_adults_summary |>
    filter(taxonID %in% top_taxa) |>
    select(taxonID, collectDate, mean_body_length, data_source)
)

# Set up color palette
data_colors <- c("Mixed (Adults + Nauplii)" = "#E69F00", "Adults Only" = "#0072B2")

# Plot 1: Temporal patterns comparison
p1 <- ggplot(viz_data, aes(x = collectDate, y = mean_body_length, color = data_source)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.8, size = 0.8) +
  facet_wrap(~ taxonID, scales = "free_y", ncol = 5) +
  scale_color_manual(values = data_colors, name = "Data Source") +
  labs(
    title = "Impact of Life Stage Separation on Body Size Temporal Patterns",
    subtitle = "Orange = mixed data (adults + nauplii), Blue = adults only",
    x = "Collection Date",
    y = "Mean Body Length (mm)"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 9, face = "bold")
  )

ggsave("figures/comparison_temporal_patterns_mixed_vs_adults.png", p1, width = 16, height = 10, dpi = 300)
cat("✓ Saved: figures/comparison_temporal_patterns_mixed_vs_adults.png\n")

# Plot 2: Distribution comparison for each taxon
dist_data <- bind_rows(
  zoo_mixed_summary |>
    filter(taxonID %in% top_taxa) |>
    select(taxonID, mean_body_length, data_source),
  zoo_adults_summary |>
    filter(taxonID %in% top_taxa) |>
    select(taxonID, mean_body_length, data_source)
)

p2 <- ggplot(dist_data, aes(x = reorder(taxonID, mean_body_length, FUN = median),
                             y = mean_body_length, fill = data_source)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, position = "dodge") +
  scale_fill_manual(values = data_colors, name = "Data Source") +
  coord_flip() +
  labs(
    title = "Distribution Comparison: Mixed vs. Adults-Only Data",
    x = "Taxon ID",
    y = "Mean Body Length (mm)",
    subtitle = "Side-by-side box plots showing how including nauplii affects size distributions"
  ) +
  theme_cowplot() +
  theme(legend.position = "bottom")

ggsave("figures/comparison_distributions_mixed_vs_adults.png", p2, width = 12, height = 8, dpi = 300)
cat("✓ Saved: figures/comparison_distributions_mixed_vs_adults.png\n")

# ============================================================================
# Part 5: Key Findings Summary
# ============================================================================

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("KEY FINDINGS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("1. SAMPLE SIZE IMPACT:\n")
cat("   - Mixed data: ", nrow(zoo_mixed_summary), " samples\n", sep = "")
cat("   - Adults-only: ", nrow(zoo_adults_summary), " samples\n", sep = "")
cat("   - Difference: ", nrow(zoo_mixed_summary) - nrow(zoo_adults_summary), " samples removed\n\n", sep = "")

cat("2. BODY SIZE IMPACT:\n")
cat("   - Mean length (mixed): ", round(mean(zoo_mixed_summary$mean_body_length, na.rm = TRUE), 4), " mm\n", sep = "")
cat("   - Mean length (adults): ", round(mean(zoo_adults_summary$mean_body_length, na.rm = TRUE), 4), " mm\n", sep = "")
cat("   - Overall difference: ", round(mean(zoo_mixed_summary$mean_body_length, na.rm = TRUE) - mean(zoo_adults_summary$mean_body_length, na.rm = TRUE), 4), " mm\n\n", sep = "")

# Find taxa with biggest differences
biggest_diff <- taxon_comparison |> arrange(desc(abs(length_pct_difference))) |> head(5)
cat("3. TAXA MOST AFFECTED BY NAUPLII INCLUSION:\n")
for (i in 1:nrow(biggest_diff)) {
  row <- biggest_diff[i, ]
  direction <- if_else(row$length_pct_difference > 0, "INFLATED", "DEFLATED")
  cat(sprintf("   %s: Mean %s by %.1f%% (%.4f mm)\n",
              row$taxonID, direction, abs(row$length_pct_difference), abs(row$length_difference_mm)))
}

cat("\n4. IMPLICATIONS FOR YOUR ANALYSIS:\n")
cat("   - Using mixed data risks confounding life stage composition with actual body size responses\n")
cat("   - Some taxa (like those heavily dominated by nauplii) show dramatic shifts\n")
cat("   - Adults-only approach gives cleaner signal for temperature/food supply hypothesis\n")
cat("   - Nauplii should be analyzed separately if interested in recruitment dynamics\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("Comparison analysis complete!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

