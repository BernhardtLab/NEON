# Zooplankton Maximum Body Size Data Visualization - ADULTS ONLY
# Purpose: Create visualizations of zooplankton MAXIMUM body size patterns across sites and time
#          Using ADULTS ONLY (excludes nauplii/larvae to avoid life stage confounding)
# Date: 2026-05-02
#
# INPUTS:
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Sample-level body size summary from script 05)
#
# OUTPUTS:
#   - figures/zooplankton_timeseries_by_site_max_adults_2014_2026.png
#   - figures/zooplankton_body_size_distribution_max_adults_2014_2026.png
#   - figures/zooplankton_temporal_patterns_max_adults_2014_2026.png
#   - figures/zooplankton_density_vs_body_size_max_adults_2014_2026.png
#   - figures/zooplankton_site_comparison_max_adults_2014_2026.png
#   - figures/zooplankton_body_size_across_sites_max_adults_2014_2026.png
#   - figures/zooplankton_mean_vs_max_by_taxon_adults_2014_2026.png
#   (7 PNG visualization files showing maximum body size patterns and comparisons)

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)

# Load the ADULTS-ONLY zooplankton body size summary data (2014-2026)
zoo_body_size_summary <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv")

# Get the top 10 most abundant taxa for consistent visualization across plots
top_taxa <- zoo_body_size_summary |>
  group_by(taxonID) |>
  summarise(total_samples = n(), .groups = "drop") |>
  arrange(desc(total_samples)) |>
  head(10) |>
  pull(taxonID)

# Set up ggplot theme for consistency
theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 13, face = "bold", hjust = 0)
  ))

# Define color palette for taxa
taxa_colors <- colorRampPalette(c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                                   "#66a61e", "#e6ab02", "#a6761d", "#666666",
                                   "#1f77b4", "#ff7f0e"))(length(top_taxa))
names(taxa_colors) <- top_taxa

# ============================================================================
# Plot 1: Time Series by Site (Maximum Body Length)
# ============================================================================
p1_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa) |>
  group_by(siteID, collectDate, taxonID) |>
  summarise(max_body_length = mean(max_body_length, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(p1_data, aes(x = collectDate, y = max_body_length, color = taxonID, group = taxonID)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_point(alpha = 0.5, size = 2) +
  facet_wrap(~ siteID, scales = "free_y", ncol = 2) +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Maximum Body Size Time Series by Site",
    x = "Collection Date",
    y = "Maximum Body Length (mm)",
    subtitle = "Top 10 taxa"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_timeseries_by_site_max_adults_2014_2026.png", p1, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/zooplankton_timeseries_by_site_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 2: Body Size Distribution by Taxon (Maximum Body Length)
# ============================================================================
p2_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa)

p2 <- ggplot(p2_data, aes(x = reorder(taxonID, max_body_length, FUN = median),
                           y = max_body_length, fill = taxonID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  scale_fill_manual(values = taxa_colors, guide = "none") +
  coord_flip() +
  labs(
    title = "Zooplankton Maximum Body Size Distribution by Taxon",
    x = "Taxon ID",
    y = "Maximum Body Length (mm)",
    subtitle = "Top 10 taxa; points show individual samples"
  )

ggsave("figures/zooplankton_body_size_distribution_max_adults_2014_2026.png", p2, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_body_size_distribution_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 3: Temporal Patterns (Maximum Body Size vs. Date)
# ============================================================================
p3_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa)

p3 <- ggplot(p3_data, aes(x = collectDate, y = max_body_length, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.1, size = 0.8) +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Maximum Body Size Temporal Patterns",
    x = "Collection Date",
    y = "Maximum Body Length (mm)",
    subtitle = "Points show individual samples; lines are LOESS smooths"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_temporal_patterns_max_adults_2014_2026.png", p3, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_temporal_patterns_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 4: Density vs. Maximum Body Size
# ============================================================================
p4_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa, !is.na(count_per_liter), count_per_liter > 0)

p4 <- ggplot(p4_data, aes(x = max_body_length, y = count_per_liter, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_y_log10() +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton: Density vs. Maximum Body Size",
    x = "Maximum Body Length (mm)",
    y = "Count per Liter (log scale)",
    subtitle = "Relationship between organism maximum size and abundance"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_density_vs_body_size_max_adults_2014_2026.png", p4, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_density_vs_body_size_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 5: Site Comparison (Box Plot - Distribution of Maximum Body Sizes)
# ============================================================================
p5_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa)

p5 <- ggplot(p5_data, aes(x = siteID, y = max_body_length, fill = taxonID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Maximum Body Size Distribution by Site",
    x = "Site ID",
    y = "Maximum Body Length (mm)",
    subtitle = "Distribution of maximum body sizes across samples for top 10 taxa"
  ) +
  theme(legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/zooplankton_site_comparison_max_adults_2014_2026.png", p5, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_site_comparison_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 6: Body Size Variation Across Sites (by Taxon, Maximum Body Length)
# ============================================================================
p6_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa)

p6 <- ggplot(p6_data, aes(x = siteID, y = max_body_length, fill = siteID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ taxonID, scales = "free_y", ncol = 5) +
  labs(
    title = "Zooplankton Maximum Body Size Variation Across Sites",
    x = "Site ID",
    y = "Maximum Body Length (mm)",
    subtitle = "Distribution of maximum body sizes for each taxon across sites (all years pooled)"
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/zooplankton_body_size_across_sites_max_adults_2014_2026.png", p6, width = 16, height = 10, dpi = 300)
cat("✓ Saved: figures/zooplankton_body_size_across_sites_max_adults_2014_2026.png\n")

# ============================================================================
# Plot 7: Comparing Mean vs. Maximum Body Length by Taxon
# ============================================================================
p7_data <- zoo_body_size_summary |>
  filter(taxonID %in% top_taxa) |>
  select(taxonID, mean_body_length, max_body_length) |>
  pivot_longer(
    cols = c(mean_body_length, max_body_length),
    names_to = "length_type",
    values_to = "body_length"
  ) |>
  mutate(length_type = factor(length_type, levels = c("mean_body_length", "max_body_length"),
                              labels = c("Mean", "Maximum")))

p7 <- ggplot(p7_data, aes(x = reorder(taxonID, body_length, FUN = median),
                           y = body_length, fill = length_type)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Mean" = "#0072B2", "Maximum" = "#D55E00"), name = "Length Type") +
  coord_flip() +
  labs(
    title = "Comparison of Mean vs. Maximum Body Length by Taxon",
    x = "Taxon ID",
    y = "Body Length (mm)",
    subtitle = "Top 10 taxa; showing distribution of both mean and maximum measurements"
  ) +
  theme(legend.text = element_text(size = 10))

ggsave("figures/zooplankton_mean_vs_max_by_taxon_adults_2014_2026.png", p7, width = 11, height = 8, dpi = 300)
cat("✓ Saved: figures/zooplankton_mean_vs_max_by_taxon_adults_2014_2026.png\n")

# ============================================================================
# Summary
# ============================================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Visualization Summary - Maximum Body Size (2014-2026 data, ADULTS ONLY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("All plots saved to figures/ folder:\n")
cat("  1. zooplankton_timeseries_by_site_max_adults_2014_2026.png\n")
cat("  2. zooplankton_body_size_distribution_max_adults_2014_2026.png\n")
cat("  3. zooplankton_temporal_patterns_max_adults_2014_2026.png\n")
cat("  4. zooplankton_density_vs_body_size_max_adults_2014_2026.png\n")
cat("  5. zooplankton_site_comparison_max_adults_2014_2026.png\n")
cat("  6. zooplankton_body_size_across_sites_max_adults_2014_2026.png\n")
cat("  7. zooplankton_mean_vs_max_by_taxon_adults_2014_2026.png\n")
cat("\nNote: Nauplii (larvae) excluded from this analysis to avoid life stage confounding\n")
cat("\nTop 10 taxa visualized:\n")
print(top_taxa)
