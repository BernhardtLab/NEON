# Zooplankton Body Size Data Visualization
# Purpose: Create visualizations of zooplankton body size patterns across sites and time
# Date: 2026-05-02

# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)

# Load the zooplankton body size summary data
zoo_body_size_summary <- read_csv("data-processed/zooplankton_body_size_summary.csv")

# Get the top 10 most abundant taxa for consistent visualization across plots
top_taxa <- zoo_body_size_summary %>%
  group_by(taxonID) %>%
  summarise(total_samples = n(), .groups = "drop") %>%
  arrange(desc(total_samples)) %>%
  head(10) %>%
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
# Plot 1: Time Series by Site
# ============================================================================
p1_data <- zoo_body_size_summary %>%
  filter(taxonID %in% top_taxa) %>%
  group_by(siteID, collectDate, taxonID) %>%
  summarise(mean_body_length = mean(mean_body_length, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(p1_data, aes(x = collectDate, y = mean_body_length, color = taxonID, group = taxonID)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_point(alpha = 0.5, size = 2) +
  facet_wrap(~ siteID, scales = "free_y", ncol = 2) +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Body Size Time Series by Site",
    x = "Collection Date",
    y = "Mean Body Length (mm)",
    subtitle = "Top 10 taxa"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_timeseries_by_site.png", p1, width = 14, height = 10, dpi = 300)
cat("✓ Saved: figures/zooplankton_timeseries_by_site.png\n")

# ============================================================================
# Plot 2: Body Size Distribution by Taxon
# ============================================================================
p2_data <- zoo_body_size_summary %>%
  filter(taxonID %in% top_taxa)

p2 <- ggplot(p2_data, aes(x = reorder(taxonID, mean_body_length, FUN = median),
                           y = mean_body_length, fill = taxonID)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  scale_fill_manual(values = taxa_colors, guide = "none") +
  coord_flip() +
  labs(
    title = "Zooplankton Body Size Distribution by Taxon",
    x = "Taxon ID",
    y = "Mean Body Length (mm)",
    subtitle = "Top 10 taxa; points show individual samples"
  )

ggsave("figures/zooplankton_body_size_distribution.png", p2, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_body_size_distribution.png\n")

# ============================================================================
# Plot 3: Temporal Patterns (Body Size vs. Date)
# ============================================================================
p3_data <- zoo_body_size_summary %>%
  filter(taxonID %in% top_taxa)

p3 <- ggplot(p3_data, aes(x = collectDate, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.1, size = 0.8) +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Body Size Temporal Patterns",
    x = "Collection Date",
    y = "Mean Body Length (mm)",
    subtitle = "Points show individual samples; lines are LOESS smooths"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_temporal_patterns.png", p3, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_temporal_patterns.png\n")

# ============================================================================
# Plot 4: Density vs. Body Size
# ============================================================================
p4_data <- zoo_body_size_summary %>%
  filter(taxonID %in% top_taxa, !is.na(count_per_liter), count_per_liter > 0)

p4 <- ggplot(p4_data, aes(x = mean_body_length, y = count_per_liter, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_y_log10() +
  scale_color_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton: Density vs. Body Size",
    x = "Mean Body Length (mm)",
    y = "Count per Liter (log scale)",
    subtitle = "Relationship between organism size and abundance"
  ) +
  theme(legend.text = element_text(size = 9))

ggsave("figures/zooplankton_density_vs_body_size.png", p4, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_density_vs_body_size.png\n")

# ============================================================================
# Plot 5: Site Comparison
# ============================================================================
p5_data <- zoo_body_size_summary %>%
  filter(taxonID %in% top_taxa) %>%
  group_by(siteID, taxonID) %>%
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    sd_body_length = sd(mean_body_length, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  )

p5 <- ggplot(p5_data, aes(x = siteID, y = mean_body_length, fill = taxonID)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = taxa_colors, name = "Taxon") +
  labs(
    title = "Zooplankton Body Size by Site",
    x = "Site ID",
    y = "Mean Body Length (mm)",
    subtitle = "Average body size for top 10 taxa across sites"
  ) +
  theme(legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/zooplankton_site_comparison.png", p5, width = 12, height = 7, dpi = 300)
cat("✓ Saved: figures/zooplankton_site_comparison.png\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n" + "="*70 + "\n")
cat("Visualization Summary\n")
cat("="*70 + "\n")
cat("All plots saved to figures/ folder:\n")
cat("  1. zooplankton_timeseries_by_site.png\n")
cat("  2. zooplankton_body_size_distribution.png\n")
cat("  3. zooplankton_temporal_patterns.png\n")
cat("  4. zooplankton_density_vs_body_size.png\n")
cat("  5. zooplankton_site_comparison.png\n")
cat("\nTop 10 taxa visualized:\n")
print(top_taxa)
