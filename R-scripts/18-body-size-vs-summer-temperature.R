# Zooplankton Body Size vs Summer Temperature by Site
# Purpose: Test hypothesis - are zooplankton larger in cooler lakes?
# Analysis: Body size across sites compared to site-specific summer temperatures (June-August)
# Date: 2026-05-02
#
# INPUTS:
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Adults-only body size summary from script 05)
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Raw daily temperature data for summer months)
#
# OUTPUTS:
#   - stats-tables/overall_body_size_temp_regression.csv
#     (Overall correlation: body size vs summer temperature across all taxa)
#   - stats-tables/taxon_body_size_temp_regressions.csv
#     (Per-taxon regressions: individual responses to temperature)
#   - stats-tables/body_size_summary_by_site.csv
#     (Site-level summaries: mean body size and summer temperature)
#   - stats-tables/analysis_summary.csv
#     (Summary of results and pattern of findings)
#   - figures/body_size_vs_summer_temp_*.png (5 visualization figures)

library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)

# Create stats-tables directory if it doesn't exist
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
  cat("Created stats-tables directory\n\n")
}

# ============================================================================
# Part 1: Load and Prepare Data
# ============================================================================

cat("Loading zooplankton body size data...\n")

zoo_body_size <- read_csv("data-processed/zooplankton_body_size_summary_adults_2014_2026.csv")

cat("Body size data loaded:\n")
cat("  Records:", nrow(zoo_body_size), "\n")
cat("  Taxa:", n_distinct(zoo_body_size$taxonID), "\n")
cat("  Sites:", n_distinct(zoo_body_size$siteID), "\n\n")

cat("Loading summer temperature data...\n")

# Load raw temperature to calculate summer temps
temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

temp_data <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) |>
  filter(month %in% c(6, 7, 8))  # Summer only

# Calculate mean summer temperature by site (across all years)
summer_temp_by_site <- temp_data |>
  group_by(siteID) |>
  summarise(
    summer_temp_mean = mean(meanTemp, na.rm = TRUE),
    summer_temp_sd = sd(meanTemp, na.rm = TRUE),
    n_summer_days = n(),
    .groups = "drop"
  ) |>
  arrange(siteID)

cat("Summer temperature summary:\n")
print(summer_temp_by_site)
cat("\n")

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

# Prepare body size data by site-taxon combination
body_size_by_site_taxon <- zoo_body_size |>
  filter(siteID %in% zoo_sites) |>
  group_by(siteID, taxonID) |>
  summarise(
    mean_length = mean(mean_body_length, na.rm = TRUE),
    max_length = mean(max_body_length, na.rm = TRUE),
    count_per_liter = mean(count_per_liter, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) |>
  left_join(summer_temp_by_site, by = "siteID")

cat("Body size by site-taxon combinations:\n")
cat("  Records:", nrow(body_size_by_site_taxon), "\n")
cat("  Unique taxa:", n_distinct(body_size_by_site_taxon$taxonID), "\n\n")

# ============================================================================
# Part 2: Overall Correlation - Body Size vs Summer Temperature
# ============================================================================

cat("Testing overall relationship: Body size vs summer temperature...\n\n")

# Remove NAs for correlation
data_complete <- body_size_by_site_taxon |>
  filter(!is.na(mean_length) & !is.na(summer_temp_mean))

cat("Sample size:", nrow(data_complete), "site-taxon combinations\n")

# Overall correlation
overall_cor <- cor(data_complete$summer_temp_mean, data_complete$mean_length, use = "complete.obs")
overall_lm <- lm(mean_length ~ summer_temp_mean, data = data_complete)
overall_summary <- summary(overall_lm)

# Save overall results
overall_results <- data.frame(
  model = "Mean Body Length ~ Summer Temperature (all taxa)",
  n_observations = nrow(data_complete),
  intercept = overall_lm$coefficients[1],
  slope = overall_lm$coefficients[2],
  r_squared = overall_summary$r.squared,
  adj_r_squared = overall_summary$adj.r.squared,
  p_value = overall_summary$coefficients[2, 4],
  correlation = overall_cor,
  pattern = if_else(overall_lm$coefficients[2] < 0, "Larger in cool sites", "Larger in warm sites")
)

write_csv(overall_results, "stats-tables/overall_body_size_temp_regression.csv")
cat("✓ Saved: stats-tables/overall_body_size_temp_regression.csv\n\n")

# ============================================================================
# Part 3: Correlation by Taxon
# ============================================================================

cat("Analyzing body size-temperature relationships by taxon...\n\n")

# Get top taxa by frequency
top_taxa <- body_size_by_site_taxon |>
  group_by(taxonID) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n)) |>
  head(10) |>
  pull(taxonID)

cat("Testing top 10 taxa by frequency\n\n")

taxon_results <- list()

for (taxon in top_taxa) {
  taxon_data <- body_size_by_site_taxon |>
    filter(taxonID == taxon, !is.na(mean_length), !is.na(summer_temp_mean))

  if (nrow(taxon_data) >= 3) {  # Need at least 3 points for meaningful correlation
    taxon_lm <- lm(mean_length ~ summer_temp_mean, data = taxon_data)
    taxon_summary <- summary(taxon_lm)
    taxon_cor <- cor(taxon_data$summer_temp_mean, taxon_data$mean_length)

    slope <- taxon_lm$coefficients[2]
    pval <- taxon_summary$coefficients[2, 4]
    r_sq <- taxon_summary$r.squared
    n_sites <- nrow(taxon_data)
    intercept <- taxon_lm$coefficients[1]

    taxon_results[[taxon]] <- data.frame(
      taxonID = taxon,
      n_sites = n_sites,
      intercept = intercept,
      slope = slope,
      r_squared = r_sq,
      p_value = pval,
      correlation = taxon_cor,
      pattern = if_else(slope < 0, "Larger in cool", "Larger in warm"),
      significant = if_else(pval < 0.05, "Yes", "No")
    )
  }
}

taxon_summary_df <- bind_rows(taxon_results) |>
  arrange(slope)

write_csv(taxon_summary_df, "stats-tables/taxon_body_size_temp_regressions.csv")
cat("✓ Saved: stats-tables/taxon_body_size_temp_regressions.csv\n\n")

# ============================================================================
# Part 4: Site-level Analysis
# ============================================================================

cat("Summarizing body size and temperature by site...\n\n")

body_size_by_site <- body_size_by_site_taxon |>
  filter(!is.na(mean_length)) |>
  group_by(siteID) |>
  summarise(
    mean_body_length = mean(mean_length, na.rm = TRUE),
    sd_body_length = sd(mean_length, na.rm = TRUE),
    min_body_length = min(mean_length, na.rm = TRUE),
    max_body_length = max(mean_length, na.rm = TRUE),
    n_taxa = n(),
    summer_temp_mean = first(summer_temp_mean),
    summer_temp_sd = first(summer_temp_sd),
    .groups = "drop"
  ) |>
  arrange(desc(summer_temp_mean))

write_csv(body_size_by_site, "stats-tables/body_size_summary_by_site.csv")
cat("✓ Saved: stats-tables/body_size_summary_by_site.csv\n\n")

# ============================================================================
# Part 5: Create Visualizations
# ============================================================================

cat("\n\nCreating visualizations...\n\n")

theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  ))

# Plot 1: Scatter plot - all taxa combined, by site
p1_overall <- data_complete |>
  ggplot(aes(x = summer_temp_mean, y = mean_length, color = siteID, size = n_samples)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2, size = 0.8) +
  labs(
    title = "Zooplankton Body Size vs Summer Temperature",
    x = "Summer Mean Temperature (°C)",
    y = "Mean Body Length (mm)",
    color = "Site",
    size = "N Samples",
    subtitle = "All taxa combined, regression line shows overall trend"
  ) +
  theme(legend.position = "right")

ggsave("figures/body_size_vs_summer_temp_overall.png", p1_overall, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_vs_summer_temp_overall.png\n")

# Plot 2: Faceted by taxon (top 6 taxa)
top_6_taxa <- taxon_summary_df |> slice(c(1:3, (n()-2):n())) |> pull(taxonID)

p2_by_taxon <- data_complete |>
  filter(taxonID %in% top_6_taxa) |>
  ggplot(aes(x = summer_temp_mean, y = mean_length, color = siteID)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.15, size = 0.8) +
  facet_wrap(~taxonID, scales = "free_y", ncol = 3) +
  labs(
    title = "Body Size vs Summer Temperature by Taxon",
    x = "Summer Mean Temperature (°C)",
    y = "Mean Body Length (mm)",
    color = "Site"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/body_size_vs_summer_temp_by_taxon.png", p2_by_taxon, width = 14, height = 8, dpi = 300)
cat("✓ Saved: figures/body_size_vs_summer_temp_by_taxon.png\n")

# Plot 3: Body size distribution at warm vs cool sites
cool_warm_threshold <- median(body_size_by_site_taxon$summer_temp_mean, na.rm = TRUE)

p3_warm_cool <- body_size_by_site_taxon |>
  filter(!is.na(mean_length), !is.na(summer_temp_mean)) |>
  mutate(
    site_type = if_else(summer_temp_mean < cool_warm_threshold, "Cool Sites", "Warm Sites")
  ) |>
  ggplot(aes(x = site_type, y = mean_length, fill = site_type)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(
    title = "Zooplankton Body Size: Warm vs Cool Sites",
    x = "",
    y = "Mean Body Length (mm)",
    subtitle = sprintf("Cool: < %.2f°C | Warm: > %.2f°C", cool_warm_threshold, cool_warm_threshold)
  ) +
  theme(legend.position = "none")

ggsave("figures/body_size_warm_vs_cool_sites.png", p3_warm_cool, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_warm_vs_cool_sites.png\n")

# Plot 4: Mean body size by site (ordered by temperature)
p4_by_site <- body_size_by_site |>
  mutate(siteID = fct_reorder(siteID, summer_temp_mean)) |>
  ggplot(aes(x = siteID, y = mean_body_length, fill = summer_temp_mean)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_body_length - sd_body_length,
                    ymax = mean_body_length + sd_body_length),
                width = 0.2, alpha = 0.7, size = 1) +
  scale_fill_gradient(low = "blue", high = "red", name = "Summer Temp (°C)") +
  labs(
    title = "Mean Zooplankton Body Size by Site",
    x = "Site (ordered by summer temperature)",
    y = "Mean Body Length (mm)",
    subtitle = "Error bars show ±1 SD; color indicates summer temperature"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/body_size_by_site_temp_gradient.png", p4_by_site, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_by_site_temp_gradient.png\n")

# Plot 5: Taxon-specific patterns (slope comparison)
if (nrow(taxon_summary_df) > 0) {
  p5_taxon_slopes <- taxon_summary_df |>
    mutate(taxonID = fct_reorder(taxonID, slope)) |>
    ggplot(aes(x = taxonID, y = slope, fill = pattern, alpha = -log10(p_value))) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    scale_fill_manual(
      values = c("Larger in cool" = "blue", "Larger in warm" = "red"),
      name = "Pattern"
    ) +
    coord_flip() +
    labs(
      title = "Taxon-Specific Responses: Body Size vs Temperature",
      x = "Taxon ID",
      y = "Slope (mm/°C)",
      alpha = "-log10(p-value)\n(larger = more significant)",
      subtitle = "Negative slope = larger in cool sites; Positive = larger in warm sites"
    )

  ggsave("figures/taxon_body_size_slopes.png", p5_taxon_slopes, width = 12, height = 8, dpi = 300)
  cat("✓ Saved: figures/taxon_body_size_slopes.png\n")
}

# ============================================================================
# Part 6: Summary Statistics and Save
# ============================================================================

cat("Creating analysis summary...\n\n")

# Create summary table
n_cool_larger <- sum(taxon_summary_df$slope < 0, na.rm = TRUE)
n_warm_larger <- sum(taxon_summary_df$slope > 0, na.rm = TRUE)
sig_taxa <- sum(taxon_summary_df$significant == "Yes", na.rm = TRUE)

summary_stats <- data.frame(
  metric = c(
    "Overall slope (mm per °C)",
    "Overall R-squared",
    "Overall p-value",
    "Pattern",
    "Taxon sample size",
    "Taxa larger in cool sites",
    "Taxa larger in warm sites",
    "Significant taxa (p<0.05)"
  ),
  value = c(
    round(overall_lm$coefficients[2], 6),
    round(overall_summary$r.squared, 4),
    format(overall_summary$coefficients[2, 4], scientific = TRUE),
    overall_results$pattern,
    nrow(taxon_summary_df),
    n_cool_larger,
    n_warm_larger,
    sig_taxa
  )
)

write_csv(summary_stats, "stats-tables/analysis_summary.csv")
cat("✓ Saved: stats-tables/analysis_summary.csv\n\n")

cat("================================\n")
cat("ANALYSIS COMPLETE\n")
cat("================================\n")
cat("Statistical results saved to stats-tables/:\n")
cat("  - overall_body_size_temp_regression.csv\n")
cat("  - taxon_body_size_temp_regressions.csv\n")
cat("  - body_size_summary_by_site.csv\n")
cat("  - analysis_summary.csv\n\n")
cat("Figures saved to figures/\n\n")
