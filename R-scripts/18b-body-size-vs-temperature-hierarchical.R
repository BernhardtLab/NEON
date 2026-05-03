# Zooplankton Body Size vs Temperature - Hierarchical Matching Strategy
# Purpose: Test hypothesis with MAXIMUM data retention using fallback matching strategies
# Analysis: Body size correlated with temperature, using hierarchical matching:
#   Priority 1: Exact month/year match
#   Priority 2: Same month, average across years
#   Priority 3: Adjacent months (seasonal proxy)
#   Priority 4: Annual average for site
# Date: 2026-05-03
#
# INPUTS:
#   - data-processed/zooplankton_body_size_summary_adults_2014_2026.csv
#     (Adults-only body size summary from script 05)
#   - data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv
#     (Raw daily temperature data for all months)
#
# OUTPUTS:
#   - stats-tables/overall_body_size_temp_hierarchical_regression.csv
#   - stats-tables/taxon_body_size_temp_hierarchical_regressions.csv
#   - stats-tables/body_size_summary_by_site_hierarchical.csv
#   - stats-tables/temperature_matching_summary.csv
#     (Details on how many observations used each matching strategy)
#   - stats-tables/analysis_summary_hierarchical.csv
#   - figures/body_size_vs_hierarchical_temp_*.png

library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)

# Create directories if they don't exist
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
}
if (!dir.exists("figures")) {
  dir.create("figures", showWarnings = FALSE)
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

cat("Loading temperature data (all months)...\n")

temp_raw <- read_csv("data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv")

temp_data <- temp_raw |>
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )

# ============================================================================
# Part 2: Create Temperature Lookup Tables for Hierarchical Matching
# ============================================================================

cat("Building temperature lookup tables...\n\n")

# Level 1: Monthly by year (exact match)
monthly_temp_exact <- temp_data |>
  group_by(siteID, year, month) |>
  summarise(
    temp_mean = mean(meanTemp, na.rm = TRUE),
    temp_sd = sd(meanTemp, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) |>
  mutate(match_type = "exact_month_year")

# Level 2: Monthly average across all years (same month, any year)
monthly_temp_bymonth <- temp_data |>
  group_by(siteID, month) |>
  summarise(
    temp_mean = mean(meanTemp, na.rm = TRUE),
    temp_sd = sd(meanTemp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(match_type = "same_month_all_years")

# Level 3: Adjacent months (for seasonal proxy when month not available)
# If April not available, try Mar/May; if June missing, try May/July, etc.
adjacent_months <- data.frame(
  primary_month = 1:12,
  adjacent_1 = c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),  # previous month
  adjacent_2 = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1)    # next month
)

monthly_temp_adjacent <- temp_data |>
  group_by(siteID, month) |>
  summarise(
    temp_mean = mean(meanTemp, na.rm = TRUE),
    temp_sd = sd(meanTemp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(adjacent_month = month) |>
  mutate(match_type = "adjacent_month")

# Level 4: Annual average by site (fallback)
annual_temp_bysite <- temp_data |>
  group_by(siteID) |>
  summarise(
    temp_mean = mean(meanTemp, na.rm = TRUE),
    temp_sd = sd(meanTemp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(match_type = "site_annual_average")

# ============================================================================
# Part 3: Hierarchical Temperature Matching (Simplified & Fixed)
# ============================================================================

cat("Applying hierarchical temperature matching strategy...\n\n")

# Prepare body size data with date info
zoo_body_size_dated <- zoo_body_size |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate),
    month = month(collectDate)
  )

# Focus on zooplankton sites
zoo_sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")

zoo_filtered <- zoo_body_size_dated |>
  filter(siteID %in% zoo_sites) |>
  mutate(temp_mean = NA_real_, match_type = NA_character_)

# LEVEL 1: Try exact month/year match
cat("Level 1: Exact month/year matching...")
zoo_with_temp_hierarchical <- zoo_filtered |>
  left_join(
    monthly_temp_exact |> select(siteID, year, month, temp_mean, match_type),
    by = c("siteID", "year", "month"),
    suffix = c("", "_l1")
  ) |>
  mutate(
    temp_mean = coalesce(temp_mean_l1, temp_mean),
    match_type = coalesce(match_type_l1, match_type)
  ) |>
  select(-ends_with("_l1"))

l1_count <- sum(!is.na(zoo_with_temp_hierarchical$match_type))
cat(" ", l1_count, "matches\n")

# LEVEL 2: For remaining unmatched, try same month across all years
cat("Level 2: Same month (any year) matching...")
zoo_with_temp_hierarchical <- zoo_with_temp_hierarchical |>
  left_join(
    monthly_temp_bymonth |> select(siteID, month, temp_mean_l2 = temp_mean, match_type_l2 = match_type),
    by = c("siteID", "month")
  ) |>
  mutate(
    temp_mean = if_else(is.na(temp_mean), temp_mean_l2, temp_mean),
    match_type = if_else(is.na(match_type), match_type_l2, match_type)
  ) |>
  select(-ends_with("_l2"))

l2_count <- sum(!is.na(zoo_with_temp_hierarchical$match_type)) - l1_count
cat(" ", l2_count, "additional matches\n")

# LEVEL 3: For remaining unmatched, try adjacent months (seasonal)
cat("Level 3: Adjacent month (seasonal) matching...")

# For each unmatched record, try adjacent months
unmatched <- is.na(zoo_with_temp_hierarchical$temp_mean)
l3_count <- 0

if (sum(unmatched) > 0) {
  for (i in which(unmatched)) {
    site <- zoo_with_temp_hierarchical$siteID[i]
    month_orig <- zoo_with_temp_hierarchical$month[i]

    # Get adjacent months
    prev_month <- if_else(month_orig == 1L, 12L, month_orig - 1L)
    next_month <- if_else(month_orig == 12L, 1L, month_orig + 1L)

    # Try previous month
    prev_temp <- monthly_temp_bymonth |>
      filter(siteID == site & month == prev_month) |>
      pull(temp_mean)

    if (length(prev_temp) > 0) {
      zoo_with_temp_hierarchical$temp_mean[i] <- prev_temp[1]
      zoo_with_temp_hierarchical$match_type[i] <- "Level 3: Previous Month"
      l3_count <- l3_count + 1
      next
    }

    # Try next month
    next_temp <- monthly_temp_bymonth |>
      filter(siteID == site & month == next_month) |>
      pull(temp_mean)

    if (length(next_temp) > 0) {
      zoo_with_temp_hierarchical$temp_mean[i] <- next_temp[1]
      zoo_with_temp_hierarchical$match_type[i] <- "Level 3: Next Month"
      l3_count <- l3_count + 1
    }
  }
}

cat(" ", l3_count, "additional matches\n")

# LEVEL 4: For remaining unmatched, use site annual average (last resort)
cat("Level 4: Site annual average matching...")
zoo_with_temp_hierarchical <- zoo_with_temp_hierarchical |>
  left_join(
    annual_temp_bysite |> select(siteID, temp_mean_l4 = temp_mean, match_type_l4 = match_type),
    by = "siteID"
  ) |>
  mutate(
    temp_mean = if_else(is.na(temp_mean), temp_mean_l4, temp_mean),
    match_type = if_else(is.na(match_type), match_type_l4, match_type)
  ) |>
  select(-ends_with("_l4"))

l4_count <- sum(!is.na(zoo_with_temp_hierarchical$temp_mean)) - l1_count - l2_count - l3_count
cat(" ", l4_count, "additional matches\n")

# Summary of matching
total_records <- nrow(zoo_with_temp_hierarchical)
matched_records <- sum(!is.na(zoo_with_temp_hierarchical$temp_mean))
pct_matched <- (matched_records / total_records) * 100

cat("\n")
cat("HIERARCHICAL MATCHING RESULTS:\n")
cat("  Total records:", total_records, "\n")
cat("  Successfully matched:", matched_records, "(", round(pct_matched, 1), "% )\n")
cat("  Unmatched:", total_records - matched_records, "\n\n")

# Breakdown by matching strategy
match_breakdown <- zoo_with_temp_hierarchical |>
  filter(!is.na(match_type)) |>
  group_by(match_type) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n))

cat("Breakdown by matching strategy:\n")
for (i in 1:nrow(match_breakdown)) {
  strategy <- match_breakdown$match_type[i]
  count <- match_breakdown$n[i]
  pct <- (count / matched_records) * 100
  cat("  ", strategy, ": ", count, " (", round(pct, 1), "%)\n", sep = "")
}
cat("\n")

# ============================================================================
# Part 4: Prepare Analysis Data
# ============================================================================

# Filter to records with temperature matches
body_size_with_temp <- zoo_with_temp_hierarchical |>
  filter(!is.na(temp_mean))

write_csv(body_size_with_temp, "data-processed/body_size_with_temp_hierarchical_temp.csv")

# Aggregate by site-taxon
body_size_by_site_taxon <- body_size_with_temp |>
  group_by(siteID, taxonID) |>
  summarise(
    mean_length = mean(mean_body_length, na.rm = TRUE),
    max_length = mean(max_body_length, na.rm = TRUE),
    count_per_liter = mean(count_per_liter, na.rm = TRUE),
    matched_temp_mean = mean(temp_mean, na.rm = TRUE),
    matched_temp_sd = sd(temp_mean, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) |>
  arrange(siteID, taxonID)

cat("Body size by site-taxon combinations:\n")
cat("  Records:", nrow(body_size_by_site_taxon), "\n")
cat("  Unique taxa:", n_distinct(body_size_by_site_taxon$taxonID), "\n\n")

# ============================================================================
# Part 5: Overall Correlation Analysis
# ============================================================================

cat("Testing overall relationship: Body size vs HIERARCHICAL matched temperature...\n\n")

data_complete <- body_size_by_site_taxon |>
  filter(!is.na(mean_length) & !is.na(matched_temp_mean))

cat("Sample size:", nrow(data_complete), "site-taxon combinations\n\n")

# Overall correlation
overall_cor <- cor(data_complete$matched_temp_mean, data_complete$mean_length, use = "complete.obs")
overall_lm <- lm(mean_length ~ matched_temp_mean, data = data_complete)
overall_summary <- summary(overall_lm)

# Save overall results
overall_results <- data.frame(
  model = "Mean Body Length ~ Hierarchical Matched Temperature (all taxa)",
  matching_strategy = "Hierarchical (exact → same month → adjacent month → site average)",
  n_observations = nrow(data_complete),
  intercept = overall_lm$coefficients[1],
  slope = overall_lm$coefficients[2],
  r_squared = overall_summary$r.squared,
  adj_r_squared = overall_summary$adj.r.squared,
  p_value = overall_summary$coefficients[2, 4],
  correlation = overall_cor,
  pattern = if_else(overall_lm$coefficients[2] < 0, "Larger in cool conditions", "Larger in warm conditions")
)

write_csv(overall_results, "stats-tables/overall_body_size_temp_hierarchical_regression.csv")
cat("✓ Saved: stats-tables/overall_body_size_temp_hierarchical_regression.csv\n\n")

# ============================================================================
# Part 6: Correlation by Taxon
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
    filter(taxonID == taxon, !is.na(mean_length), !is.na(matched_temp_mean))

  if (nrow(taxon_data) >= 3) {
    taxon_lm <- lm(mean_length ~ matched_temp_mean, data = taxon_data)
    taxon_summary <- summary(taxon_lm)
    taxon_cor <- cor(taxon_data$matched_temp_mean, taxon_data$mean_length)

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

write_csv(taxon_summary_df, "stats-tables/taxon_body_size_temp_hierarchical_regressions.csv")
cat("✓ Saved: stats-tables/taxon_body_size_temp_hierarchical_regressions.csv\n\n")

# ============================================================================
# Part 7: Site-level Summary
# ============================================================================

cat("Summarizing body size and temperature by site...\n\n")

body_size_by_site <- body_size_by_site_taxon |>
  filter(!is.na(mean_length) & !is.na(matched_temp_mean)) |>
  group_by(siteID) |>
  summarise(
    mean_body_length = mean(mean_length, na.rm = TRUE),
    sd_body_length = sd(mean_length, na.rm = TRUE),
    n_taxa = n(),
    matched_temp_mean = mean(matched_temp_mean, na.rm = TRUE),
    matched_temp_sd = sd(matched_temp_mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(matched_temp_mean))

write_csv(body_size_by_site, "stats-tables/body_size_summary_by_site_hierarchical.csv")
cat("✓ Saved: stats-tables/body_size_summary_by_site_hierarchical.csv\n\n")

# ============================================================================
# Part 8: Temperature Matching Summary
# ============================================================================

cat("Creating temperature matching summary...\n\n")

matching_summary <- zoo_with_temp_hierarchical |>
  filter(!is.na(match_type)) |>
  group_by(siteID, match_type) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = match_type, values_from = n, values_fill = 0) |>
  arrange(siteID)

write_csv(matching_summary, "stats-tables/temperature_matching_summary.csv")
cat("✓ Saved: stats-tables/temperature_matching_summary.csv\n\n")

# ============================================================================
# Part 9: Visualizations
# ============================================================================

cat("Creating visualizations...\n\n")

theme_set(theme_cowplot() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  ))

# Plot 1: Overall scatter
p1_overall <- data_complete |>
  ggplot(aes(x = matched_temp_mean, y = mean_length, color = siteID, size = n_samples)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2, size = 0.8) +
  labs(
    title = "Zooplankton Body Size vs Hierarchical Matched Temperature",
    x = "Temperature (°C, hierarchical matching)",
    y = "Mean Body Length (mm)",
    color = "Site",
    size = "N Samples",
    subtitle = "Uses exact matches, seasonal averages, or site average as fallback"
  ) +
  theme(legend.position = "right")

ggsave("figures/body_size_vs_hierarchical_temp_overall.png", p1_overall, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_vs_hierarchical_temp_overall.png\n")

# Plot 2: By taxon
top_6_taxa <- taxon_summary_df |> slice(c(1:3, (n()-2):n())) |> pull(taxonID)

p2_by_taxon <- data_complete |>
  filter(taxonID %in% top_6_taxa) |>
  ggplot(aes(x = matched_temp_mean, y = mean_length, color = siteID)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.15, size = 0.8) +
  facet_wrap(~taxonID, scales = "free_y", ncol = 3) +
  labs(
    title = "Body Size vs Hierarchical Matched Temperature by Taxon",
    x = "Temperature (°C)",
    y = "Mean Body Length (mm)",
    color = "Site"
  ) +
  theme(legend.position = "bottom")

ggsave("figures/body_size_vs_hierarchical_temp_by_taxon.png", p2_by_taxon, width = 14, height = 8, dpi = 300)
cat("✓ Saved: figures/body_size_vs_hierarchical_temp_by_taxon.png\n")

# Plot 3: Warm vs cool
cool_warm_threshold <- median(data_complete$matched_temp_mean, na.rm = TRUE)

p3_warm_cool <- data_complete |>
  mutate(
    condition = if_else(matched_temp_mean < cool_warm_threshold, "Cool", "Warm")
  ) |>
  ggplot(aes(x = condition, y = mean_length, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(
    title = "Zooplankton Body Size: Cool vs Warm Conditions",
    x = "",
    y = "Mean Body Length (mm)",
    subtitle = sprintf("Cool: < %.2f°C | Warm: > %.2f°C", cool_warm_threshold, cool_warm_threshold)
  ) +
  theme(legend.position = "none")

ggsave("figures/body_size_hierarchical_warm_vs_cool.png", p3_warm_cool, width = 10, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_hierarchical_warm_vs_cool.png\n")

# Plot 4: By site
p4_by_site <- body_size_by_site |>
  mutate(siteID = fct_reorder(siteID, matched_temp_mean)) |>
  ggplot(aes(x = siteID, y = mean_body_length, fill = matched_temp_mean)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_body_length - sd_body_length,
                    ymax = mean_body_length + sd_body_length),
                width = 0.2, alpha = 0.7, size = 1) +
  scale_fill_gradient(low = "blue", high = "red", name = "Temp (°C)") +
  labs(
    title = "Mean Zooplankton Body Size by Site (Hierarchical Matching)",
    x = "Site (ordered by temperature)",
    y = "Mean Body Length (mm)",
    subtitle = "Error bars show ±1 SD"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/body_size_by_site_hierarchical_temp.png", p4_by_site, width = 11, height = 7, dpi = 300)
cat("✓ Saved: figures/body_size_by_site_hierarchical_temp.png\n")

# ============================================================================
# Part 10: Analysis Summary
# ============================================================================

cat("\n")
cat("Creating analysis summary...\n\n")

n_cool_larger <- sum(taxon_summary_df$slope < 0, na.rm = TRUE)
n_warm_larger <- sum(taxon_summary_df$slope > 0, na.rm = TRUE)
sig_taxa <- sum(taxon_summary_df$significant == "Yes", na.rm = TRUE)

summary_stats <- data.frame(
  metric = c(
    "Matching Strategy",
    "Data Retention Rate",
    "Total Site-Taxon Combinations",
    "Overall Slope (mm per °C)",
    "Overall R-squared",
    "Overall p-value",
    "Pattern",
    "Taxa Larger in Cool Conditions",
    "Taxa Larger in Warm Conditions",
    "Significant Taxa (p<0.05)"
  ),
  value = c(
    "Hierarchical (exact → month → season → site avg)",
    sprintf("%.1f%% of records (%d / %d observations)", pct_matched, matched_records, total_records),
    nrow(data_complete),
    round(overall_lm$coefficients[2], 6),
    round(overall_summary$r.squared, 4),
    format(overall_summary$coefficients[2, 4], scientific = TRUE),
    overall_results$pattern,
    n_cool_larger,
    n_warm_larger,
    sig_taxa
  )
)

write_csv(summary_stats, "stats-tables/analysis_summary_hierarchical.csv")
cat("✓ Saved: stats-tables/analysis_summary_hierarchical.csv\n\n")

# ============================================================================
# Final Summary
# ============================================================================

cat(paste(rep("=", 90), collapse = ""), "\n")
cat("HIERARCHICAL TEMPERATURE MATCHING - FINAL SUMMARY\n")
cat(paste(rep("=", 90), collapse = ""), "\n\n")

cat("DATA RETENTION IMPROVEMENTS:\n")
cat("  Exact month/year match (Script 18):    69.4% retention\n")
cat("  Hierarchical matching (Script 18b):   ", sprintf("%.1f%%", pct_matched), "retention\n")
cat("  Additional observations retained:     ", sprintf("%.1f%%", pct_matched - 69.4), "improvement\n\n")

cat("MATCHING STRATEGY BREAKDOWN:\n")
for (i in 1:nrow(match_breakdown)) {
  strategy <- match_breakdown$match_type[i]
  count <- match_breakdown$n[i]
  pct <- (count / matched_records) * 100
  cat(sprintf("  %-40s: %5d obs (%.1f%%)\n", strategy, count, pct))
}

cat("\n")
cat("ANALYSIS STATISTICS:\n")
cat("  Site-taxon combinations with data:  ", nrow(data_complete), "\n")
cat("  Overall slope:                      ", round(overall_lm$coefficients[2], 6), "mm/°C\n")
cat("  R-squared:                          ", round(overall_summary$r.squared, 4), "\n")
cat("  P-value:                            ", format(overall_summary$coefficients[2, 4], scientific = TRUE), "\n")
cat("  Pattern:                            ", overall_results$pattern, "\n\n")

cat("All results saved to stats-tables/ and figures/\n")
cat(paste(rep("=", 90), collapse = ""), "\n")



body_size_with_temp |> 
  filter(taxonID %in% top_taxa) |> 
  ggplot(aes(x = temp_mean, y = max_body_length)) + geom_point() +
  facet_wrap( ~ taxonID, scales = "free") + geom_smooth(method = "lm")
