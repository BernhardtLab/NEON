# Zooplankton Body Size Data Wrangling
# Purpose: Create a summary dataset with mean body size per zooplankton taxon per site per date
# Date: 2026-05-02

# Load libraries
library(tidyverse)
library(readr)

# Load the raw zooplankton data
zoo_raw <- read_csv("Clean Data/zooplankton.csv")

# Create summary by siteID, collectDate, and taxonID
# Calculate mean body size as (min + max) / 2 for each taxon-sample combination
zoo_body_size_summary <- zoo_raw %>%
  mutate(mean_body_length = (zooMinimumLength + zooMaximumLength) / 2) %>%
  group_by(siteID, namedLocation, collectDate, taxonID) %>%
  summarise(
    mean_body_length = mean(mean_body_length, na.rm = TRUE),
    min_body_length = mean(zooMinimumLength, na.rm = TRUE),
    max_body_length = mean(zooMaximumLength, na.rm = TRUE),
    mean_body_width = mean(zooWidth, na.rm = TRUE),
    count_per_bottle = first(adjCountPerBottle),
    count_per_liter = first(countPerL),
    sampler_type = first(samplerType),
    aquatic_site_type = first(aquaticSiteType),
    .groups = "drop"
  ) %>%
  mutate(collectDate = as.Date(collectDate)) %>%
  arrange(siteID, collectDate, taxonID)

# Print summary statistics
cat("Original zooplankton records:", nrow(zoo_raw), "\n")
cat("Summary records (taxa per sample):", nrow(zoo_body_size_summary), "\n")
cat("Unique taxa:", n_distinct(zoo_body_size_summary$taxonID), "\n")
cat("Unique sites:", n_distinct(zoo_body_size_summary$siteID), "\n")
cat("Date range:", min(zoo_body_size_summary$collectDate), "to", max(zoo_body_size_summary$collectDate), "\n")

# Check data completeness
cat("\nData completeness:\n")
cat("  Mean body length:", round(sum(!is.na(zoo_body_size_summary$mean_body_length))/nrow(zoo_body_size_summary)*100, 1), "%\n")
cat("  Body width:", round(sum(!is.na(zoo_body_size_summary$mean_body_width))/nrow(zoo_body_size_summary)*100, 1), "%\n")

# Calculate body size statistics by taxon (all 177 taxa)
zoo_taxon_stats <- zoo_body_size_summary %>%
  group_by(taxonID) %>%
  summarise(
    mean_length_mm = mean(mean_body_length, na.rm = TRUE),
    std_dev = sd(mean_body_length, na.rm = TRUE),
    min_length = min(mean_body_length, na.rm = TRUE),
    max_length = max(mean_body_length, na.rm = TRUE),
    n_samples = n(),
    avg_density_per_L = mean(count_per_liter, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_samples))

cat("\nBody size statistics by taxon (all", n_distinct(zoo_taxon_stats$taxonID), "taxa):\n")
cat("Showing top 10 most abundant:\n")
print(head(zoo_taxon_stats, 10))

# Save the full taxon statistics
write_csv(zoo_taxon_stats, "data-processed/zooplankton_taxon_body_size_stats.csv")
cat("\n✓ Full taxon statistics saved to: data-processed/zooplankton_taxon_body_size_stats.csv\n")

# Save the summary dataset
write_csv(zoo_body_size_summary, "data-processed/zooplankton_body_size_summary.csv")
cat("\n✓ Summary dataset saved to: data-processed/zooplankton_body_size_summary.csv\n")



