# Zooplankton Taxon Reference
# Purpose: Extract and compile zooplankton taxon codes and scientific names from raw NEON data
# Source: NEON zoo_taxonomyProcessed files from data-raw/NEON_zooplankton/
# Date: 2026-05-02

# Load libraries
library(tidyverse)
library(readr)

# Find all zoo_taxonomyProcessed files in the NEON zooplankton raw data directory
taxonomy_files <- list.files(
  path = "data-raw/NEON_zooplankton",
  pattern = "zoo_taxonomyProcessed.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Found", length(taxonomy_files), "taxonomy files\n")

# Read and combine all taxonomy files
zoo_taxonomy_raw <- map_df(
  taxonomy_files,
  function(file) {
    tryCatch(
      read_csv(file, show_col_types = FALSE) %>%
        select(taxonID, scientificName, phylum, class, order, family, genus),
      error = function(e) {
        cat("Error reading", file, ":", e$message, "\n")
        return(NULL)
      }
    )
  }
)

cat("Loaded", nrow(zoo_taxonomy_raw), "records from all files\n")

# Create the reference table: keep unique taxa sorted by taxonID
zoo_taxon_reference <- zoo_taxonomy_raw %>%
  distinct(taxonID, .keep_all = TRUE) %>%
  arrange(taxonID)

cat("\nZooplankton Taxon Reference Summary\n")
cat("="*70, "\n")
cat("Total unique taxa:", n_distinct(zoo_taxon_reference$taxonID), "\n")
cat("Date range of source data:", "\n")

# Show sample of the reference table
cat("\nFirst 20 taxa:\n")
print(zoo_taxon_reference %>%
  select(taxonID, scientificName, class, order) %>%
  head(20))

# Save the reference table
write_csv(zoo_taxon_reference, "data-processed/zooplankton_taxon_reference.csv")
cat("\n✓ Reference table saved to: data-processed/zooplankton_taxon_reference.csv\n")
