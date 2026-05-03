# Extract and Clean Zooplankton Data from Raw NEON Downloads
# Purpose: Process fresh NEON zooplankton data downloads (2014-2026) following the same
#          cleaning steps as exploreZooDataProducts.R (deduplication, filtering, merging)
# Date: 2026-05-02
#
# INPUTS:
#   - data-raw/NEON_zooplankton/ (zoo_fieldData*.csv files)
#   - data-raw/NEON_zooplankton/ (zoo_perSample*.csv files)
#   - data-raw/NEON_zooplankton/ (zoo_taxonomyProcessed*.csv files)
#     (Raw NEON zooplankton data: field observations, sample data, taxonomy)
#
# OUTPUTS:
#   - data-processed/zooplankton_2014_2026.csv
#     (Cleaned zooplankton data: all life stages, 2014-2026, with body size and count metrics)
#     Used by: script 05 (filtered to adults only)

# Load libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)

cat("Loading raw NEON zooplankton data files...\n")

# Find and read all field data files
field_files <- list.files(
  path = "data-raw/NEON_zooplankton",
  pattern = "zoo_fieldData.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

field <- map_df(field_files, read_csv, show_col_types = FALSE)
cat("Loaded", nrow(field), "field data records from", length(field_files), "files\n")

# Find and read all sample data files
samp_files <- list.files(
  path = "data-raw/NEON_zooplankton",
  pattern = "zoo_perSample.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

samp <- map_df(samp_files, read_csv, show_col_types = FALSE)
cat("Loaded", nrow(samp), "sample data records from", length(samp_files), "files\n")

# Find and read all taxonomy data files
tax_files <- list.files(
  path = "data-raw/NEON_zooplankton",
  pattern = "zoo_taxonomyProcessed.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

tax <- map_df(tax_files, read_csv, show_col_types = FALSE)
cat("Loaded", nrow(tax), "taxonomy records from", length(tax_files), "files\n")

# ============================================================================
# Step 1: Remove duplicates (keep all columns except uid)
# ============================================================================
cat("\nStep 1: Removing duplicates...\n")

field_deduped <- field %>%
  distinct(across(-uid), .keep_all = TRUE)

samp_deduped <- samp %>%
  distinct(across(-uid), .keep_all = TRUE)

tax_deduped <- tax %>%
  distinct(across(-uid), .keep_all = TRUE)

cat("  Field records after deduplication:", nrow(field_deduped), "\n")
cat("  Sample records after deduplication:", nrow(samp_deduped), "\n")
cat("  Taxonomy records after deduplication:", nrow(tax_deduped), "\n")

# ============================================================================
# Step 2: Filter field data - remove impractical sampling records
# ============================================================================
cat("\nStep 2: Filtering out samplingImpractical records...\n")

field_before <- nrow(field_deduped)
field_filtered <- field_deduped[is.na(field_deduped$samplingImpractical), ]
cat("  Removed", field_before - nrow(field_filtered), "impractical sampling records\n")

# ============================================================================
# Step 3: Join field and sample data
# ============================================================================
cat("\nStep 3: Joining field and sample data...\n")

zoo_field_samp <- full_join(field_filtered, samp_deduped, join_by("sampleID" == "sampleID"))
cat("  Joined dataset has", nrow(zoo_field_samp), "records\n")

# ============================================================================
# Step 4: Handle duplicates with intelligent filtering
# ============================================================================
cat("\nStep 4: Handling duplicate sampleIDs...\n")

dupes <- zoo_field_samp[which(zoo_field_samp$uid.x %in% zoo_field_samp$uid.x[duplicated(zoo_field_samp$uid.x)]), ]
cat("  Found", n_distinct(dupes$uid.x), "duplicate uid.x values\n")

# Function to identify which duplicate to keep based on benchRemarks
find_flagged_uidy <- function(df_group) {
  if (nrow(df_group) < 2) return(NULL)

  df_trimmed <- df_group %>% select(-uid.y)

  combs <- combn(nrow(df_trimmed), 2, simplify = FALSE)

  for (idx in combs) {
    i <- idx[1]
    j <- idx[2]

    row1 <- df_trimmed[i, ]
    row2 <- df_trimmed[j, ]

    diff_cols <- names(row1)[which(as.character(row1) != as.character(row2))]

    if (identical(diff_cols, "benchRemarks")) {
      br1 <- row1$benchRemarks
      br2 <- row2$benchRemarks

      if (str_detect(br1, "Percent Subbed measured by weight")) {
        return(tibble(uid.y = df_group$uid.y[i], keep = TRUE))
      } else if (str_detect(br2, "Percent Subbed measured by weight")) {
        return(tibble(uid.y = df_group$uid.y[j], keep = TRUE))
      }
    }
  }

  return(NULL)
}

# Debug: Check structure of dupes
cat("\nDEBUG: Duplicate handling\n")
cat("Columns in dupes:\n")
print(names(dupes))
cat("\nFirst few rows of dupes:\n")
print(head(dupes))
cat("\n")

uid_to_keep <- dupes %>%
  group_by(uid.x) %>%
  group_split() %>%
  map_dfr(find_flagged_uidy)

cat("uid_to_keep result:\n")
print(uid_to_keep)
cat("Class of uid_to_keep:", class(uid_to_keep), "\n")
cat("Rows in uid_to_keep:", ifelse(is.null(uid_to_keep), 0, nrow(uid_to_keep)), "\n\n")

# Handle case where uid_to_keep is empty/NULL
if (is.null(uid_to_keep) || nrow(uid_to_keep) == 0) {
  cat("No duplicate records matched filtering criteria - keeping all duplicates\n")
  dupes_filtered <- dupes
} else {
  dupes_filtered <- dupes %>%
    mutate(uid.y = as.character(uid.y)) %>%
    left_join(uid_to_keep, by = "uid.y") %>%
    mutate(keep = if_else(is.na(keep), TRUE, keep)) %>%
    filter(keep) %>%
    select(-keep)
}

# Remove original dupes and add back the filtered ones
zoo_field_samp_clean <- zoo_field_samp[-which(zoo_field_samp$uid.y %in% dupes$uid.y), ]
zoo_field_samp_clean <- rbind(zoo_field_samp_clean, dupes_filtered)

cat("  After duplicate handling:", nrow(zoo_field_samp_clean), "records\n")

# ============================================================================
# Step 5: Join with taxonomy data
# ============================================================================
cat("\nStep 5: Joining with taxonomy data...\n")

zoo_with_taxonomy <- full_join(zoo_field_samp_clean, tax_deduped, join_by("sampleID" == "sampleID"))
cat("  After joining taxonomy:", nrow(zoo_with_taxonomy), "records\n")

# ============================================================================
# Step 6: Select relevant columns and clean
# ============================================================================
cat("\nStep 6: Selecting columns and calculating metrics...\n")

zoo <- zoo_with_taxonomy %>%
  select(
    siteID,
    namedLocation,
    collectDate,
    eventID,
    sampleID,
    samplerType,
    towsTrapsVolume,
    sampleNumber,
    towsTrapsNumber,
    samplerType,
    zooDepth1,
    zooDepth2,
    zooDepth3,
    taxonID,
    nauplii,
    zooMinimumLength,
    zooMeanLength,
    zooMaximumLength,
    zooWidth,
    adjCountPerBottle
  ) %>%
  mutate(
    aquaticSiteType = "lake",
    countPerL = adjCountPerBottle / towsTrapsVolume
  )

# ============================================================================
# Step 7: Remove exact duplicates (but preserve life stage information)
# ============================================================================
cat("\nStep 7: Removing exact duplicates...\n")

zoo_before <- nrow(zoo)
zoo <- zoo %>% distinct()
zoo_after <- nrow(zoo)

cat("  Removed", zoo_before - zoo_after, "exact duplicate rows\n")
cat("  Note: Preserving intentional duplicates (same taxon, different nauplii/life stages)\n")

# ============================================================================
# Summary and Export
# ============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("FINAL DATASET SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Total records:", nrow(zoo), "\n")
cat("Unique sites:", n_distinct(zoo$siteID), "\n")
cat("Unique taxa:", n_distinct(zoo$taxonID), "\n")
cat("Date range:", min(zoo$collectDate, na.rm = TRUE), "to", max(zoo$collectDate, na.rm = TRUE), "\n")
cat("Data completeness:\n")
cat("  zooMinimumLength:", round(sum(!is.na(zoo$zooMinimumLength))/nrow(zoo)*100, 1), "%\n")
cat("  zooMaximumLength:", round(sum(!is.na(zoo$zooMaximumLength))/nrow(zoo)*100, 1), "%\n")
cat("  countPerL:", round(sum(!is.na(zoo$countPerL))/nrow(zoo)*100, 1), "%\n")

# Save the cleaned dataset
write_csv(zoo, "data-processed/zooplankton_2014_2026.csv")
cat("\n✓ Cleaned zooplankton dataset saved to: data-processed/zooplankton_2014_2026.csv\n")
