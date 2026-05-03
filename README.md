# NEON Zooplankton Body Size & Environmental Drivers Analysis

## Project Overview

This repository contains a reproducible analysis pipeline investigating how zooplankton (and eventually fish) body size responds to temperature and food supply in NEON lake ecosystems. 


**Data Source:** National Ecological Observatory Network (NEON) – seven freshwater lake sites across North America, 2014-2026

**Key Sites:** BARC (Barco Lake, FL), CRAM (Crampton Lake, IN), LIRO (Little Rock Lake, WI), PRLA (Prairie Lake, KS), PRPO (Prairie Pothole, ND), SUGG (Suggs Lake, AL), TOOK (Toolik Lake, AK)

---

## Data Sources

### Raw NEON Data
- **Zooplankton:** Field observations, sample data, taxonomy (2014-2026)
  - Body size measurements (minimum, mean, maximum length)
  - Life stage classification (adults vs. nauplii/larvae)
  - Abundance counts
  - Location: `data-raw/NEON_zooplankton/`

- **Temperature:** Daily lake water temperature from thermal chain sensors (2017-2024)
  - Location: `data-raw/NEON_daily_summaries/NEON_daily_temp_stats_lake_tchain.csv`
  
- **Nutrients:** Daily nutrient concentrations (nitrogen and phosphorus species)
  - Nitrogen: NO₃⁻, NH₄⁺, TDN, TN
  - Phosphorus: Orthophosphate, TDP, TP
  - Location: `data-raw/NEON_daily_summaries/NEON_nutrients.csv`

- **Dissolved Oxygen:** Daily water column dissolved oxygen (productivity proxy)
  - Location: `data-raw/NEON_daily_summaries/NEON_daily_oxygen_stats.csv`

- **Algae Ash-Free Dry Mass (AFDM):** Organic matter content of algal cells
  - ALGAE ONLY (does not include other phytoplankton or organisms)
  - Direct measure of primary algal food resource for zooplankton
  - Units: Micrograms per liter (μg/L)
  - Location: `data-raw/MicroAlgae_Collection_NeonData.Robj`

---

## Directory Structure

```
NEON-old2/
├── README.md                          # This file
├── CLAUDE.md                          # Project instructions and guidelines
│
├── R-scripts/                         # Main analysis pipeline (18+ scripts)
│   ├── 05-zooplankton-body-size.R                    # Filter adults, summarize body size
│   ├── 06-zooplankton-body-size-visualization.R     # Mean body size plots
│   ├── 06b-zooplankton-body-size-visualization-max.R # Max body size plots
│   ├── 07-zooplankton-taxon-reference.R             # Taxon lookup table
│   ├── 08-extract-zooplankton-raw-data.R            # Clean raw NEON data
│   ├── 09-zooplankton-life-stage-analysis.R         # Nauplii vs adult comparison
│   ├── 10-zooplankton-mixed-vs-adults-comparison.R  # Life stage impact analysis
│   ├── 11-explore-missing-nauplii-records.R         # Data quality checks
│   ├── 12-prepare-temperature-data.R                # Temperature exploration
│   ├── 13-merge-temperature-with-body-size.R        # Main temp+body size merge (exact match)
│   ├── 14-temperature-trends-over-time.R            # Linear trend analysis
│   ├── 15-prepare-food-supply-data.R                # Nutrients & DO prep
│   ├── 15b-prepare-chlorophyll-data.R               # Phytoplankton biomass prep
│   ├── 16-merge-all-data-for-analysis.R             # Merge all data (exact matching)
│   ├── 16b-merge-all-data-hierarchical.R            # **RECOMMENDED** (hierarchical temp+DO)
│   ├── 17-summer-temperature-comparison.R           # Growing season analysis
│   ├── 18-body-size-vs-summer-temperature.R         # Body size vs temp (exact matching)
│   └── 18b-body-size-vs-temperature-hierarchical.R  # **RECOMMENDED** (hierarchical temp)
│
├── data-raw/                          # Raw NEON downloads (not in repo)
│   └── NEON_zooplankton/              # zoo_*.csv files
│   └── NEON_daily_summaries/          # Temperature, nutrients, DO
│   └── MicroAlgae_Collection_NeonData.Robj
│
├── data-processed/                    # Intermediate & analysis-ready datasets
│   ├── zooplankton_2014_2026.csv                                              # Cleaned raw data (all life stages)
│   ├── zooplankton_body_size_summary_adults_2014_2026.csv                     # Main body size dataset
│   ├── zooplankton_taxon_reference.csv                                        # Taxon lookup
│   ├── body_size_temperature_analysis.csv                                     # Monthly merged (temp + body size, exact)
│   ├── zooplankton_body_size_temp_food_supply_analysis.csv                   # Merged data (exact matching)
│   ├── zooplankton_analysis_complete_cases.csv                                # Complete cases (exact matching)
│   ├── zooplankton_body_size_temp_food_supply_hierarchical.csv               # **RECOMMENDED** (hierarchical temp+DO)
│   ├── zooplankton_analysis_complete_cases_hierarchical.csv                   # **RECOMMENDED** (hierarchical, complete)
│   ├── nutrients_monthly_summary.csv                                          # Monthly nutrient aggregates
│   ├── dissolved_oxygen_monthly_summary.csv                                   # Monthly DO aggregates
│   └── phytoplankton_afdm_monthly_summary.csv                                # Monthly algal ash-free dry mass (AFDM - ALGAE ONLY)
│
├── stats-tables/                      # Statistical results & summaries (CSV)
│   ├── temperature_trends_by_site.csv
│   ├── summer_temperature_*.csv
│   ├── overall_body_size_temp_regression.csv
│   ├── taxon_body_size_temp_regressions.csv
│   ├── body_size_summary_by_site.csv
│   ├── food_supply_nutrients_by_site.csv
│   ├── food_supply_dissolved_oxygen_by_site.csv
│   └── food_supply_phytoplankton_biomass_by_site.csv
│
└── figures/                           # Visualization outputs (PNG)
    ├── zooplankton_timeseries_by_site_*.png
    ├── zooplankton_body_size_distribution_*.png
    ├── temperature_distribution_by_site.png
    ├── temperature_timeseries_by_site.png
    ├── body_size_vs_summer_temp_*.png
    └── [additional visualizations from each script]
```

---

## Analysis Pipeline

### Data Processing & Cleaning (Scripts 07-08)
1. **Script 08:** Extract and clean raw NEON zooplankton data
   - Input: Raw NEON CSV files (field data, sample data, taxonomy)
   - Output: `zooplankton_2014_2026.csv` (all life stages, 2014-2026)
   - Tasks: Deduplication, impractical sample removal, merging datasets

2. **Script 07:** Build taxon reference table
   - Input: Taxonomy data from NEON files
   - Output: `zooplankton_taxon_reference.csv` (177 unique taxa)

### Life Stage & Body Size Analysis (Scripts 05, 09-10)
3. **Script 05:** Summarize zooplankton body size (adults only)
   - Input: `zooplankton_2014_2026.csv`
   - Output: `zooplankton_body_size_summary_adults_2014_2026.csv`
   - **Key decision:** Uses `zooMeanLength` directly (NEON measurement, not derived)
   - Filters: Nauplii == "N" to exclude larvae
   - Creates: Monthly summaries by site × taxon

4. **Script 09-10:** Assess life stage impact
   - Demonstrates that nauplii (larvae) are ~2.4× smaller than adults
   - Justifies adult-only analysis to avoid confounding

### Temperature Analysis (Scripts 12-14, 17)
5. **Script 13:** Merge temperature with body size data
   - Input: Daily temperature data + monthly body size summaries
   - Output: `body_size_temperature_analysis.csv` (monthly)
   - Creates: 4 temperature visualization files

6. **Script 14:** Test for long-term temperature trends
   - Output: Trend statistics by site (warming/cooling analysis)

7. **Script 17:** Compare summer temperatures (June-August)
   - Output: Site-specific growing season temperature profiles

### Food Supply Analysis (Scripts 15-15b)
8. **Script 15:** Prepare nutrient and dissolved oxygen data
   - Input: Daily nutrient and DO measurements
   - Output: Monthly summaries
   - Variables: NO₃, NH₄, TDN, TN, OrthoP, TDP, TP, meanDO

9. **Script 15b:** Prepare phytoplankton biomass data
   - Input: Microalgae R object (ash-free dry mass)
   - Output: `phytoplankton_biomass_monthly_summary.csv`

### Final Integration & Analysis (Scripts 16-18)
10. **Script 16:** Merge all data sources into analysis dataset
    - Input: Body size + temperature + nutrients + DO + phytoplankton
    - Output: `zooplankton_body_size_temp_food_supply_analysis.csv`
    - **Approach:** Exact month-year matching (69.4% temperature retention)
    - Includes complete and subset versions

11. **Script 16b:** Merge all data with HIERARCHICAL matching ⭐ **RECOMMENDED**
    - Input: Body size + temperature + nutrients + DO + phytoplankton
    - Output: `zooplankton_body_size_temp_food_supply_hierarchical.csv`
    - **Approach:** Hierarchical fallback for temperature and DO (~100% retention)
    - Includes match_type columns for transparency
    - **ADVANTAGE:** Retains ~30% more observations while tracking data quality

12. **Script 18:** Test main hypothesis (exact matching)
    - Input: Body size data + temperatures (exact month-year only)
    - Tests: Overall correlation and per-taxon regressions
    - Output: Statistical results and visualizations
    - **Data retention:** 69.4% of observations

13. **Script 18b:** Test main hypothesis with HIERARCHICAL matching ⭐ **RECOMMENDED**
    - Input: Body size data + temperatures (hierarchical fallback)
    - Tests: Overall correlation and per-taxon regressions
    - Output: Statistical results and visualizations (includes matching breakdown)
    - **Data retention:** ~100% of observations
    - **Key advantage:** Matches all data while maintaining transparency about match quality

---

## Analysis Approaches: Exact vs Hierarchical Matching

This project supports **two complementary analysis approaches**:

### Approach 1: Exact Matching (Scripts 16, 18)
- **Temperature matching:** Exact month-year match only
- **Data retention:** 69.4% of zooplankton observations
- **Pros:** Highest data quality (true monthly match)
- **Cons:** Loses ~30% of valuable observations
- **Use when:** Conservative analysis preferred, sample size adequate

### Approach 2: Hierarchical Matching (Scripts 16b, 18b) ⭐ Recommended
- **Temperature/DO matching:** 4-level fallback strategy
  1. **Level 1:** Exact month-year match (if available)
  2. **Level 2:** Same month, average across years (seasonal capture)
  3. **Level 3:** Adjacent months (seasonal proxy)
  4. **Level 4:** Site annual average (last resort)
- **Data retention:** ~100% of zooplankton observations
- **Pros:** 
  - Maximizes data retention (30.6% more records)
  - Fully transparent (match_type column)
  - Same-month averaging captures seasonal patterns
  - Independent matching for temperature AND dissolved oxygen
- **Cons:** Uses averaged data at lower priority levels (acceptable for seasonal analysis)
- **Use when:** Maximum statistical power needed, data quality transparency valued
- **See:** `HIERARCHICAL_MATCHING_GUIDE.md` for detailed explanation

### Choosing Your Approach

| Goal | Recommended Scripts |
|------|-------------------|
| Maximum data retention with transparency | **16b + 18b** |
| Conservative, exact matches only | 16 + 18 |
| Compare both approaches (sensitivity) | All scripts (16, 16b, 18, 18b) |

---



### 3. **Monthly Aggregation**
- All environmental variables aggregated to monthly resolution
- Matches zooplankton sampling frequency
- Allows site × month × year comparisons

### 4. **Multiple Food Supply Indicators**
- **Nutrients** (NO₃, NH₄, TN, TP): Primary production substrates
- **Dissolved Oxygen** (meanDO): Photosynthetic productivity proxy
- **Phytoplankton Biomass** (AFDM): Direct food resource measurement


---

## How to Use This Repository

### Quick Start: Run the Full Pipeline (Recommended - Hierarchical Matching)

```bash
# Navigate to project directory
cd NEON-old2

# Run core preparation scripts (within R/RStudio):
source("R-scripts/08-extract-zooplankton-raw-data.R")
source("R-scripts/05-zooplankton-body-size.R")
source("R-scripts/15-prepare-food-supply-data.R")
source("R-scripts/15b-prepare-chlorophyll-data.R")

# Run hierarchical analysis (RECOMMENDED - maximum data retention):
source("R-scripts/16b-merge-all-data-hierarchical.R")   # Create analysis dataset
source("R-scripts/18b-body-size-vs-temperature-hierarchical.R")  # Main hypothesis test

# Optional: Run exact-matching versions for comparison (sensitivity analysis):
source("R-scripts/16-merge-all-data-for-analysis.R")    # Exact matches only
source("R-scripts/18-body-size-vs-summer-temperature.R")  # Exact matches only
```

### Alternative: Conservative Analysis (Exact Matching Only)

```bash
# Same prep scripts, but use exact-matching analysis:
source("R-scripts/08-extract-zooplankton-raw-data.R")
source("R-scripts/05-zooplankton-body-size.R")
source("R-scripts/15-prepare-food-supply-data.R")
source("R-scripts/15b-prepare-chlorophyll-data.R")
source("R-scripts/16-merge-all-data-for-analysis.R")   # Exact matches
source("R-scripts/18-body-size-vs-summer-temperature.R")  # Exact matches
```

### Output Locations

After running the pipeline:

- **Analysis Dataset:** `data-processed/zooplankton_body_size_temp_food_supply_analysis.csv`
- **Statistical Results:** `stats-tables/*.csv` (4-5 files per analysis)
- **Figures:** `figures/*.png` (20+ visualization files)

### Run Individual Analyses

Each script is self-contained and can be run independently if its input files exist:

```r
# Just visualize body size patterns
source("R-scripts/06-zooplankton-body-size-visualization.R")

# Just test temperature trends
source("R-scripts/14-temperature-trends-over-time.R")

# Just hypothesis test (if analysis dataset exists)
source("R-scripts/18-body-size-vs-summer-temperature.R")
```

### Reference Documentation

For a detailed explanation of the hierarchical matching strategy, including why it works and how to use match quality columns in your analysis, see:
- **`HIERARCHICAL_MATCHING_GUIDE.md`** - Complete guide to hierarchical vs exact matching approaches

---

## Key Datasets

### 1. `zooplankton_body_size_summary_adults_2014_2026.csv`
- **Rows:** One per unique combination of (site, date, taxon)
- **Columns:** 
  - `siteID`, `namedLocation`, `collectDate`, `taxonID`
  - `mean_body_length` (mm) ← **Primary body size metric**
  - `max_body_length`, `mean_body_width`
  - `count_per_bottle`, `count_per_liter`
  - `sampler_type`, `aquatic_site_type`

### 2. `body_size_temperature_analysis.csv`
- **Rows:** One per site × month × year
- **Columns:**
  - Time: `siteID`, `year`, `month`, `date`
  - Body size: `mean_body_length`, `sd_body_length`, `max_body_length`
  - Temperature: `temp_mean_monthly`, `temp_sd_monthly`, `temp_max_monthly`, `temp_min_monthly`
  - Sample info: `n_samples`, `n_taxa`, `mean_count_per_liter`

### 3. `zooplankton_body_size_temp_food_supply_analysis.csv` (FINAL)
- **Rows:** One per site × month × year
- **Columns:** Combines all three data sources
  - Body size variables (from Script 05)
  - Temperature variables (from Script 13)
  - Nutrient variables (from Script 15): NO₃, NH₄, TDN, TN, OrthoP, TDP, TP
  - DO variables (from Script 15): meanDO, maxDO, minDO
  - Biomass variables (from Script 15b): phytoplankton AFDM

---

## Statistical Analyses Performed

### Temperature Trends (Script 14)
- **Model:** `meanTemp ~ year_decimal`
- **Test:** Linear regression (raw and seasonal-adjusted)
- **Finding:** Mixed results—some sites warming, some cooling significantly

### Summer Temperature Comparison (Script 17)
- **Test:** One-way ANOVA + pairwise comparisons
- **Scope:** June-August temperatures across sites
- **Purpose:** Identify warmest/coolest sites during growing season

### Body Size vs Temperature (Script 18)
- **Model 1:** `body_size ~ summer_temperature` (overall)
- **Model 2:** Per-taxon regressions (top 10 taxa)
- **Purpose:** Test if zooplankton are larger in cooler lakes

### Hypothesis Testing Framework
- Uses monthly aggregated data
- Tests both overall patterns and taxon-specific responses
- Includes complete-cases subset for regression

---

## Requirements & Dependencies

### R Libraries
```r
tidyverse     # Data wrangling and visualization
readr         # CSV reading
ggplot2       # Plotting
cowplot       # Publication-quality plots
lubridate     # Date/time handling
purrr         # Functional programming
stringr       # String manipulation
tibble        # Modern data frames
```

### R Version
- R 4.0+ recommended
- Uses native pipe operator (`|>`)

### System Requirements
- ~1-2 GB disk space for raw NEON downloads
- ~500 MB for processed data and outputs

---

## Data Quality Notes

### Temperature Data Coverage
| Site | Coverage (%) |
|------|-------------|
| SUGG | 96.1% |
| BARC | 88.2% |
| CRAM | 80.5% |
| LIRO | 75.0% |
| PRPO | 72.8% |
| PRLA | 61.7% |
| TOOK | 18.0% |

*Note: Coverage varies by site; TOOK has limited early data (2017-2024)*

### Zooplankton Data
- **Total records:** 25,000+ observations (2014-2026)
- **Taxa:** 177 unique species/taxa
- **Adults:** 18,000+ records (nauplii filtered out)
- **Completeness:** 98%+ for body size measurements on adults

### Missing Life Stage Information
- ~2-3% of records have missing nauplii field
- Treated as "unknown" in exploratory analyses
- Filtered for adult-only analyses

---

## Publication & Reproducibility

All scripts include:
- Explicit inputs/outputs in header comments
- Data completeness checks and reporting
- Summary statistics and visualizations
- No hardcoded paths (relative to project root)

### Citation
If using this analysis, cite NEON as the data source:

> National Ecological Observatory Network (NEON). (2024). *Zooplankton collection data*. National Center for Ecological Analysis and Synthesis.

---

## Project Contacts & Contributors

**Principal Investigator:** Joey Bernhardt
- Email: joey.bernhardt@uoguelph.ca

**Analysis Date:** May 2026

---

## File Inventory Checklist

### Must Have (for running pipeline)
- [ ] Raw NEON zooplankton data in `data-raw/NEON_zooplankton/`
- [ ] Temperature data: `NEON_daily_temp_stats_lake_tchain.csv`
- [ ] Nutrients data: `NEON_nutrients.csv`
- [ ] DO data: `NEON_daily_oxygen_stats.csv`
- [ ] Microalgae R object: `MicroAlgae_Collection_NeonData.Robj`

### Generated During Pipeline
- [ ] `data-processed/zooplankton_2014_2026.csv`
- [ ] `data-processed/zooplankton_body_size_summary_adults_2014_2026.csv`
- [ ] `data-processed/body_size_temperature_analysis.csv`
- [ ] `data-processed/zooplankton_body_size_temp_food_supply_analysis.csv` ← **Final**

### Analysis Outputs
- [ ] `stats-tables/` (multiple CSV results)
- [ ] `figures/` (20+ PNG files)

---

## Next Steps & Future Analyses

### Potential Extensions
1. Incorporate fish body size data (scripts 01-03 started)
2. Multi-level modeling accounting for site random effects
3. Interaction terms: Does food supply modify temperature effects?
4. Temporal analysis: Do relationships change over time?
5. Cross-site synthesis: Meta-analysis patterns

### Data Updates
- Pipeline designed to accept new NEON downloads (2026+)
- Simply replace raw files and re-run scripts
- All file paths are relative and project-agnostic

---

## License & Attribution

NEON data is provided under Creative Commons attribution (CC-BY-4.0).
Analysis code provided as-is for research and educational use.

---

*Last updated: May 2, 2026*
*For questions or issues, see individual script headers for specific data sources and methods.*
