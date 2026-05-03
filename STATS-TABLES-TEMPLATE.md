# Stats-Tables Standard Format

This document defines the standard structure for saving statistical analysis results to the `stats-tables/` folder.

## Overview

All statistical test results, summaries, and model outputs are saved as CSV files in `stats-tables/` rather than printed to console. This ensures:
- Results are reproducible and permanently stored
- Data can be easily imported for further analysis
- A consistent record of all statistical findings

## Directory Structure

```
stats-tables/
├── temperature_trends_by_site.csv              # Linear regression results
├── temperature_trend_categories.csv            # Categorized trends (warming/cooling/none)
├── temperature_trend_summary.csv               # Summary statistics
├── summer_temperature_by_site.csv              # Site-level temperature summaries
├── summer_temperature_by_month.csv             # Monthly breakdown
├── summer_temperature_by_year.csv              # Year-over-year variation
├── summer_temperature_anova.csv                # ANOVA test results
├── summer_temperature_pairwise.csv             # Pairwise comparisons
├── overall_body_size_temp_regression.csv       # Overall regression model
├── taxon_body_size_temp_regressions.csv        # Per-taxon regression results
├── body_size_summary_by_site.csv               # Body size by site
├── analysis_summary.csv                        # High-level summary
├── food_supply_nutrients_by_site.csv           # Nutrient summaries
└── food_supply_dissolved_oxygen_by_site.csv    # Dissolved oxygen summaries
```

## File Format Conventions

### 1. Temperature Trend Results
**File:** `temperature_trends_by_site.csv`

Columns:
- `siteID`: Site identifier (BARC, CRAM, LIRO, PRLA, PRPO, SUGG, TOOK)
- `slope`: Linear regression slope (°C per year)
- `pval`: P-value from regression
- `r_squared`: R-squared value
- `n_obs`: Number of observations
- `significant`: TRUE/FALSE indicator

Example:
```
siteID,slope,pval,r_squared,n_obs,significant
BARC,0.0962,0.0001,0.0015,2579,TRUE
CRAM,-0.3792,0.0000,0.0222,1269,TRUE
```

### 2. Summary Statistics Tables
**Pattern:** `[analysis]_summary_by_[grouping].csv`

Example: `body_size_summary_by_site.csv`

Columns should include:
- Grouping variable(s) (siteID, taxonID, etc.)
- `n` or `n_samples`: Sample size
- `mean`: Mean value
- `sd`: Standard deviation
- `min`: Minimum value
- `max`: Maximum value

Example:
```
siteID,mean_body_length,sd_body_length,min_body_length,max_body_length,n_taxa
BARC,0.3841,0.2103,0.0524,0.9876,45
```

### 3. Regression Model Results
**Pattern:** `[dependent]_[independent]_regression[s].csv`

Example: `overall_body_size_temp_regression.csv`

Columns:
- `model`: Description of the model
- `n_observations`: Sample size
- `intercept`: Intercept (b0)
- `slope`: Slope (b1)
- `r_squared`: R-squared
- `adj_r_squared`: Adjusted R-squared
- `p_value`: P-value for slope
- `correlation`: Correlation coefficient
- `pattern`: Interpretation (e.g., "Larger in cool sites")

Example:
```
model,n_observations,intercept,slope,r_squared,adj_r_squared,p_value,correlation,pattern
Mean Body Length ~ Summer Temperature,247,0.4521,-0.0084,0.0342,0.0304,0.0287,-0.1850,Larger in cool sites
```

### 4. Statistical Test Results
**Pattern:** `[test_name]_[comparison].csv`

Example: `summer_temperature_anova.csv`

Columns:
- `test`: Name of statistical test
- `F_value` or `t_stat`: Test statistic
- `p_value`: P-value
- `significant`: YES/NO result
- `result`: Interpretation

Example:
```
test,F_value,p_value,significant,result
One-way ANOVA: Summer Temperature ~ Site,24.15,0.0000,Yes,Sites differ significantly in summer temperature
```

### 5. Categorized Results
**Pattern:** `[analysis]_categories.csv`

Example: `temperature_trend_categories.csv`

Columns:
- Core result columns (siteID, slope, p_value, etc.)
- `[category_name]`: Categorical grouping (trend_type, significant, etc.)

Example:
```
siteID,slope,pval,trend_type
BARC,0.0962,0.0001,Warming
CRAM,-0.3792,0.0000,Cooling
```

### 6. Summary Statistics
**File:** `analysis_summary.csv`

Columns:
- `metric`: Name of the metric
- `value`: The value
- Description columns as needed

Example:
```
metric,value
Overall slope (mm per °C),−0.0084
Overall R-squared,0.0342
Overall p-value,0.0287
Pattern,Larger in cool sites
Taxon sample size,10
Taxa larger in cool sites,3
Taxa larger in warm sites,7
Significant taxa (p<0.05),2
```

## Naming Conventions

1. **File names are lowercase with underscores**
   - ✓ `body_size_summary_by_site.csv`
   - ✗ `Body_Size_Summary_By_Site.csv`

2. **Descriptive and hierarchical**
   - Start with the variable being analyzed
   - Use `_by_` for groupings: `temperature_by_site.csv`
   - Use `_vs_` for comparisons: `body_size_vs_temperature.csv`

3. **Plural for multiple observations**
   - ✓ `temperature_trends_by_site.csv` (multiple sites)
   - ✗ `temperature_trend_by_site.csv`

## Column Naming Conventions

1. **Use underscores, not spaces**
   - ✓ `mean_body_length`
   - ✗ `mean body length`

2. **Be specific about measurements**
   - ✓ `temp_mean`, `temp_sd`, `temp_min`, `temp_max`
   - ✗ `temperature`, `stats`

3. **Include units in descriptions or separate columns**
   - Use consistent units across files
   - Document units in README if not obvious

4. **Standard statistical abbreviations**
   - `n`: sample size
   - `mean`: mean value
   - `sd`: standard deviation
   - `min`: minimum
   - `max`: maximum
   - `p_value` or `pval`: p-value from statistical test
   - `r_squared`: coefficient of determination
   - `slope`: regression slope
   - `intercept`: regression intercept

## How to Use Stats-Tables Files

### Reading in R
```r
library(tidyverse)

# Load a stats file
trends <- read_csv("stats-tables/temperature_trends_by_site.csv")

# Combine multiple files
all_results <- bind_rows(
  read_csv("stats-tables/summer_temperature_by_site.csv"),
  read_csv("stats-tables/body_size_summary_by_site.csv")
)
```

### Filtering and Analysis
```r
# Find significant results
significant_trends <- trends |>
  filter(significant == TRUE)

# Compare across analyses
warming_sites <- trends |>
  filter(slope > 0, significant == TRUE) |>
  pull(siteID)
```

### Creating Reports
```r
# Use stats-tables as source for reports/tables
summary_table <- read_csv("stats-tables/analysis_summary.csv") |>
  knitr::kable()
```

## Script Implementation Checklist

When writing a new analysis script:

- [ ] Create `stats-tables/` directory at the beginning
- [ ] Replace all `print()` and `cat()` summary output with `write_csv()`
- [ ] Use consistent column names across related files
- [ ] Include `n_observations` or `n_samples` in all summaries
- [ ] Save p-values as-is (not rounded to 0.05 threshold)
- [ ] Include interpretation columns (e.g., "Larger in cool sites")
- [ ] End script with summary message showing which files were saved

Example script template:
```r
# Create stats-tables directory
if (!dir.exists("stats-tables")) {
  dir.create("stats-tables", showWarnings = FALSE)
}

# ... analysis code ...

# Save results
write_csv(results_df, "stats-tables/analysis_name_results.csv")
cat("✓ Saved: stats-tables/analysis_name_results.csv\n")

# Summary
cat("\n================================\n")
cat("ANALYSIS COMPLETE\n")
cat("================================\n")
cat("Results saved to stats-tables/:\n")
cat("  - file1.csv\n")
cat("  - file2.csv\n")
```

## Maintaining Consistency

- All numerical values should have reasonable precision (3-4 decimal places)
- Use NA (not "NA" as string) for missing values
- Keep column order logical (grouping variables first, then statistics, then p-values/significance)
- Document any special coding (e.g., TRUE/FALSE vs YES/NO)
