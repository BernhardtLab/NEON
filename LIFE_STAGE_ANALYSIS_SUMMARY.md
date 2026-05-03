# Zooplankton Life Stage Analysis Summary

**Date:** 2026-05-02  
**Analysis:** Separation of nauplii (larvae) from adults in NEON zooplankton dataset

---

## Key Findings

### 1. Life Stage Composition

The zooplankton dataset contains:
- **Adults/Copepodites:** 5,981 records (78.3%)
- **Nauplii (larvae):** 596 records (7.8%)
- **Unknown/Missing:** 1,065 records (13.9%)

⚠️ **Note:** ~14% of records have missing life stage information (nauplii = NA)

---

### 2. Critical Observation: COPSP is Predominantly Nauplii

| Taxon | Total Records | Adults | Nauplii | % Nauplii |
|-------|-------|--------|---------|-----------|
| **COPSP** | 572 | 5 | 567 | **99.1%** |
| CALSP1 | 427 | 427 | 0 | 0.0 |
| CYCSP | 394 | 394 | 0 | 0.0 |
| POLSP20 | 317 | 317 | 0 | 0.0 |

**This is a major confound:** If you use mixed data (adults + nauplii), COPSP's body size will be dramatically underestimated because 99% of its records are tiny larvae.

---

### 3. Body Size Differences Between Life Stages

**Overall comparison:**

| Life Stage | Mean Body Length | Std Dev | N Records |
|-----------|------------------|---------|-----------|
| Adults | 0.455 mm | 0.600 | 5,981 |
| Nauplii | 0.187 mm | 0.083 | 596 |

**Nauplii are 2.4× SMALLER than adults on average.**

**Top 3 Most Abundant Taxa Breakdown:**

| Taxon | Life Stage | N | Mean Length | Size Range |
|-------|-----------|---|-------------|-----------|
| COPSP | Adult | 5 | 0.499 mm | 0.15–0.81 mm |
| COPSP | **Nauplii** | 567 | **0.186 mm** | **0.012–2.70 mm** |
| CALSP1 | Adult | 427 | 0.681 mm | 0.16–2.93 mm |
| CYCSP | Adult | 394 | 0.558 mm | 0.1–1.425 mm |

**For COPSP:** Using mixed data would underestimate average body size by **62.6%** because the population is overwhelmingly nauplii.

---

### 4. Impact on Analysis

When comparing mixed (all) vs. adults-only data:

**Taxa with Largest Differences (showing inflation when nauplii are included):**

| Taxon | Mixed Mean | Adults Mean | Difference | % Change |
|-------|-----------|-------------|-----------|----------|
| LEPKIN1 | 3.717 mm | 2.250 mm | +1.467 mm | +65.2% |
| CERRET | 0.488 mm | 0.300 mm | +0.188 mm | +62.5% |
| ASPHER | 0.461 mm | 0.345 mm | +0.116 mm | +33.5% |

**Meaning:** If a taxon has BOTH adults and nauplii in the sample, mixing them creates uncertainty. You don't know whether trends are due to:
- True changes in body size
- Changes in life stage composition (more nauplii vs. adults)
- Both

---

### 5. Separate Datasets Created

Two new datasets are now available:

**`zooplankton_adults_2014_2026.csv`**
- 5,981 records
- 188 unique taxa
- Only includes nauplii = "N" (confirmed adults/copepodites)
- **Recommended for testing your temperature/food hypothesis**

**`zooplankton_nauplii_2014_2026.csv`**
- 596 records
- Only 3 taxa (mostly COPSP)
- Only includes nauplii = "Y"
- Useful for understanding recruitment dynamics

---

## Recommendations for Your Analysis

### **Use Adults-Only Dataset for Body Size Hypothesis**

**Why:**
1. **Clean biology:** You're measuring organisms at the same life stage
2. **Metabolic relevance:** Adults are the dominant energetic consumers; they're what's affected by temperature/food stress
3. **Clearer interpretation:** Changes in body size = actual response to environment, not population composition shifts
4. **Better statistics:** More data (5,981 vs 596 records), more taxa (188 vs 3)
5. **Comparable to literature:** Most zooplankton ecology studies focus on adult copepods

### **What About Nauplii?**

Keep them separate for a complementary analysis:
- **Research question:** Do nauplii recruitment rates change with temperature/food?
- **Ecological insight:** Understanding population age structure responses to environmental stress
- **But:** Don't mix with adults in body size analysis

### **How to Proceed**

1. **For your main hypothesis (body size response to temp/food):**
   - Use `zooplankton_adults_2014_2026.csv`
   - Filter to: `nauplii == "N"` (though this dataset only has those)
   - Analyze mean and max body length separately

2. **Create parallel summary datasets:**
   - One from adults only
   - One from nauplii only
   - Keep them separate in visualization scripts

3. **Run the visualization scripts on adults-only data:**
   - Modify scripts 05, 06, and 06b to use the new adults-only dataset
   - This will show you how patterns change when life stages are separated

---

## R Script Created

**`09-zooplankton-life-stage-analysis.R`**

This script provides:
1. Overall life stage breakdown
2. Taxa composition table (% nauplii vs. adult for each taxon)
3. Body size ranges by life stage
4. Comparison for top 10 taxa
5. Creation of separate datasets
6. Visualization comparing mixed vs. adults-only patterns
7. Statistical impact summary

Run this script to generate a visual comparison plot showing how your body size patterns would look with vs. without nauplii.

---

## Bottom Line

**Your initial concern was exactly right:** Mixing nauplii and adults confounds the analysis. Some taxa (like COPSP) are almost entirely larval, which would completely distort size estimates. Using the adults-only dataset will give you clean, interpretable results for testing your hypothesis about temperature and food supply effects on body size.
