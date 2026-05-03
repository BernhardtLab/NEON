# Guide to Oxygen Metrics: Concentration vs Percent Saturation
**Date:** May 3, 2026

---

## Quick Answer

**Use concentration (mg/L) if:** You care about absolute oxygen availability or detection of hypoxia thresholds

**Use percent saturation (%) if:** You're comparing across different temperatures or seasons and want to understand relative oxygen stress

**Best practice:** Use BOTH to get complete picture

---

## Dissolved Oxygen Concentration (mg/L)

### What It Is
The actual amount of O₂ molecules dissolved in water, measured in milligrams per liter.

**Example:** 8 mg/L of DO = 8 mg of oxygen gas per liter of water

### When to Use
- **Organism metabolic needs:** Many animals have minimum oxygen requirements (e.g., "need ≥5 mg/L to survive")
- **Hypoxia detection:** Critical thresholds like 2 mg/L (severe hypoxia) are defined in absolute terms
- **Calibration against standards:** Regulatory agencies often set concentration limits

### Advantages
✅ Direct, absolute measure  
✅ Easy to interpret for toxicologists  
✅ No additional calculations needed  
✅ What most dissolved oxygen meters measure directly  

### Limitations
❌ Ignores temperature dependence  
❌ Same 6 mg/L means different things in cold vs warm water  
❌ Can't directly compare spring vs summer without knowing both temperature AND concentration  
❌ Doesn't show if water is "stressed" relative to capacity

### Example Scenario
| Month | Temperature | DO Conc | Saturation | Interpretation |
|-------|-------------|---------|------------|-----------------|
| April | 5°C | 11 mg/L | 80% | Adequate oxygen, but below capacity |
| July | 25°C | 7 mg/L | 75% | Less oxygen, but proportionally similar stress |

**Using concentration alone:** July has LESS oxygen (7 < 11), so seems worse  
**Using saturation:** Both months have similar stress (~75-80% of capacity)  
**Reality:** Water temperature drives solubility—July isn't necessarily worse

---

## Dissolved Oxygen Percent Saturation (%)

### What It Is
The percentage of oxygen that water is holding relative to the maximum it can hold at that temperature and pressure.

**Formula:** `% Saturation = (Measured DO / Saturation DO at that temperature) × 100`

**Example:** If water at 20°C can hold max 9 mg/L, and measured DO = 7.2 mg/L:
- Percent saturation = (7.2 / 9) × 100 = **80%**

### When to Use
- **Ecological comparisons:** Fish and zooplankton evolved in water of specific saturation ranges
- **Seasonal patterns:** Understand if hypoxia is driven by temperature or respiration
- **Comparing across lakes:** Different lakes, different temperatures, same saturation = similar stress
- **Photosynthesis/respiration:** >100% saturation indicates photosynthesis exceeds respiration

### Advantages
✅ Temperature-normalized (accounts for solubility differences)  
✅ Reflects biological stress better than raw concentration  
✅ Easy to interpret: 100% = equilibrium, >100% = super-saturated, <50% = hypoxic  
✅ Comparable across seasons and locations  
✅ Shows ecosystem productivity (peak saturation from photosynthesis)  

### Limitations
❌ Requires temperature data for calculation  
❌ Won't detect absolute hypoxia if saturation is high but temperature is cold  
❌ Less useful for toxicological thresholds  
❌ Additional complexity in analysis

### Interpretation Guide
| Range | Meaning | Biological Significance |
|-------|---------|------------------------|
| >120% | Supersaturated | Strong photosynthesis, possible gas bubble disease risk |
| 100% | Equilibrium | Water in balance with atmosphere |
| 80-100% | Well-oxygenated | Most organisms thrive |
| 50-80% | Moderately low | Stress for sensitive species |
| 20-50% | Hypoxic | Avoidance behavior in mobile species |
| <20% | Severely hypoxic | Lethal for most organisms |

---

## How the Scripts Calculate Saturation

### Scripts 15d & 15e Use Garcia-Gordon Equation

This is the standard freshwater saturation equation in limnology:

```
ln(DO_sat) = -173.4292 + 249.6339(100/T) + 143.3483·ln(100/T) - 21.8492(100/T)²
```

Where T = absolute temperature (Kelvin) = °C + 273.15

This equation is:
- ✅ Validated for freshwater lakes
- ✅ Valid range: 0-35°C (covers all NEON freshwater lakes)
- ✅ Accounts for temperature-dependent gas solubility
- ✅ Standard in limnology textbooks and USGS publications

### Output Variables

**Script 15d creates (daily):**
- `meanDO_sat_conc` - Saturation DO at mean temperature (mg/L)
- `meanDO_sat_pct` - Percent saturation of mean DO
- `maxDO_sat_pct`, `minDO_sat_pct` - Peak and minimum saturation

**Script 15e creates (monthly aggregates):**
- `meanDO_sat_pct_mean` - Monthly mean percent saturation
- `meanDO_sat_pct_sd`, `_min`, `_max` - Variability metrics
- `maxDO_sat_pct_mean` - Monthly mean of daily peaks
- `minDO_sat_pct_mean` - Monthly mean of daily minimums

---

## Using Both Metrics in Your Analysis

### Approach 1: Separate Models
Run your analysis twice:

```r
# Model 1: Using concentration
lm(body_size ~ temp + do_concentration + afdm)

# Model 2: Using saturation
lm(body_size ~ temp + do_saturation_pct + afdm)

# Compare R², coefficients, and interpretation
```

**What this tells you:** Do zooplankton respond more strongly to absolute oxygen or relative oxygen stress?

### Approach 2: Include Both
Add both to a single model:

```r
lm(body_size ~ temp + do_concentration + do_saturation_pct + afdm)
```

**Interpretation:** 
- If saturation coefficient is significant but concentration is not → zooplankton care about relative stress
- If concentration is significant but saturation is not → zooplankton have absolute thresholds
- If both are significant → both aspects matter

### Approach 3: Create Diagnostic Plots
```r
# Plot 1: DO concentration vs temperature
ggplot(data, aes(x = temp, y = do_concentration)) +
  geom_point() + geom_smooth(method = "lm")

# Plot 2: DO saturation vs temperature
ggplot(data, aes(x = temp, y = do_saturation_pct)) +
  geom_point() + geom_smooth(method = "lm")

# If Plot 1 shows strong negative correlation and Plot 2 shows flat line,
# then the correlation in Plot 1 is purely temperature-driven solubility
# (ecological meaning is minimal)
```

---

## For Your Zooplankton Analysis

### Recommended Approach

**Step 1: Use concentration first**
- Your current dataset uses `meanDO_avg` (concentration)
- Run Script 19 as-is to establish baseline results

**Step 2: Optionally add saturation**
- Run Scripts 15d and 15e to calculate saturation
- Manually add saturation columns to your merged dataset
- Re-run Script 19 comparing both metrics

**Step 3: Interpret differences**
- Do body size responses differ between models?
- Does saturation explain more variance than concentration?
- Are zooplankton responding to absolute oxygen or relative stress?

### Example Hypothesis Tests

**Hypothesis 1:** "Zooplankton are smaller in warm water because oxygen is lower"
- Test: Does body size ~ temperature relationship disappear after controlling for DO?
- Use DO concentration (mg/L)

**Hypothesis 2:** "Zooplankton are smaller when oxygen stress (saturation) is high"
- Test: Does body size ~ saturation relationship remain after controlling for temperature?
- Use DO saturation (%)

**Hypothesis 3:** "Body size responds to absolute oxygen requirements"
- Test: Body size ~ DO concentration (independent of temperature)
- Use DO concentration (mg/L)

**Hypothesis 4:** "Body size responds to ecosystem-wide oxygen stress"
- Test: Body size ~ DO saturation (showing photosynthesis/respiration balance)
- Use DO saturation (%)

---

## Data Availability in Your Pipeline

### Current Setup (Scripts 05-19)
- ✅ Body size: available
- ✅ Temperature: available (hierarchical matching)
- ✅ DO concentration: available (hierarchical matching from Script 15)
- ⚠️ DO saturation: NOT calculated (saturation columns in raw file are invalid)

### Enhanced Setup (with Scripts 15d-15e)
- ✅ Body size: available
- ✅ Temperature: available
- ✅ DO concentration: available
- ✅ DO saturation: NOW available (calculated from Scripts 15d-15e)

---

## Technical Details

### Garcia-Gordon Equation Validation
- **Source:** Garcia & Gordon (1992) Limnol. Oceanogr.
- **Uncertainty:** ±1-2% for freshwater applications
- **Temperature validity:** 0-35°C (NEON data range: ~2-30°C ✓)
- **Salinity:** Equation assumes freshwater (NEON lakes ✓)
- **Pressure:** Standard atmospheric pressure assumed (slight error at high elevation, ~1-2%)

### When Saturation Calculation Is Invalid
- ❌ Marine/estuarine ecosystems (need salinity adjustment)
- ❌ Very high altitude (>2500m, need pressure correction)
- ❌ Extreme temperatures (<0°C or >40°C, outside equation range)

**Your NEON lakes:** All calculations are valid ✓

---

## Making a Decision: Concentration or Saturation?

### Decide Based on Your Question

**Use DO Concentration (mg/L) if:**
- Your hypothesis is about absolute oxygen availability
- You're comparing to published hypoxia thresholds (e.g., "harmful at <2 mg/L")
- You want simplicity (no additional calculations)
- You're worried about oxygen-limited metabolic rates

**Use DO Saturation (%) if:**
- Your hypothesis is about relative oxygen stress across seasons
- You're comparing ecosystems with different baseline temperatures
- You want to understand ecosystem productivity (peaks >100% from photosynthesis)
- You want to account for temperature-dependent solubility differences

**Use BOTH if:**
- You want to understand whether zooplankton respond to absolute vs relative oxygen
- You're doing sensitivity analysis
- You're publishing and want comprehensive oxygen assessment

---

## Implementation in Script 19

### Option 1: Keep Current Analysis
No changes needed. Script 19 currently uses DO concentration (`do_mean`).

### Option 2: Switch to Saturation
Requires:
1. Run Scripts 15d & 15e
2. Modify Script 16b to merge saturation instead of concentration (change column names)
3. Update Script 19 to use `meanDO_sat_pct_mean` instead of `do_mean`

### Option 3: Include Both (Recommended)
Requires:
1. Run Scripts 15d & 15e
2. Modify Script 16b to merge BOTH datasets
3. Update Script 19 to test both metrics separately or together

---

## Questions to Consider

1. **Temporal pattern:** Does DO concentration drop in summer because water warms (solubility) or because of respiration?
   - **Answer:** Use saturation. If saturation drops too, it's respiration. If saturation stays flat, it's temperature.

2. **Zooplankton body size:** Do zooplankton get smaller in summer because of low DO or because of temperature itself?
   - **Answer:** Compare two models (conc vs sat). If saturation explains more, it's about oxygen stress. If both similar, both matter.

3. **Ecological threshold:** What oxygen level stresses zooplankton?
   - **Answer:** Probably saturation. They evolved in their native temperature range at certain saturation levels.

---

## Summary Table: When to Use Each

| Situation | Use | Reason |
|-----------|-----|--------|
| Toxicology/regulation | Concentration | Absolute thresholds |
| Comparing spring vs summer | Saturation | Temperature-normalized |
| Absolute metabolic need | Concentration | "Need ≥5 mg/L O₂" |
| Ecosystem health | Saturation | Reflects photosynthesis/respiration balance |
| Hypoxia detection (routine) | Concentration | Easy to measure, standard practice |
| Hypoxia mechanistic understanding | Both | One shows "what," other shows "why" |
| Publishing in ecology journal | Saturation | More sophisticated interpretation |
| Publishing in toxicology journal | Concentration | Standard approach in field |

---

## References & Further Reading

- Garcia, H. E., and L. I. Gordon. 1992. Oxygen solubility in seawater: Better fitting equations. *Limnol. Oceanogr.* 37(6): 1307-1312.
- Wetzel, R. G. 2001. *Limnology: Lake and River Ecosystems*. 3rd ed. Academic Press. (Chapter on dissolved gases)
- USGS NWQL protocols for DO saturation calculations
- APHA Standard Methods for water analysis (Method 4500-O)

---

**Created:** May 3, 2026  
**Status:** Ready for implementation  
**Scripts:** 15d (calculate), 15e (aggregate), modified 16b (optional), modified 19 (optional)
