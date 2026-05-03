
library(cowplot)
theme_set(theme_cowplot())

## Zooplankton Analysis

all_zoop_data <- read_csv("data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv")

# ============================================================================
# Identify Top 10 Most Abundant Zooplankton Taxa
# ============================================================================

cat("Identifying top 10 most abundant zooplankton taxa...\n\n")

top_10_taxa <- all_zoop_data |>
  group_by(taxonID) |>
  summarise(n_records = n(), .groups = "drop") |>
  arrange(desc(n_records)) |>
  head(10) |>
  pull(taxonID)

cat("Top 10 most frequent taxa (by number of records):\n")
print(top_10_taxa)
cat("\n")

# ============================================================================
# Filter to Top 10 Taxa
# ============================================================================

top_zoop <- all_zoop_data |>
  filter(taxonID %in% top_10_taxa)

cat("Records in top 10 taxa:", nrow(top_zoop), "\n\n")

# ============================================================================
# Plot 1: Body Size Variation Across Sites (Top 10 Taxa)
# ============================================================================

cat("Creating plot: Body size variation across sites...\n\n")

p1_body_size_by_site <- top_zoop |>
  ggplot(aes(x = temp_mean, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~taxonID, scales = "free_y", ncol = 5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  geom_smooth(method = "lm")

print(p1_body_size_by_site)

# ============================================================================
# Analyze: How does body size change with temperature?
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("BODY SIZE vs TEMPERATURE ANALYSIS (Top 10 Taxa)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Filter to data with both body size and temperature
body_size_temp_data <- top_zoop |>
  filter(!is.na(mean_body_length), !is.na(temp_mean)) |>
  select(siteID, taxonID, mean_body_length, temp_mean)

cat("Records with both body size and temperature data:", nrow(body_size_temp_data), "\n\n")

# ============================================================================
# Plot 2: Body Size vs Temperature (All Top Taxa Together)
# ============================================================================

cat("Creating plot: Body size vs temperature...\n\n")

p2_body_size_vs_temp <- body_size_temp_data |>
  ggplot(aes(x = temp_mean, y = mean_body_length, color = taxonID)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "black") +
  labs(
    title = "Zooplankton Body Size vs Temperature",
    subtitle = "Top 10 most frequent taxa",
    x = "Temperature (°C)",
    y = "Mean Body Length (mm)",
    color = "Taxon"
  ) +
  theme(legend.position = "right")

print(p2_body_size_vs_temp)

# ============================================================================
# Plot 3: Body Size vs Temperature (Faceted by Taxon)
# ============================================================================

cat("Creating plot: Body size vs temperature by taxon (faceted)...\n\n")

p3_body_size_vs_temp_facet <- body_size_temp_data |>
  ggplot(aes(x = temp_mean, y = mean_body_length)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "red") +
  facet_wrap(~taxonID, scales = "free", ncol = 5) +
  labs(
    title = "Body Size vs Temperature by Taxon",
    x = "Temperature (°C)",
    y = "Mean Body Length (mm)"
  ) +
  theme(axis.text.x = element_text(size = 8))

print(p3_body_size_vs_temp_facet)

# ============================================================================
# Statistical Analysis: Per-Taxon Regressions
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("REGRESSION RESULTS: Body Size ~ Temperature\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Run regression for each taxon
taxon_results <- list()

for (taxon in top_10_taxa) {
  taxon_data <- body_size_temp_data |>
    filter(taxonID == taxon)

  if (nrow(taxon_data) >= 3) {  # Need at least 3 points for regression
    lm_result <- lm(mean_body_length ~ temp_mean, data = taxon_data)
    lm_summary <- summary(lm_result)

    slope <- lm_result$coefficients[2]
    intercept <- lm_result$coefficients[1]
    r_squared <- lm_summary$r.squared
    p_value <- lm_summary$coefficients[2, 4]
    n_obs <- nrow(taxon_data)

    taxon_results[[taxon]] <- data.frame(
      taxonID = taxon,
      n_observations = n_obs,
      intercept = round(intercept, 4),
      slope = round(slope, 6),
      r_squared = round(r_squared, 4),
      p_value = format(p_value, scientific = TRUE, digits = 3),
      significant = if_else(p_value < 0.05, "Yes", "No"),
      pattern = if_else(slope < 0, "Larger in cool", "Larger in warm")
    )
  } else {
    cat("Taxon", taxon, "has only", nrow(taxon_data), "observations - skipping regression\n")
  }
}

# Combine results into a table
taxon_summary <- bind_rows(taxon_results) |>
  arrange(p_value)

cat("Summary of regressions:\n\n")
print(taxon_summary)

# Count patterns
larger_cool <- sum(taxon_summary$pattern == "Larger in cool", na.rm = TRUE)
larger_warm <- sum(taxon_summary$pattern == "Larger in warm", na.rm = TRUE)
sig_taxa <- sum(taxon_summary$significant == "Yes", na.rm = TRUE)

cat("\n\nKEY FINDINGS:\n")
cat("  Taxa larger in cool conditions:", larger_cool, "\n")
cat("  Taxa larger in warm conditions:", larger_warm, "\n")
cat("  Significant relationships (p < 0.05):", sig_taxa, "\n\n")

# ============================================================================
# Environmental Analysis: How do AFDM and DO vary with Temperature?
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ENVIRONMENTAL CONDITIONS: How AFDM and DO vary with Temperature\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Prepare data with environmental variables
env_data <- top_zoop |>
  filter(!is.na(mean_body_length), !is.na(temp_mean), !is.na(afdm_mean), !is.na(do_mean)) |>
  select(siteID, taxonID, temp_mean, afdm_mean, do_mean)

cat("Records with complete environmental data:", nrow(env_data), "\n\n")

# ============================================================================
# Plot 4: Food Availability (AFDM) vs Temperature
# ============================================================================

cat("Creating plot: AFDM vs Temperature...\n\n")

p4_afdm_vs_temp <- env_data |>
  ggplot(aes(x = temp_mean, y = afdm_mean, color = siteID)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "darkgreen", aes(color = NULL)) +
  labs(
    title = "Food Availability vs Temperature",
    x = "Temperature (°C)",
    y = "Algal AFDM (μg/L)",
    color = "Site",
    subtitle = "How does phytoplankton/seston biomass change with temperature?"
  ) +
  theme(legend.position = "right")

print(p4_afdm_vs_temp)

# ============================================================================
# Plot 5: Dissolved Oxygen vs Temperature
# ============================================================================

cat("Creating plot: DO vs Temperature...\n\n")

p5_do_vs_temp <- env_data |>
  ggplot(aes(x = temp_mean, y = do_mean, color = siteID)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue", aes(color = NULL)) +
  labs(
    title = "Dissolved Oxygen vs Temperature",
    x = "Temperature (°C)",
    y = "Dissolved Oxygen (mg/L)",
    color = "Site",
    subtitle = "How does oxygen availability change with temperature?"
  ) +
  theme(legend.position = "right")

print(p5_do_vs_temp)

# ============================================================================
# Regression Analysis: AFDM and DO vs Temperature
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STATISTICAL RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# AFDM vs Temperature
lm_afdm <- lm(afdm_mean ~ temp_mean, data = env_data)
lm_afdm_summary <- summary(lm_afdm)

cat("AFDM (Food Availability) vs Temperature:\n")
cat("  Slope:", round(lm_afdm$coefficients[2], 6), "μg/L per °C\n")
cat("  R-squared:", round(lm_afdm_summary$r.squared, 4), "\n")
cat("  P-value:", format(lm_afdm_summary$coefficients[2, 4], scientific = TRUE, digits = 3), "\n")
cat("  Pattern:", if_else(lm_afdm$coefficients[2] < 0, "More food in cool conditions", "More food in warm conditions"), "\n\n")

# DO vs Temperature
lm_do <- lm(do_mean ~ temp_mean, data = env_data)
lm_do_summary <- summary(lm_do)

cat("Dissolved Oxygen vs Temperature:\n")
cat("  Slope:", round(lm_do$coefficients[2], 6), "mg/L per °C\n")
cat("  R-squared:", round(lm_do_summary$r.squared, 4), "\n")
cat("  P-value:", format(lm_do_summary$coefficients[2, 4], scientific = TRUE, digits = 3), "\n")
cat("  Pattern:", if_else(lm_do$coefficients[2] < 0, "Higher oxygen in cool conditions", "Higher oxygen in warm conditions"), "\n\n")

# ============================================================================
# Per-Taxon Analysis: Body Size Response to Temperature, DO, and AFDM
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("PER-TAXON ANALYSIS: Body Size Response to Environmental Variables\n")
cat("Testing: Body Size ~ Temperature + Dissolved Oxygen + Food Availability (AFDM)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Collect results for all taxa
taxon_env_results <- list()

# Loop through each taxon
for (taxon in top_10_taxa) {
  cat("\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat("TAXON:", taxon, "\n")
  cat(paste(rep("-", 80), collapse = ""), "\n\n")

  # Filter data for this taxon with all variables needed
  taxon_env_data <- top_zoop |>
    filter(taxonID == taxon) |>
    select(siteID, taxonID, mean_body_length, temp_mean, afdm_mean, do_mean) |>
    filter(!is.na(mean_body_length) & !is.na(temp_mean) & !is.na(afdm_mean) & !is.na(do_mean))

  cat("Records with complete data:", nrow(taxon_env_data), "\n\n")

  if (nrow(taxon_env_data) >= 4) {  # Need at least 4 points for 3 predictors

    # ====================================================================
    # Visualization: Body Size vs Environmental Variables
    # ====================================================================

    # Create a 3-panel plot
    p_temp <- taxon_env_data |>
      ggplot(aes(x = temp_mean, y = mean_body_length, color = siteID)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2) +
      labs(title = paste(taxon, "- Body Size vs Temperature"),
           x = "Temperature (°C)", y = "Body Length (mm)") +
      theme(legend.position = "none")

    p_do <- taxon_env_data |>
      ggplot(aes(x = do_mean, y = mean_body_length, color = siteID)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2) +
      labs(title = paste(taxon, "- Body Size vs DO"),
           x = "Dissolved Oxygen (mg/L)", y = "Body Length (mm)") +
      theme(legend.position = "none")

    p_afdm <- taxon_env_data |>
      ggplot(aes(x = afdm_mean, y = mean_body_length, color = siteID)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2) +
      labs(title = paste(taxon, "- Body Size vs AFDM"),
           x = "Algal AFDM (μg/L)", y = "Body Length (mm)") +
      theme(legend.position = "none")

    # Combine and print
    p_combined <- plot_grid(p_temp, p_do, p_afdm, ncol = 3)
    print(p_combined)

    # ====================================================================
    # Multiple Regression: Body Size ~ Temperature + DO + AFDM
    # ====================================================================

    lm_multi <- lm(mean_body_length ~ temp_mean + do_mean + afdm_mean,
                   data = taxon_env_data)
    lm_multi_summary <- summary(lm_multi)

    cat("MULTIPLE REGRESSION: Body Size ~ Temperature + DO + AFDM\n")
    cat("  R-squared:", round(lm_multi_summary$r.squared, 4), "\n")
    cat("  Adjusted R-squared:", round(lm_multi_summary$adj.r.squared, 4), "\n")
    cat("  Overall p-value:", format(pf(lm_multi_summary$fstatistic[1],
                                         lm_multi_summary$fstatistic[2],
                                         lm_multi_summary$fstatistic[3],
                                         lower.tail = FALSE),
                           scientific = TRUE, digits = 3), "\n\n")

    cat("  COEFFICIENTS:\n")
    cat("    Temperature:  ", round(lm_multi$coefficients[2], 6),
        " (p =", format(lm_multi_summary$coefficients[2, 4], scientific = TRUE, digits = 2), ")\n")
    cat("    Dissolved O2:  ", round(lm_multi$coefficients[3], 6),
        " (p =", format(lm_multi_summary$coefficients[3, 4], scientific = TRUE, digits = 2), ")\n")
    cat("    AFDM:          ", round(lm_multi$coefficients[4], 6),
        " (p =", format(lm_multi_summary$coefficients[4, 4], scientific = TRUE, digits = 2), ")\n\n")

    # ====================================================================
    # Individual Regressions for Comparison
    # ====================================================================

    cat("INDIVIDUAL REGRESSIONS (for comparison):\n\n")

    # Temperature only
    lm_temp <- lm(mean_body_length ~ temp_mean, data = taxon_env_data)
    lm_temp_summary <- summary(lm_temp)
    cat("  Temperature only:\n")
    cat("    Slope:", round(lm_temp$coefficients[2], 6),
        " | R²:", round(lm_temp_summary$r.squared, 4),
        " | p:", format(lm_temp_summary$coefficients[2, 4], scientific = TRUE, digits = 2), "\n")

    # DO only
    lm_do_ind <- lm(mean_body_length ~ do_mean, data = taxon_env_data)
    lm_do_summary <- summary(lm_do_ind)
    cat("  Oxygen only:\n")
    cat("    Slope:", round(lm_do_ind$coefficients[2], 6),
        " | R²:", round(lm_do_summary$r.squared, 4),
        " | p:", format(lm_do_summary$coefficients[2, 4], scientific = TRUE, digits = 2), "\n")

    # AFDM only
    lm_afdm_ind <- lm(mean_body_length ~ afdm_mean, data = taxon_env_data)
    lm_afdm_summary <- summary(lm_afdm_ind)
    cat("  AFDM only:\n")
    cat("    Slope:", round(lm_afdm_ind$coefficients[2], 6),
        " | R²:", round(lm_afdm_summary$r.squared, 4),
        " | p:", format(lm_afdm_summary$coefficients[2, 4], scientific = TRUE, digits = 2), "\n\n")

    # Store results for summary table
    taxon_env_results[[taxon]] <- data.frame(
      taxonID = taxon,
      n_obs = nrow(taxon_env_data),
      model_r2 = round(lm_multi_summary$r.squared, 4),
      temp_coef = round(lm_multi$coefficients[2], 6),
      temp_p = round(lm_multi_summary$coefficients[2, 4], 4),
      do_coef = round(lm_multi$coefficients[3], 6),
      do_p = round(lm_multi_summary$coefficients[3, 4], 4),
      afdm_coef = round(lm_multi$coefficients[4], 6),
      afdm_p = round(lm_multi_summary$coefficients[4, 4], 4)
    )

  } else {
    cat("Insufficient data for this taxon - skipping analysis\n\n")
  }
}

# ============================================================================
# Summary Table
# ============================================================================

cat("\n\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY TABLE: Per-Taxon Results\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

summary_table <- bind_rows(taxon_env_results)
print(summary_table)

cat("\nNote: Positive coefficient = larger body size with higher variable value\n")
cat("      Negative coefficient = larger body size with lower variable value\n")


t1 <- top_zoop |> 
  filter(taxonID == "CALSP1")

t1 |> 
  ggplot(aes(x = temp_mean, y = mean_body_length)) + geom_point() + geom_smooth(method = "lm")

t1 |> 
  ggplot(aes(x = do_mean, y = mean_body_length)) + geom_point() + geom_smooth(method = "lm")

t1 |> 
  ggplot(aes(x = afdm_mean, y = mean_body_length)) + geom_point() + geom_smooth(method = "lm")

t1 |> 
  ggplot(aes(x = do_mean, y = temp_mean)) + geom_point() + geom_smooth(method = "lm")

t1_mod <- lm(mean_body_length ~ temp_mean, data = t1)

summary(t1_mod)
  
