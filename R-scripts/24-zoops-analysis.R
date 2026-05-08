


#### supply demand model testing
library(tidyverse)
library(visreg)
library(cowplot)
theme_set(theme_cowplot())
library(ggridges)

k <- 8.617333e-5  # Boltzmann's constant in eV/K

celsius_to_invkT <- function(temp_c) {
  temp_k <- temp_c + 273.15
  1 / (k * temp_k)
}

names(zoops)

locations <- read_csv("data-processed/neon_lake_sites_coordinates.csv") |> 
  rename(siteID = site_code)

zoops <- read_csv("data-processed/zoo-chl-temp.csv") |> 
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |>
  filter(exclude == "include") |>
  filter(chl_source == "Discrete samples") |> 
  filter(!is.na(chl_mean), !is.na(mean_body_length)) |> 
  left_join(locations) |> 
  mutate(siteID = forcats::fct_reorder(siteID, latitude, .desc = TRUE))


zoops |> 
  # filter(taxonID == "CALSP1") |> 
  ggplot(aes(x = mean_body_length)) + geom_density() +
  facet_grid(siteID~ taxonID, scales = "free")
ggsave("figures/zoop-body-size-dist-all.png", width = 20, height = 20)



zoops |> 
  filter(taxonID == "CALSP1") |> 
  ggplot(aes(x = mean_body_length)) + geom_density() +
  facet_grid(siteID~ taxonID, scales = "free")
ggsave("figures/zoop-body-size-dist-calsp1.png", width = 10, height = 15)

zoops |>
  filter(taxonID == "CYCSP") |>
  ggplot(aes(x = mean_body_length)) + geom_density() +
  facet_grid(siteID ~ taxonID, scales = "free")
ggsave("figures/zoop-body-size-dist-cycsp.png", width = 10, height = 15)


# ridgeline density plot for each taxon, saved to figures/
library(ggridges)

taxa_list <- unique(zoops$taxonID)

for (taxon in taxa_list) {
  p <- zoops |>
    filter(taxonID == taxon) |>
    ggplot(aes(x = mean_body_length, y = siteID, fill = siteID)) +
    geom_density_ridges(alpha = 0.5) +
    labs(title = taxon, x = "Mean body length (mm)", y = NULL) +
    theme(legend.position = "none")

  filename <- paste0("figures/zoop-body-size-dist-", tolower(taxon), ".png")
  ggsave(filename, plot = p, width = 8, height = 6)
}



zoops |>
  ggplot(aes(x = mean_body_length, y = taxonID, fill = taxonID)) +
  geom_density_ridges(alpha = 0.5) +
  facet_grid(~ siteID) +
  theme(legend.position = "none")
ggsave("figures/zp-size-overlap.png", width = 15, height = 12)


zoops |>
  ggplot(aes(x = mean_body_length, y = siteID, fill = siteID)) +
  geom_density_ridges(alpha = 0.5) +
  facet_grid(~ taxonID) +
  theme(legend.position = "none")
ggsave("figures/zp-size-overlap-site.png", width = 20, height = 12)


zoops |>
  ggplot(aes(x = mean_body_length, fill = taxonID)) +
  geom_density(alpha = 0.3) +
  facet_grid(~ siteID)


zoops |> 
  filter(taxonID == "CALSP1") |> 
  ggplot(aes(y = mean_body_length, x = collectDate, color = factor(month))) + geom_point() + geom_line() +
  facet_grid( ~ year, col =1)
ggsave("figures/zoop-body-size-dist-month-calsp.png", width = 10, height = 15)

zoops |> 
  # filter(taxonID == "") |> 
  filter(siteID == "TOOK") |> 
  ggplot(aes(y = mean_body_length, x = collectDate, color = factor(month))) + geom_point() + geom_line() +
  facet_wrap(year ~ siteID, scales = "free")


# Dumont et al. 1975, Calanoida pooled equation
# prosome length in mm, returns dry mass in µg
calanoid_mass_ug <- function(prosome_mm) {
  W_mg <- 0.0077 * prosome_mm^2.33
  W_mg * 1000  # convert mg to µg
}



cals <- zoops |> 
  filter(taxonID == "CALSP1") |> 
  mutate(log_size = log(mean_body_length),
         log_temp = log(temp_mean),
         log_chl = log(chl_mean),
         log_supply_demand = log(chl_mean/temp_mean)) |> 
  mutate(inv_temp = celsius_to_invkT(temp_mean)*-1) |> 
  mutate(log_mass = log(calanoid_mass_ug(mean_body_length)))


cals_mod <- lm(log_mass ~ log_chl + inv_temp, data = cals)
cals_mod1 <- lm(log_mass ~ inv_temp, data = cals)

AIC(cals_mod, cals_mod1)

summary(cals_mod)
summary(cals_mod1)



summary(cals_mod)
visreg(cals_mod)
visreg(cals_mod, "inv_temp", "log_chl", gg=TRUE, ylab="Log body mass (ug)", xlab = "Temperature (-1/kT)")
ggsave("figures/partial-regression-cals.png", width = 10, height = 6)

visreg(cals_mod, "log_chl", gg=TRUE, ylab="Log body mass (ug)", xlab = "Mean chlorophyll (ug/L)")
ggsave("figures/partial-regression-cals-chla.png", width = 6, height = 4)


cals |> 
  ggplot(aes(x = mean_temp, y = mean_body_length)) + geom_point() +
  geom_smooth(method = "lm")



cyc <- zoops |> 
  filter(taxonID == "CYCSP")


cyc_mod <- lm(mean_body_length ~ temp_mean*chl_mean, data = cyc)
summary(cyc_mod)
visreg(cyc_mod, "temp_mean", "chl_mean")


cyc1 <- zoops |> 
  filter(taxonID == "CYCSP1")

 
cyc1_mod <- lm(mean_body_length ~ temp_mean*chl_mean, data = cyc1)
summary(cyc1_mod)

dia <- zoops |> 
  filter(taxonID == "DIASP10")


dia_mod <- lm(mean_body_length ~ temp_mean*chl_mean, data = dia)
summary(dia_mod)




all_zoop_data <- read_csv("data-processed/zooplankton_body_size_temp_food_supply_hierarchical.csv") |>
  left_join(locations) |>
  mutate(siteID = forcats::fct_reorder(siteID, latitude, .desc = TRUE)) |> 
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |>
  filter(exclude == "include")

# normalized abundance size spectrum (NASS)
# approach: bin taxa by mean body length on log scale, sum count_per_liter within
# each bin, then divide by bin width to normalize

n_bins <- 10  # number of log-spaced bins across the size range

nass <- all_zoop_data |>
  filter(!is.na(mean_body_length), !is.na(count_per_liter)) |>
  mutate(log_length = log10(mean_body_length)) |>
  mutate(size_bin = cut(log_length,
                        breaks = seq(min(log_length, na.rm = TRUE),
                                     max(log_length, na.rm = TRUE),
                                     length.out = n_bins + 1),
                        include.lowest = TRUE)) |>
  group_by(siteID, size_bin) |>
  summarise(total_abundance = sum(count_per_liter, na.rm = TRUE),
            bin_midpoint = mean(log_length, na.rm = TRUE),
            .groups = "drop") |>
  mutate(bin_width = (max(bin_midpoint) - min(bin_midpoint)) / n_bins,
         norm_abundance = total_abundance / bin_width)

# identify and exclude the first (smallest) bin before fitting slopes
first_bin <- nass |> filter(norm_abundance > 0) |> pull(bin_midpoint) |> min()

# fit slope per site on log-log scale, excluding first bin
nass_slopes <- nass |>
  filter(norm_abundance > 0, bin_midpoint > first_bin) |>
  group_by(siteID) |>
  summarise(slope = coef(lm(log10(norm_abundance) ~ bin_midpoint))[2],
            .groups = "drop") |>
  mutate(label = paste0("slope = ", round(slope, 2)))

nass |>
  filter(norm_abundance > 0) |>
  ggplot(aes(x = bin_midpoint, y = norm_abundance, color = siteID)) +
  geom_point(size = 2) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_text(data = nass_slopes, aes(label = label),
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
            color = "black", size = 3) +
  scale_y_log10() +
  facet_grid(siteID ~ .) +
  labs(x = "Log10 body length (mm)", y = "Normalized abundance (count per litre per bin width)") +
  theme(legend.position = "none")
ggsave("figures/zoop-nass-by-site.png", width = 5, height = 12)

# size spectrum by taxon (top 10 most frequently sampled)
top10_taxa <- all_zoop_data |>
  filter(!is.na(mean_body_length), !is.na(count_per_liter)) |>
  count(taxonID, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(taxonID)

nass_taxon <- all_zoop_data |>
  filter(!is.na(mean_body_length), !is.na(count_per_liter),
         taxonID %in% top10_taxa) |>
  mutate(log_length = log10(mean_body_length)) |>
  mutate(size_bin = cut(log_length,
                        breaks = seq(min(log_length, na.rm = TRUE),
                                     max(log_length, na.rm = TRUE),
                                     length.out = n_bins + 1),
                        include.lowest = TRUE)) |>
  group_by(siteID, taxonID, size_bin) |>
  summarise(total_abundance = sum(count_per_liter, na.rm = TRUE),
            bin_midpoint = mean(log_length, na.rm = TRUE),
            .groups = "drop") |>
  mutate(bin_width = (max(bin_midpoint) - min(bin_midpoint)) / n_bins,
         norm_abundance = total_abundance / bin_width)

first_bin_taxon <- nass_taxon |> filter(norm_abundance > 0) |> pull(bin_midpoint) |> min()

nass_taxon |>
  filter(norm_abundance > 0) |>
  ggplot(aes(x = bin_midpoint, y = norm_abundance, color = taxonID)) +
  geom_point(size = 2) +
  geom_line() +
  geom_smooth(data = ~ filter(., bin_midpoint > first_bin_taxon),
              method = "lm", se = FALSE, linetype = "dashed") +
  scale_y_log10() +
  facet_grid(siteID ~ .) +
  labs(x = "Log10 body length (mm)", y = "Normalized abundance (count per litre per bin width)",
       color = "Taxon")
ggsave("figures/zoop-nass-by-taxon.png", width = 10, height = 15)

# slope per taxon x site vs latitude
nass_taxon_site_slopes <- nass_taxon |>
  filter(norm_abundance > 0, bin_midpoint > first_bin_taxon) |>
  group_by(siteID, taxonID) |>
  filter(n() >= 3) |>  # need at least 3 bins to fit a slope
  summarise(slope = coef(lm(log10(norm_abundance) ~ bin_midpoint))[2],
            .groups = "drop") |>
  left_join(locations)

nass_taxon_site_slopes |>
  ggplot(aes(x = latitude, y = slope, color = taxonID)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  labs(x = "Latitude", y = "Size spectrum slope", color = "Taxon") + facet_wrap( ~ taxonID, scales = "free")
ggsave("figures/zoop-nass-slope-vs-latitude-by-taxon.png", width = 12, height = 5)

# slope of size spectrum vs latitude
nass_slopes |>
  left_join(locations) |>
  ggplot(aes(x = latitude, y = slope, label = siteID)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  ggrepel::geom_label_repel(size = 3) +
  labs(x = "Latitude", y = "Size spectrum slope")
ggsave("figures/zoop-nass-slope-vs-latitude.png", width = 6, height = 5)




