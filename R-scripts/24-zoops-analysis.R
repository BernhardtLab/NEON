


#### supply demand model testing
library(tidyverse)
library(visreg)
library(cowplot)
theme_set(theme_cowplot())

k <- 8.617333e-5  # Boltzmann's constant in eV/K

celsius_to_invkT <- function(temp_c) {
  temp_k <- temp_c + 273.15
  1 / (k * temp_k)
}

names(zoops)

locations <- read_csv("data-processed/neon_lake_sites_coordinates.csv") |> 
  rename(siteID = site_code)

zoops <- read_csv("data-processed/zoo-chl-temp.csv") |> 
  # mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
  #                            taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
  #                            taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
  #                            TRUE ~ "include")) |> 
  # filter(exclude == "include") |> 
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


# density plot for each taxon, saved to figures/
taxa_list <- unique(zoops$taxonID)

for (taxon in taxa_list) {
  p <- zoops |>
    filter(taxonID == taxon) |>
    ggplot(aes(x = mean_body_length)) + geom_density() +
    facet_grid(siteID ~ taxonID, scales = "free")

  filename <- paste0("figures/zoop-body-size-dist-", tolower(taxon), ".png")
  ggsave(filename, plot = p, width = 10, height = 15)
}



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





