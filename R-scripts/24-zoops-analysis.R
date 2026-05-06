


#### supply demand model testing
library(tidyverse)
library(visreg)

zoops <- read_csv("data-processed/zoo-chl-temp.csv") |> 
  mutate(exclude = case_when(taxonID == "KERCOC" & mean_body_length > 0.5 ~ "exclude",
                             taxonID == "KELLON" & mean_body_length > 0.25 ~ "exclude",
                             taxonID == "POLSP20" & mean_body_length > 0.25 ~ "exclude",
                             TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  filter(chl_source == "Discrete samples") |> 
  filter(!is.na(chl_mean), !is.na(mean_body_length))

cals <- zoops |> 
  filter(taxonID == "CALSP1")


cals_mod <- lm(mean_body_length ~ temp_mean + chl_mean, data = cals)
summary(cals_mod)
visreg(cals_mod)
visreg(cals_mod, "temp_mean", "chl_mean", gg=TRUE, ylab="Mean body length", xlab = "Mean temperature")
ggsave("figures/partial-regression-cals.png", width = 10, height = 6)


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





