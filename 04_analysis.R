

#### Neon fish data analysis
### Streams we have 27 sites
### Phytoplankton
## Chlorophyll





library(tidyverse)
library(janitor)
library(lubridate)

neon_fish <- readRDS('2025 Raw Release Data/neonFish.Robj')


zdata <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG", "TOOK")


perfish <- neon_fish$fsh_perFish %>% 
  clean_names() %>% 
  mutate(year = year(bout_end_date))


perpass <- neon_fish$fsh_perPass %>% 
  clean_names()


do <- perpass %>%
  mutate(year = year(bout_end_date)) %>% 
  group_by(year, site_id) %>% 
  summarise(mean_do = mean(dissolved_oxygen, na.rm = TRUE),
            mean_temp = mean(water_temp, na.rm = TRUE))

(unique(perfish$scientific_name))



fish_little <- perfish %>% 
  select(site_id, fish_total_length, scientific_name, fish_life_stage)

temp_little <- perpass %>% 
  select(site_id, water_temp)


fish_temp <- fish_little %>% 
  left_join(temp_little)


lots_fish <- perfish %>% 
  filter(site_id %in% zdata) %>% 
  filter(!is.na(fish_total_length)) %>% 
  group_by(scientific_name) %>% 
  tally() %>% 
  filter(n>100)

write_csv(lots_fish, "data-processed/lots_fish.csv")



lots_fish_all <- perfish %>% 
  # filter(site_id %in% zdata) %>% 
  filter(!is.na(fish_total_length)) %>% 
  group_by(scientific_name) %>% 
  tally() %>%
  filter(n > 500)

write_csv(lots_fish_all, "data-processed/lots_fish_all_sites.csv")





fish_temp_sel <- fish_temp %>% 
  filter(scientific_name %in% lots_fish$scientific_name)



  fish_temp_sel %>% 
  ggplot(aes(x = water_temp, y = fish_total_length, color = fish_life_stage)) + geom_point() +
  geom_smooth(method= "lm") +
    facet_wrap( ~ scientific_name, scales = "free")
ggsave("figures/length-temp.png", width = 25, height = 22)


perfish2 <- perfish %>% 
  left_join(perpass) %>% 
  filter(!is.na(fish_total_length))

lots_fish <- perfish %>% 
  filter(!is.na(fish_total_length)) %>% 
  group_by(scientific_name) %>% 
  tally() %>%
  filter(n>1000)


perfish2 %>% 
  filter(scientific_name %in% lots_fish$scientific_name) %>%
  # filter(fish_life_stage == "adult") %>%
  # group_by(year, scientific_name, mean_temp, site_id) %>% 
  # summarise(mean_length = mean(fish_total_length)) %>% 
  ggplot(aes(x = water_temp, y = fish_total_length)) + geom_jitter(width = 0.1) +
  facet_wrap(~ scientific_name, scales = "free") +geom_smooth(method = "lm")



# is body size related to macroinvert biomass? -----------------------------



p7 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(body_condition = 100*(fish_weight/fish_total_length^3))



p7 %>% 
  group_by(scientific_name) %>% 
  tally() %>% View



perfish %>% 
  filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(body_condition = 100*(fish_weight/fish_total_length^3)) %>% 
  ggplot(aes(x = bout_end_date, y = fish_weight)) + geom_point()


zoo <- read_csv("Clean Data/zooplankton.csv")


zoo2 <- zoo %>% 
  group_by(siteID,collectDate) %>%
  mutate(avg_length = (zooMaximumLength + zooMinimumLength)/2) %>%
  mutate(biomass_proxy = avg_length*countPerL) %>% 
  summarise(mean_biomass =mean(biomass_proxy,na.rm=T))


zoo2 %>%
  ggplot(aes(x = collectDate, y = mean_biomass, 
             color = siteID,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("Biomass proxy") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID, scales = "free_y")
ggsave("figures/zooplankton-biomass-proxy.png", width = 8, height = 6)


zoo3 <- zoo2 %>% 
  mutate(year = year(collectDate)) %>% 
  group_by(year, siteID) %>% 
  summarise(mean_zoop_biomass = mean(mean_biomass, na.rm = TRUE)) %>% 
  rename(site_id = siteID)


fish_sum <- p7 %>% 
  mutate(year = year(bout_end_date)) %>% 
  group_by(site_id, scientific_name, year) %>% 
  summarise(mean_weight = mean(fish_weight, na.rm= TRUE))


fish_sum2 <- fish_sum %>% 
  left_join(zoo3, by = c("year", "site_id"))


fish_sum2 %>% 
  ggplot(aes(x = mean_zoop_biomass, y = log(mean_weight))) + geom_point() +
  geom_smooth(method = "lm")
ggsave("figures/weight-zoop-biomass.png", width = 8, height = 6)

mod1 <- lm(mean_weight ~ mean_zoop_biomass, data = fish_sum2)
summary(mod1)


mic_alg <- read_csv("Clean Data/micAlg_afdm.csv") %>% 
  rename(site_id = siteID.x) %>% 
  mutate(year = year(collectDate.x)) %>% 
  clean_names() %>% 
  group_by(site_id, year) %>% 
  summarise(mean_algae = mean(adj_ash_free_dry_mass, na.rm = TRUE))


fish_alg <- fish_sum %>% 
  left_join(mic_alg, by = c("site_id", "year"))


fish_alg %>% 
  # filter(mean_algae < 0.01) %>% 
  ggplot(aes(x= log(mean_algae), y = log(mean_weight))) + geom_point() +
  geom_smooth(method = "lm")
ggsave("figures/fish-weight-algae-mass.png", width = 8, height = 6)



# try without species level means -----------------------------------------

fish_all <- p7 %>% 
  mutate(year = year(bout_end_date)) %>% 
  left_join(mic_alg)


fish_all %>% 
  ggplot(aes(x= log(mean_algae), y = log(fish_weight))) + geom_point() +
  geom_smooth(method = "lm")
  

mod2 <- lm(log(fish_weight) ~ log(mean_algae), data = fish_all)
summary(mod2)


length(unique(fish_all$scientific_name))

fish_numbers <- fish_all %>% 
  group_by(scientific_name) %>% 
  tally() %>%
  filter(n > 50)

### Rhinichthys atratulus


fish_all %>% 
  filter(scientific_name %in% fish_numbers$scientific_name) %>% 
  ggplot(aes(x= log(mean_algae), y = log(fish_weight))) + geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ scientific_name, scales = "free")
ggsave("figures/fish-weight-algae-major-species.png", width = 15, height = 12)

fish_lots <- fish_all %>% 
  filter(scientific_name %in% fish_numbers$scientific_name)

mod3 <- lm(log(fish_weight) ~ log(mean_algae), data = fish_lots)
summary(mod3)




# now bring in DO ---------------------------------------------------------


neon_fish <- readRDS('2025 Raw Release Data/neonFish.Robj')



perpass <- neon_fish$fsh_perPass %>% 
  clean_names()


do <- perpass %>%
  mutate(year = year(bout_end_date)) %>% 
  group_by(year, site_id) %>% 
  summarise(mean_do = mean(dissolved_oxygen, na.rm = TRUE),
            mean_temp = mean(water_temp, na.rm = TRUE))


fish_lots2 <- fish_lots %>% 
  left_join(do)


fish_all2 <- fish_all %>% 
  left_join(do)


mod4 <- lm(log(fish_weight) ~ log(mean_algae) + mean_do + mean_temp, data = fish_all2)
summary(mod4)

library(visreg)

plot1 <- visreg(mod4, xvar = "mean_temp")




# set for testing the supply demand model ---------------------------------

neonZoops <- readRDS('2025 Raw Release Data/neonZoops.Robj')

tax <- neonZoops$zoo_taxonomyProcessed





