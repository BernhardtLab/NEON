

#### Issues of outliers
#### what's happening in the body condition data such that a bunch of points are getting dropped out?
### power analysis
### sampling effort?
#### Ask Ryan about the fish sampling effort data, how do they know what's a juvenile vs adult
#### how many times per year?
### add a column to the fish data for lake, river, stream



#### NEON data exploring

library(neonUtilities)
library(janitor)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(lubridate)


### try to get the fish data

fishes <- loadByProduct(dpID="DP1.20107.001", 
                           site=c("all"),
                           startdate= NA, 



perfish <- fishes$fsh_perFish %>% 
  clean_names()

write_csv(perfish, "data-processed/perfish_data.csv")
perfish <- read_csv("data-processed/perfish_data.csv")




neon_fish <- readRDS('2025 Raw Release Data/neonFish.Robj')


perfish <- neon_fish$fsh_perFish %>% 
  clean_names()

perpass <- neon_fish$fsh_perPass %>% 
  clean_names()



perpass %>% 
  ggplot(aes(x = pass_start_time, y = water_temp)) + geom_point() +
  facet_wrap( ~ site_id, scales = "free")
ggsave("figures/water_temp_from_fishing.png", width = 16, height = 12)

perpass %>%
  group_by(bout_end_date, site_id) %>% 
  summarise(mean_temp = mean(water_temp, na.rm = TRUE)) %>% 
  ggplot(aes(x = bout_end_date, y = mean_temp, color = site_id, group = site_id)) + geom_line() +
  facet_wrap( ~ site_id)
ggsave("figures/water_temp_from_fishing-line.png", width = 16, height = 12)

perpass %>%
  group_by(bout_end_date, site_id) %>% 
  summarise(mean_do = mean(dissolved_oxygen, na.rm = TRUE)) %>% 
  ggplot(aes(x = bout_end_date, y = mean_do, color = site_id, group = site_id)) + geom_line() +
  facet_wrap( ~ site_id)
ggsave("figures/do_from_fishing-line.png", width = 16, height = 12)



### CRAM,PRIN,  




str(perfish)
sum(is.na(perfish$fish_total_length))
sum(is.na(perfish$fish_weight))


perfish %>% 
  filter(fish_total_length == 0) %>% View





# p2 <- perfish %>% 
#   mutate(day = dmy(pass_start_time))



# p2 <- perfish %>% 
#   mutate(day = dmy(pass_start_time))


perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  ggplot(aes(x = pass_start_time, y = fish_total_length, color = site_id)) + geom_point() +
  ylab("Fish total length") + xlab("Date") + facet_wrap( ~ scientific_name)
ggsave("figures/length_time.png", width = 24, height = 22)


p3 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  group_by(scientific_name, site_id, pass_start_time) %>% 
  summarise(mean_length = mean(fish_total_length)) 

p4 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  group_by(scientific_name, site_id, pass_start_time) %>% 
  summarise(mean_length = mean(fish_total_length)) %>% 
  group_by(scientific_name, site_id) %>% 
  mutate(unique_points = n_distinct(mean_length)) %>% 
  mutate(year = year(pass_start_time)) %>% 
  group_by(scientific_name, site_id) %>% 
  mutate(unique_years = n_distinct(year)) %>% 
  filter(unique_points > 4, unique_years > 2)

p3 %>% 
  ggplot(aes(x = pass_start_time, y = mean_length, color = site_id)) + geom_point() +
  ylab("Mean fish total length") + xlab("Date") + facet_wrap( ~ scientific_name, scales = "free_y")
ggsave("figures/length_time-site-time-average-scales-free.png", width = 30, height = 22)

p4 %>% 
  ggplot(aes(x = pass_start_time, y = mean_length, color = site_id)) + geom_point() +
  ylab("Mean fish total length") + xlab("Date") + facet_wrap( ~ scientific_name, scales = "free_y") +
  geom_smooth(method = "lm")
ggsave("figures/length_time-site-time-average-scales-free-more-than2years.png", width = 30, height = 22)

p5 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(body_condition = 100*(fish_weight/fish_total_length^3)) %>% 
  group_by(scientific_name, site_id, pass_start_time) %>% 
  summarise(mean_mass = mean(fish_weight),
            mean_length = mean(fish_total_length),
            mean_body_condition = mean(body_condition)) %>% 
  group_by(scientific_name, site_id) %>% 
  mutate(unique_points = n_distinct(mean_mass)) %>% 
  mutate(year = year(pass_start_time)) %>% 
  group_by(scientific_name, site_id) %>% 
  mutate(unique_years = n_distinct(year)) %>% 
  filter(unique_points > 4, unique_years > 2)

p7 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(body_condition = 100*(fish_weight/fish_total_length^3)) %>%
  mutate(unique_points = n_distinct(fish_weight)) %>% 
  mutate(year = year(pass_start_time)) %>% 
  group_by(scientific_name, site_id) %>% 
  mutate(unique_years = n_distinct(year)) %>% 
  filter(unique_points > 4, unique_years > 2)

p7 %>% 
  # filter(fish_weight < 1500) %>% 
  ggplot(aes(x = pass_start_time, y = fish_weight, color = scientific_name)) + geom_point() +
  ylab("Fish mass") + xlab("Date") + facet_wrap( ~ site_id, scales = "free_y") +
  geom_smooth(method = "lm")
ggsave("figures/fish-weight-more-than2years-bysite.png", width = 30, height = 22)

p7 %>% 
  # filter(fish_weight < 1500) %>% 
  ggplot(aes(x = fish_total_length, y = fish_weight, color = site_id)) + geom_point() + facet_wrap( ~ scientific_name, scales = "free") 
ggsave("figures/fish-weight-length-more-than2years.png", width = 30, height = 22)
  
?loadByProduct  
  
p5 %>% 
  ggplot(aes(x = pass_start_time, y = mean_mass, color = site_id)) + geom_point() +
  ylab("Mean fish mass") + xlab("Date") + facet_wrap( ~ scientific_name, scales = "free_y") +
  geom_smooth(method = "lm")
ggsave("figures/mass_time-site-time-average-scales-free-more-than2years.png", width = 30, height = 22)


p5 %>% 
  ggplot(aes(x = pass_start_time, y = mean_body_condition, color = site_id)) + geom_point() +
  ylab("Mean body condition") + xlab("Date") + facet_wrap( ~ scientific_name, scales = "free_y") +
  geom_smooth(method = "lm")
ggsave("figures/body_condition_time-site-time-average-scales-free-more-than2years.png", width = 30, height = 22)


p3 %>% 
  ggplot(aes(x = pass_start_time, y = mean_length, color = site_id, group = site_id)) + geom_point() +
  geom_smooth(method = "lm") +
  ylab("Mean fish total length") + xlab("Date") + facet_wrap( ~ scientific_name, scales = "free_y")
ggsave("figures/length_time-site-time-average-scales-free-lm.png", width = 30, height = 22)


perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(year = year(pass_start_time)) %>%
  group_by(scientific_name, site_id, year) %>% 
  summarise(mean_length = mean(fish_total_length, na.rm = TRUE)) %>% 
  select(scientific_name, year, site_id) %>% 
  group_by(scientific_name, site_id) %>%
  tally() %>% View


perfish %>% 
  group_by(scientific_name, pass_start_time) %>% 
  tally() %>% View


unique(perfish$scientific_name)
length(unique(perfish$site_id))


### how about some oxygen data

## water temperature

water_temp <- loadByProduct(dpID="DP1.20264.001", 
                        site=c("BARC", "TOOK"),
                        startdate= NA, 
                        enddate=NA)


str(water_temp)


### this looks handy, to get the objects out of a list
list2env(water_temp, .GlobalEnv)


water_temp2 <- water_temp$TSD_30_min %>% 
  clean_names()

tsd_30 <- TSD_30_min %>% 
  clean_names()

tsd_30 %>% 
  # filter(thermistor_depth == "0.05") %>% 
  ggplot(aes(x = start_date_time, y = tsd_water_temp_mean, color = factor(thermistor_depth))) + geom_line() +
  facet_wrap( ~ site_id)
ggsave("figures/water-temperatures-barc-took.png", width = 12, height = 6)

write_csv(tsd_30, "data-processed/water-temps-30mins.csv")



# select sites ------------------------------------------------------------



p7 <- perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  mutate(body_condition = 100*(fish_weight/fish_total_length^3))
# 
# %>%
#   mutate(unique_points = n_distinct(fish_weight)) %>% 
#   mutate(year = year(pass_start_time)) %>% 
#   group_by(scientific_name, site_id) %>% 
#   mutate(unique_years = n_distinct(year)) %>% 
#   filter(unique_points > 4, unique_years > 2)

select_sites <- p7 %>% 
  filter(site_id %in% c("MCDI", "PRIN", "LIRO", "CRAM"))



select_sites %>% 
  ggplot(aes(x = fish_total_length, y = fish_weight, color = scientific_name)) + geom_point() +
  facet_wrap( ~ scientific_name, scales = "free") + theme(legend.position = "none")
ggsave("figures/length-weight.pdf", width = 12, height = 10)



select_sites %>% 
  mutate(outlier = case_when(site_id == "CRAM" & body_condition > 0.005 ~ "yes",
                             site_id == "LIRO" & body_condition > 0.005 ~ "yes",
                             site_id == "MCDI" & body_condition > 0.004 ~ "yes",
                             site_id == "PRIN" & body_condition > 0.05 ~ "yes",
                             .default = "no")) %>%
  filter(outlier == "no") %>% 
  ggplot(aes(x = pass_start_time, y = body_condition, color = scientific_name)) + geom_point() +
  ylab("Fish body condition") + xlab("Date") + facet_wrap( ~ site_id, scales = "free_y") +
  geom_smooth(method = "lm") + theme(legend.position = "none")
ggsave("figures/condition-select-sites-fish-time.png", width = 12, height = 10)

select_sites %>% 
  mutate(outlier = case_when(site_id == "CRAM" & body_condition > 0.005 ~ "yes",
                             site_id == "LIRO" & body_condition > 0.005 ~ "yes",
                             site_id == "MCDI" & body_condition > 0.004 ~ "yes",
                             site_id == "PRIN" & body_condition > 0.05 ~ "yes",
                             .default = "no")) %>%
  filter(outlier == "no") %>% 
  ggplot(aes(x = pass_start_time, y = fish_total_length, color = scientific_name)) + geom_point() +
  ylab("Fish length") + xlab("Date") + facet_wrap( ~ site_id, scales = "free_y") +
  geom_smooth(method = "lm") + theme(legend.position = "none")
ggsave("figures/length-select-sites-fish-time.png", width = 12, height = 10)


select_sites %>% 
  mutate(outlier = case_when(site_id == "CRAM" & body_condition > 0.005 ~ "yes",
                             site_id == "LIRO" & body_condition > 0.005 ~ "yes",
                             site_id == "MCDI" & body_condition > 0.004 ~ "yes",
                             site_id == "PRIN" & body_condition > 0.05 ~ "yes",
                             site_id == "MCDI" & fish_weight > 200 ~ "yes",
                             site_id == "PRIN" & fish_weight > 200 ~ "yes",
                             .default = "no")) %>%
  filter(outlier == "no") %>% 
  ggplot(aes(x = pass_start_time, y = fish_weight, color = scientific_name)) + geom_point() +
  ylab("Fish weight") + xlab("Date") + facet_wrap( ~ site_id, scales = "free_y") +
  geom_smooth(method = "lm") + theme(legend.position = "none")
ggsave("figures/weight-select-sites-fish-time.png", width = 12, height = 10)



sites_subset <- select_sites %>% 
  mutate(outlier = case_when(site_id == "CRAM" & body_condition > 0.005 ~ "yes",
                             site_id == "LIRO" & body_condition > 0.005 ~ "yes",
                             site_id == "MCDI" & body_condition > 0.004 ~ "yes",
                             site_id == "PRIN" & body_condition > 0.05 ~ "yes",
                             site_id == "MCDI" & fish_weight > 200 ~ "yes",
                             site_id == "PRIN" & fish_weight > 200 ~ "yes",
                             .default = "no")) %>%
  filter(outlier == "no") 




