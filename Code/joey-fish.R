


### read in the fish data

library(tidyverse)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())
library(lubridate)

neon_fish <- readRDS('2025 Raw Release Data/neonFish.Robj')


perfish <- neon_fish$fsh_perFish %>% 
  clean_names()

perpass <- neon_fish$fsh_perPass %>% 
  clean_names()


perpass %>% 
  ggplot(aes(x = pass_start_time, y = water_temp)) + geom_point() +
  facet_wrap(~ site_id)

perfish %>% 
  # filter(scientific_name == "Rhinichthys atratulus") %>% 
  filter(fish_life_stage == "adult") %>%
  ggplot(aes(x = pass_start_time, y = fish_total_length, color = site_id)) + geom_point() +
  ylab("Fish total length") + xlab("Date") + facet_wrap( ~ scientific_name)




# fish exploring ----------------------------------------------------------

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


