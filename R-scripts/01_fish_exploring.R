

#### NEON data exploring

library(neonUtilities)
library(janitor)
library(tidyverse)


### try to get the fish data

fishes <- loadByProduct(dpID="DP1.20107.001", 
                           site=c("GUIL"),
                           startdate="2016-05", 
                           enddate="2023-08")


perfish <- fishes$fsh_perFish %>% 
  clean_names()


perfish %>% 
  ggplot(aes(x = pass_start_time, y = fish_total_length, color = scientific_name)) + geom_point() +
  facet_wrap( ~ fish_life_stage)


unique(perfish$scientific_name)
