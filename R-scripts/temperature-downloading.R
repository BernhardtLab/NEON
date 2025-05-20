


for (i in 1:length(aquaticsitesNEON)){
  baz<- loadByProduct(dpID="DP1.20053.001",
                      site=c(aquaticsitesNEON[i]),
                      startdate= NA,
                      enddate=NA,
                      check.size = F,
                      token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20iLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4NzM0OTMwMTUsImlhdCI6MTcxNTgxMzAxNSwiZW1haWwiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20ifQ.ieH_YfFI2CvqBfAkeGWiEfeA24xCqin6tjxVidfqNomI_Hj9MdWCgYHi9jFXA27GjjpMNtvjvmxMBExBeyoESw")
  name<- paste0(aquaticsitesNEON[i], "streamtemp")
  
  
  
  aquaticsitesNEON<- c("ARIK", "BARC", "BLDE", "CARI", "COMO", "CRAM", "CUPE", "HOPB", "KING", "LIRO", "MART", "MAYF", "MCDI", "OKSR", "POSE", "PRIN", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "WALK")
  
  
  streamsites <- c("ARIK", "BIGC", "BLDE", "CARI", "COMO", "CUPE", "GUIL", "HOPB", "KING", "LECO", "LEWI", "MART", "MAYF", "MCDI", "MCRA", "OKSR", "POSE", "PRIN", "REDB", "SYCA", "TECR", "WALK", "WLOU")
  
  download_temps_function <- function(site){
    output <- loadByProduct(dpID="DP1.20053.001",
                            site= site$aquaticsitesNEON[1],
                            startdate= NA,
                            enddate=NA,
                            tabl = "TSW_30min",
                            check.size = F,
                            token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20iLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4NzM0OTMwMTUsImlhdCI6MTcxNTgxMzAxNSwiZW1haWwiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20ifQ.ieH_YfFI2CvqBfAkeGWiEfeA24xCqin6tjxVidfqNomI_Hj9MdWCgYHi9jFXA27GjjpMNtvjvmxMBExBeyoESw")
 
    return(output$TSW_30min) 
    }

  sites <- data.frame(aquaticsitesNEON)
  
  
  sites_split <- sites %>% 
    filter(aquaticsitesNEON %in% c("MCDI", "PRIN")) %>%
    split(.$aquaticsitesNEON)
  
  temps_streams <- sites_split %>% 
    map_df(download_temps_function)
  
  write_csv(temps_streams, "/Volumes/Extreme SSD/NEON-data/stream-temps.csv")

# rivers and lakes --------------------------------------------------------

  download_temps_function_rivers_lakes <- function(site){
    output <- loadByProduct(dpID="DP1.20264.001",
                            site= site$aquaticsitesNEON[1],
                            startdate= NA,
                            enddate=NA,
                            tabl = "TSD_30_min",
                            check.size = F,
                            token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20iLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4NzM0OTMwMTUsImlhdCI6MTcxNTgxMzAxNSwiZW1haWwiOiJjaWNjaGlub2FtYW5kYUBnbWFpbC5jb20ifQ.ieH_YfFI2CvqBfAkeGWiEfeA24xCqin6tjxVidfqNomI_Hj9MdWCgYHi9jFXA27GjjpMNtvjvmxMBExBeyoESw")
    
    return(output$TSD_30_min) 
  }
  
  sites <- data.frame(aquaticsitesNEON)
  
  
  sites_split <- sites %>% 
    filter(aquaticsitesNEON %in% c("LIRO", "CRAM")) %>%
    split(.$aquaticsitesNEON)
  
  temps_lakes <- sites_split %>% 
    map_df(download_temps_function_rivers_lakes)  
  
  write_csv(temps_lakes, "/Volumes/Extreme SSD/NEON-data/lake-river-temps.csv")

  
    
temps_lakes <- read_csv("/Volumes/Extreme SSD/NEON-data/lake-river-temps.csv")
temps_lakes2 <- temps_lakes %>% 
  clean_names()

str(temps_lakes2)


temps_lakes2 %>% 
  filter(thermistor_depth == 0.05) %>% 
  ggplot(aes(x = start_date_time, y = tsd_water_temp_mean, color = site_id)) + geom_path() +
  facet_wrap( ~ site_id)

temps_lakes2 %>% 
  filter(thermistor_depth == 0.05) %>% 
  ggplot(aes(x = start_date_time, y = tsd_water_temp_maximum, color = site_id)) + geom_path() +
  facet_wrap( ~ site_id)
ggsave("figures/CRAM-LIRO-maximum-temps.pdf", width = 8, height = 6)

temps_streams <- temps_streams %>% 
  clean_names()

unique(temps_streams$site_id)


temps_streams <- read_csv("/Volumes/Extreme SSD/NEON-data/stream-temps.csv") %>%
  clean_names()

str(temps_streams)

temps_streams %>% 
  filter(surf_water_temp_mean < 60) %>% 
  filter(surf_water_temp_mean > 0) %>% 
  filter(surf_water_temp_maximum < 60) %>% 
  ggplot(aes(x = start_date_time, y = surf_water_temp_maximum, color = site_id)) + geom_line() +
  facet_wrap( ~ site_id, scales = "free_y")
ggsave("figures/MCDI-PRIN-maximum-temps.pdf", width = 8, height = 6)


perfish %>% 
  filter(site_id %in% c("LIRO", "CRAM")) %>% 
  ggplot(aes(x = pass_start_time, y = fish_weight, color = scientific_name)) + geom_point() +
  facet_wrap( ~ site_id, scales = "free_y") + geom_smooth(method = "lm")

perfish %>% 
  filter(site_id %in% c("MCDI", "PRIN")) %>% View
  ggplot(aes(x = pass_start_time, y = fish_total_length, color = scientific_name) + geom_point() +
  facet_wrap( ~ site_id, scales = "free_y") + geom_smooth(method = "lm") +
  theme(legend.position = "none")
  

  
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
  
  library(broom)
  
  results <- sites_subset %>% 
    group_by(site_id, scientific_name) %>% 
    do(tidy(lm(body_condition ~ pass_start_time, data = .), conf.int = TRUE))

  res2 <- results %>% 
    filter(term == "pass_start_time")
  
  res2 %>% 
    ggplot(aes(x = scientific_name, y = estimate)) + geom_point(color = "red") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "red") + geom_hline(yintercept = 0) + facet_wrap( ~ site_id)
  ggsave("figures/slopes.png", width = 10, height = 12)
  
  
  ?geom_pointrange()
  
  results %>% 
    filter
    ggplot(aes(x = scientific_name, y = estimate)) + geom_point() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  