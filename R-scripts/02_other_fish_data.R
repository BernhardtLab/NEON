

### joining the NEON data with CTmax data from Comte and Olden
library(readxl)
library(rfishbase)
library(tidyverse)
library(janitor)
perfish <- read_csv("data-processed/perfish_data.csv")


perfish_select <- perfish %>% 
  filter(site_id %in% c("LIRO", "MCDI", "PRIN", "CRAM"))

neon_fish <- unique(perfish_select$scientific_name)


comte <- read_excel("/Volumes/Extreme SSD/NEON-data/Comte_Olden_CTmax_Data.xlsx", sheet = 2) %>% 
  clean_names()



comte_neon <- comte %>% 
  filter(species %in% c(neon_fish))

write_csv(comte_neon, "data-processed/comte-neon-select.csv")

comte_neon <- read_csv("data-processed/comte-neon-select.csv")

length(unique(comte_neon$species))

comte_neon %>% 
  ggplot(aes(x = thermal_limit_c, fill = species)) + geom_histogram() +
  facet_wrap( ~ species, scales = "free_y")
  ggsave("figures/comte-neon-ctmax.pdf", width = 20, height = 10)


all_fish_select <- full_join(perfish_select, comte_neon, by = c("scientific_name" = "species"))



load("/Volumes/Extreme SSD/NEON-data/Organismal-data/MicroAlgae_Collection_NeonData.Robj", verbose = TRUE)
phytoplankton<-NeonData
load("/Volumes/Extreme SSD/NEON-data/Organismal-data/Zooplankton_NeonData.Robj", verbose = TRUE)
zooplankton <- NeonData


list2env(phytoplankton, envir = .GlobalEnv)
list2env(zooplankton, envir = .GlobalEnv)

View(alg_biomass)

a2 <- alg_biomass %>% 
  clean_names()


library(cowplot)
theme_set(theme_cowplot())

a2 %>% 
  filter(site_id %in% c("CRAM", "LIRO", "MCDI", "PRIN")) %>% 
  ggplot(aes(x = processed_date, y = adj_ash_free_dry_mass)) + geom_point() + 
  facet_wrap( ~ site_id, scales = "free_y") + ylab("Algae biomass") + xlab("Date") +
  geom_smooth(method = "lm")
ggsave("figures/algae-time.pdf", width = 8, height = 6)


z2 <- zoo_taxonomyProcessed %>% 
  clean_names()


z2 %>% 
  filter(site_id %in% c("CRAM")) %>% 
  ggplot(aes(x = collect_date, y = zoo_maximum_length, color = scientific_name)) + geom_point() +
  facet_wrap(~ scientific_name, scales = "free_y") + ylab("Zooplankton length") + xlab("Date") +
  geom_smooth(method = "lm") + theme(legend.position = "none")
ggsave("figures/CRAM-zooplankton-length-time-facet.pdf", width = 20, height = 20)

z2 %>% 
  filter(site_id %in% c("LIRO")) %>% 
  ggplot(aes(x = collect_date, y = zoo_maximum_length, color = scientific_name)) + geom_point() +
  facet_wrap(~ scientific_name, scales = "free_y") + ylab("Zooplankton length") + xlab("Date") +
  geom_smooth(method = "lm") + theme(legend.position = "none")
ggsave("figures/LIRO-zooplankton-length-time-facet.pdf", width = 20, height = 20)


alg_subset <- a2 %>% 
  filter(site_id %in% c("CRAM", "LIRO", "MCDI", "PRIN")) 

write_csv(alg_subset, "data-processed/algae-biomass.csv")


str(zooplankton)


library(rfishbase)
rfishbase::fb_tables()

thing <- getQuantTraits("Salmo trutta")

fb_tbl()



fish <- neon_fish

thing <- fb_tbl("species") %>% 
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% fish) 


write_csv(thing, "data-processed/fishbase-species-table.csv")

maturity <- fb_tbl("maturity") 


maturity2 <- left_join(thing, maturity, by = c("SpecCode" = "Speccode"))
write_csv(maturity2, "data-processed/fish-base-maturity.csv")

length(unique(maturity2$sci_name))
maturity2$AgeMatMin


### analysis

### fish quality control
### look at cook regression distance -- from log log plot


## phylogenetic mixed effects model
## site as a random effect
## year as a random effect

## size of the fish over time
## condition over time

### do we have fish abundance?
### FSA package, use this to get estimated abundance value 

### look at as a meta-analysis


                       