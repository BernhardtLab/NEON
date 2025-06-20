



# all other data import ---------------------------------------------------



zoo <- read_csv("Clean Data/zooplankton.csv")
plantcover <- read_csv("Clean Data/plantCov.csv")

plantCH <- read_csv("Clean Data/plantCH_afdm.csv")

mic_alg <- read_csv("Clean Data/micAlg_afdm.csv")

macroinverts <- read_csv("Clean Data/MacroinvertebrateTaxa.csv")
inverts <- read_csv("Clean Data/invertebrates.csv")
inverts2 <- read_csv("Clean Data/cleanLengthMassRelationships_inverts.csv")




# plot zooplankton --------------------------------------------------------

zoop_sum <- zoo %>%
  group_by(siteID,collectDate) %>%
  summarise(count=mean(countPerL,na.rm=T))

zoop_sum %>%
  ggplot(aes(x = collectDate, y = count, 
             color = siteID,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("count per L") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID, scales = "free_y")

zoop_sum %>%
  ggplot(aes(x = collectDate, y = count, 
             color = siteID,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("count per L") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID, scales = "free_y")



# plot invertebrates ------------------------------------------------------

sumry <- inverts %>%
  group_by(aquaticSiteType,siteID,habitatType,eventID) %>%
  summarise(massPerArea=sum(totMass,na.rm=T)/sum(benthicArea,na.rm=T),indsPerArea=sum(estimatedTotalCount)/sum(benthicArea,na.rm=T))



sumry <- sumry %>%
  mutate(year = str_sub(eventID, 6, 9))

sumry %>%
  ggplot(aes(x = year, y = massPerArea, 
             color = habitatType, 
             group = habitatType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total biomass per area") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")

sumry %>%
  ggplot(aes(x = year, y = indsPerArea, 
             color = habitatType, 
             group = habitatType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total individuals per area") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")

mic_alg <- read_csv("Clean Data/micAlg_afdm.csv")

mic_alg %>% 
  select(siteID.x)
