library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

neonZoops <- readRDS(paste0(datapath, 'neonZoops.Robj'))

names(neonZoops)

field<-neonZoops$zoo_fieldData
names(field)

samp<-neonZoops$zoo_perSample
names(samp)

tax<-neonZoops$zoo_taxonomyProcessed
names(tax)

# check for dupes

field <- field %>%
  distinct(across(-uid), .keep_all = TRUE)
samp <- samp %>% 
  distinct(across(-uid), .keep_all = TRUE)
tax <- tax %>% 
  distinct(across(-uid), .keep_all = TRUE)

# join field and samp data

field<-field[is.na(field$samplingImpractical),]

zoo <- full_join(field,samp,join_by("sampleID"=="sampleID"))
#check dupes
dupes<-zoo[which(zoo$uid.x %in% zoo$uid.x[duplicated(zoo$uid.x)]),]

library(dplyr)
library(purrr)
library(stringr)
library(tibble)

find_flagged_uidy <- function(df_group) {
  if (nrow(df_group) < 2) return(NULL)
  
  df_trimmed <- df_group %>% select(-uid.y)
  
  combs <- combn(nrow(df_trimmed), 2, simplify = FALSE)
  
  for (idx in combs) {
    i <- idx[1]
    j <- idx[2]
    
    row1 <- df_trimmed[i, ]
    row2 <- df_trimmed[j, ]
    
    diff_cols <- names(row1)[which(as.character(row1) != as.character(row2))]
    
    if (identical(diff_cols, "benchRemarks")) {
      br1 <- row1$benchRemarks
      br2 <- row2$benchRemarks
      
      if (str_detect(br1, "Percent Subbed measured by weight")) {
        return(tibble(uid.y = df_group$uid.y[i], keep = TRUE))
      } else if (str_detect(br2, "Percent Subbed measured by weight")) {
        return(tibble(uid.y = df_group$uid.y[j], keep = TRUE))
      }
    }
  }
  
  return(NULL)
}

uid_to_keep <- dupes %>%
  group_by(uid.x) %>%
  group_split() %>%
  map_dfr(find_flagged_uidy)

dupes_filtered <- dupes %>%
  mutate(uid.y = as.character(uid.y)) %>%
  left_join(uid_to_keep, by = "uid.y") %>%
  mutate(keep = if_else(is.na(keep), TRUE, keep)) %>%  # keep all rows not in the "benchRemarks only" group
  filter(keep) %>%
  select(-keep)

# keep only filtered dupes

zoo<-zoo[-which(zoo$uid.y %in% dupes$uid.y),]
zoo<-rbind(zoo,dupes_filtered)

# other dupes seem to be mostly fine vs coarse distinction

# now join to tax 

zoo <- full_join(zoo,tax,join_by("sampleID"=="sampleID"))

zoo <- zoo %>% select(siteID,
                      namedLocation,
                      collectDate,
                      eventID,
                      sampleID,
                      samplerType,
                      towsTrapsVolume,
                      taxonID,
                      nauplii,
                      zooMinimumLength,
                      zooMaximumLength,
                      zooWidth,
                      adjCountPerBottle)

zoo$aquaticSiteType<-'lake'

# adjusted counts per tow volume
zoo$countPerL<-zoo$adjCountPerBottle/zoo$towsTrapsVolume


zoo %>% group_by(eventID) %>% summarise(samps=length(unique(sampleID)))
#typically 3 samps per event, i think it's ok to ignore the few doubles indicate they should be treated as a composite sample

write.csv(zoo,paste0(resultspath,"zooplankton.csv"),row.names=F)

sumry <- zoo %>% group_by(siteID,collectDate) %>% summarise(count=mean(countPerL,na.rm=T))

sumry %>%
  ggplot(aes(x = collectDate, y = count, 
             color = siteID,
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("count per L") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID, scales = "free_y")




