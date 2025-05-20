library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)

datapath<-'/Users/kelsey/Github/RCN/2025 Raw Release Data/'

# load data

neonPlantPC <- readRDS(paste0(datapath, 'neonPlantPC.Robj'))
#neonPlantCH <- readRDS(paste0(datapath, 'neonPlantCH.Robj'))
neonPlantChem <- readRDS(paste0(datapath, 'neonPlantChem.Robj'))

#### Plant PC
#names(neonPlantPC)
#pt<-neonPlantPC$apc_pointTransect
#pt<-neonPlantPC$apc_perTaxon
# Upon further review, I think this one is not super relevant to our needs

#### Plant CH
names(neonPlantCH)
ch.var<-data.frame(neonPlantCH$variables_20066)
ch<-neonPlantCH$apl_clipHarvest

# join to biomass to connect field and lab data
ch<-full_join(ch,neonPlantCH$apl_biomass,join_by("fieldID"=="fieldID"))

# subset to bout 2 to exclude the presence/absence only data, add year
ch<-ch[which(ch$boutNumber==2),]
ch<-ch[which(ch$aquaticSiteType!="river"),]
ch$year<-year(ch$collectDate.x)

# total arealAdjAshFreeDryMass  in biomass table per site x year adjusted by sampling effort (unique quadrat for streams, point for lake) seems most relevant

# pare down data frame to potentially important variables

ch<- ch %>% select(aquaticSiteType,
              siteID.x,
              locationID,
              year,
              samplingImpractical,
              habitatType,
              samplerType,
              targetTaxaPresent,
              sampleCollected,
              benthicArea.x,
              sampleDepth,
              growthForm,
              taxonID,
              class,
              arealAdjDryMass,
              arealAdjAshFreeDryMass,
              chemSubsampleID)

names(ch)<-str_replace(names(ch),"[.]x","")

# mass for analysis
summary(ch$arealAdjAshFreeDryMass/ch$arealAdjDryMass)
# for instances in which afdm was not calculated adjust the dry mass by the median value
adjFactor<-median(ch$arealAdjAshFreeDryMass/ch$arealAdjDryMass,na.rm=T)
ch$biomass<-ch$arealAdjAshFreeDryMass
ch$biomass[which(is.na(ch$arealAdjAshFreeDryMass) & ch$arealAdjDryMass>0)]<-ch$arealAdjDryMass[which(is.na(ch$arealAdjAshFreeDryMass) & ch$arealAdjDryMass>0)]*adjFactor

summary(ch$biomass)

#distinguish plant vs algae

ch$type<-NA
ch$type[-is.na(ch$class)]<-"plant"
ch$type[which(ch$class=="Charophyceae")]<-"green algae"

# populate lakes with littoral
ch$habitatType[which(ch$aquaticSiteType=="lake")]<-"littoral"

# start to summarise the data

sumry<-ch %>% group_by(aquaticSiteType,siteID,year,samplingImpractical,habitatType,samplerType,type,targetTaxaPresent,sampleCollected) %>% summarise(samples=length(unique(locationID)),totalAdjBiomass=sum(as.numeric(biomass),na.rm=T))

sumry %>% filter(targetTaxaPresent=="Y",sampleCollected=="Y",totalAdjBiomass==0) # not many anomolous samples, some zeros seem to be real in the data

# remove sampling impractical values

sumry<-sumry[is.na(sumry$samplingImpractical),]

hist(sumry$totalAdjBiomass)

# plot total by year by site by habitat

plotsum<-sumry %>% group_by(siteID,year,habitatType) %>% summarise(biomass=sum(totalAdjBiomass,na.rm=T))


sumry %>%
  ggplot(aes(x = year, y = totalAdjBiomass, 
             color = habitatType, 
             group = interaction(siteID, habitatType))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total adjusted ash free biomass per sample") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")




