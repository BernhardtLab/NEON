library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

#neonPlantPC <- readRDS(paste0(datapath, 'neonPlantPC.Robj'))
neonPlantCH <- readRDS(paste0(datapath, 'neonPlantCH.Robj'))
neonPlantChem <- readRDS(paste0(datapath, 'neonPlantChem.Robj'))
neonPlantCov <- readRDS(paste0(datapath, 'neonPlantCov.Robj'))

#### Plant PC
#names(neonPlantPC)
#pt<-neonPlantPC$apc_pointTransect
#pt<-neonPlantPC$apc_perTaxon
# Upon further review, I think this one is not super relevant to our needs

#### Plant CH = Ash Free Dry Mass of aquatic plants
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
              chemSubsampleID,
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


plotsum %>%
  ggplot(aes(x = year, y = biomass, 
             color = habitatType, 
             group = interaction(siteID, habitatType))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("total adjusted ash free biomass per sample") + 
  xlab("year") +
  facet_wrap( ~ siteID, scales = "free_y")


# Save clean data
write.csv(ch,paste0(resultspath,'plantCH_afdm.csv'),row.names=FALSE)



##### Aquatic Plant chemical properties

names(neonPlantChem$apl_plantExternalLabDataPerSample)
chem<-full_join(ch,neonPlantChem$apl_plantExternalLabDataPerSample,join_by("chemSubsampleID"=="sampleID"))

#address replicates

drop_replicates<-c()

for (i in 1:nrow(chem)){
  if(is.na(chem$replicate[i])==FALSE){
    if(chem$replicate[i]=="2"){
      reps<-chem[which(chem$chemSubsampleID==chem$chemSubsampleID[i]
                       & chem$analyte==chem$analyte[i]),]
      meanConc<-mean(reps$analyteConcentration)
      chem$analyteConcentration[which(chem$chemSubsampleID==chem$chemSubsampleID[i] 
                                      & chem$analyte==chem$analyte[i] 
                                      & chem$replicate=="1")]<-meanConc
      drop_replicates<-c(drop_replicates,i)
    }
  }
}

chem<-chem[-drop_replicates,]

#analyses done
unique(chem$analyte)

sumry<-chem %>% 
  group_by(aquaticSiteType,siteID.x,year,samplingImpractical,
                       habitatType,samplerType,type,targetTaxaPresent,sampleCollected)  %>% 
  summarise(samples=length(unique(locationID)),
                totalAdjBiomass=sum(as.numeric(biomass),na.rm=T),
                d13C=mean(analyteConcentration[which(analyte=="d13C")],na.rm=T),
            C=mean(analyteConcentration[which(analyte=="carbon")],na.rm=T),
            d15N=mean(analyteConcentration[which(analyte=="d15N")],na.rm=T),
            N=mean(analyteConcentration[which(analyte=="nitrogen")],na.rm=T))

plotsum<-sumry %>% group_by(siteID.x,year,habitatType) %>% summarise(CNratio=mean(C/N,na.rm=T))


plotsum %>%
  ggplot(aes(x = year, y = CNratio, 
             color = habitatType, 
             group = interaction(siteID.x, habitatType))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("mean carbon nitrogen ratio per sample") + 
  xlab("year") +
  facet_wrap( ~ siteID.x, scales = "free_y")

# Not sure what is most interesting data here and lots of data is missing... 

#### Riparian plant cover

names(neonPlantCov)

cov<-neonPlantCov$rip_percentComposition
names(cov)

cov<-cov[is.na(cov$samplingImpractical),]

# deal with replicates
cov<-cov %>% group_by(siteID,startDate,namedLocation,measurementLocation,measurementDirection) %>% summarise(percCov=mean(canopyCoverPercent))
cov$year<-year(cov$startDate)
cov$month<-month(cov$startDate)

# summarise down to the relevant level

cov <-cov %>% group_by(siteID,startDate,year,month,namedLocation,measurementLocation) %>% summarise(percCov=mean(percCov))
cov <-cov %>% group_by(siteID,startDate,year,month,namedLocation) %>% summarise(percCov=mean(percCov))
cov <-cov %>% group_by(siteID,startDate,year,month) %>% summarise(percCov=mean(percCov))
cov <-cov %>% group_by(siteID,year,month) %>% summarise(percCov=mean(percCov))
cov$survey<-paste(cov$year,cov$month,sep="-")
cov$survey<-ym(cov$survey)


sitetypes<-ch %>%
  distinct(siteID, aquaticSiteType)

cov<-left_join(cov,sitetypes,join_by("siteID"=="siteID"))
cov<-cov[!is.na(cov$aquaticSiteType),]

cov %>%
  ggplot(aes(x = survey, y = percCov, 
             color = siteID, 
             group = siteID)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("mean percent cover per transect") + 
  xlab("survey") +
  facet_wrap( ~ siteID, scales = "free_y")

write.csv(cov,paste0(resultspath,"plantCov.csv"),row.names=FALSE)
