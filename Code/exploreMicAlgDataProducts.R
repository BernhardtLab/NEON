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

neonMicCol <- readRDS(paste0(datapath, 'neonMicCol.Robj'))
neonMicChem <- readRDS(paste0(datapath, 'neonMicChem.Robj'))

names(neonMicCol)
vars<-neonMicCol$variables_20166
field<-neonMicCol$alg_fieldData
bm<-neonMicCol$alg_biomass
tax<-neonMicCol$alg_taxonomyProcessed

# subset  data to relevant records

field<-field[is.na(field$samplingImpractical),]
field<-field[which(field$aquaticSiteType!='river'),]
bm<-bm[which(bm$siteID %in% field$siteID),]
tax<-tax[which(tax$siteID %in% tax$siteID),]

# check for duplicates
field <- field %>%
  distinct(across(-uid), .keep_all = TRUE)
bm <- bm %>% 
  distinct(across(-uid), .keep_all = TRUE) #  seemingly true duplicates
tax_dupes <- tax %>%
  group_by(across(-uid)) %>%
  filter(n() > 1) %>%
  arrange(across(everything())) # seemingly true duplicate
tax <- tax %>% 
  distinct(across(-uid), .keep_all = TRUE)

# join biomass and field

mic<-full_join(field,bm,join_by('parentSampleID'))

# investigate mismatches
mic<-mic[which(mic$sampleCollected=="Y"),] # some are because a sample was not collected
mic<-mic[!is.na(mic$uid.y),] # seems others were never directed to the mass/taxonomic part of the analysis, perhaps damaged sample, etc.

# first look into mass
afdm<-mic[which(mic$analysisType=="AFDM"),]

# remove filter missing samples
afdm<-afdm[which(afdm$sampleCondition!="filter missing"),]

afdm<- afdm %>% select(aquaticSiteType,
                       siteID.x,
                       namedLocation.x,
                       collectDate.x,
                       eventID,
                       parentSampleID,
                       sampleID,
                       habitatType,
                       algalSampleType,
                       samplerType,
                       benthicArea,
                       algalSampleType,
                       substratumSizeClass,
                       plantSurfaceArea,
                       fieldSampleVolume,
                       labSampleVolume,
                       preservativeVolume,
                       domainFilterVolume,
                       benthicArea,
                       ashMassDataQF,
                       adjAshFreeDryMass
                       )

# switch below detection to 0 mass, remove those with errors
afdm$adjAshFreeDryMass[which(afdm$ashMassDataQF=="belowDetection")]<-0
afdm<-afdm[-which(afdm$ashMassDataQF=="ashMassError"),]

# write clean data

write.csv(afdm,paste0(resultspath,'micAlg_afdm.csv'),row.names=F)

afdm$algalSampleType[which(afdm$algalSampleType=='epilithon_largeSubstrate')]<-'epilithon'

afdm$afdmPerArea<-afdm$adjAshFreeDryMass/afdm$domainFilterVolume
afdm$afdmPerArea[-which(afdm$algalSampleType %in% c('phytoplankton','seston'))]<-afdm$afdmPerArea[-which(afdm$algalSampleType %in% c('phytoplankton','seston'))]*afdm$fieldSampleVolume[-which(afdm$algalSampleType %in% c('phytoplankton','seston'))]/afdm$benthicArea[-which(afdm$algalSampleType %in% c('phytoplankton','seston'))]

sumry <- afdm %>% group_by(siteID.x,collectDate.x,algalSampleType,habitatType) %>% summarise(samps=length(sampleID),meanafdm=mean(afdmPerArea,na.rm=T))


sumry %>%
  ggplot(aes(x = collectDate.x, y = meanafdm, 
             color = algalSampleType,
             group = algalSampleType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("mean afdm per volume or area") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID.x, scales = "free_y")


## individual counts

inds<-full_join(field,bm,join_by('parentSampleID'))
inds<-inds[!is.na(inds$uid.y),]
inds<-inds[which(inds$analysisType=="taxonomy"),]

tax<-tax[which(tax$siteID %in% field$siteID),]
inds<-full_join(inds,tax,join_by('sampleID'))

# missing sample?
miss<-inds[is.na(inds$uid),]
miss<-miss[-grep("discard",miss$remarks.y),]
miss<-miss[-grep('froze',miss$remarks.y),]
miss<-miss[-which(miss$remarks.y=='taxonomy sample not created due to misunderstanding of protocol'),]
# plus no post 2020 taxonomy samples, ok to discard all with missing taxonomy records

inds<-inds[!is.na(inds$uid),]

#use chemically preserved samples for quantitative density calculations

inds<-inds[which(inds$algalParameterUnit=="cellsPerBottle"),]
inds$algalParameterValue[which(inds$targetTaxaPresent=="N")]<-0

inds$countPermL<-inds$algalParameterValue/(inds$labSampleVolume+inds$preservativeVolume)
inds$countPermL[which(inds$algalSampleType!='phytoplankton')]<-
  inds$countPermL[which(inds$algalSampleType!='phytoplankton')]*
  inds$fieldSampleVolume[which(inds$algalSampleType!='phytoplankton')]/inds$benthicArea[which(inds$algalSampleType!='phytoplankton')]

inds$algalSampleType[which(inds$algalSampleType=="epilithon_largeSubstrate")]<-"epilithon"
sumry <- inds %>% group_by(siteID.x,collectDate.x,algalSampleType,habitatType) %>% summarise(samps=length(sampleID),count=sum(countPermL,na.rm=T))

sumry %>%
  ggplot(aes(x = collectDate.x, y = count, 
             color = algalSampleType,
             group = algalSampleType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_blank(aes(y = 0)) +
  ylab("count per mL") + 
  xlab("collection Date") +
  facet_wrap( ~ siteID.x, scales = "free_y")

# chemistry



