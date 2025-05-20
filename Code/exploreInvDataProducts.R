library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)

datapath<-'/Users/kelsey/Github/RCN-KY/2025 Raw Release Data/'
resultspath<-'/Users/kelsey/Github/RCN-KY/Clean Data/'

# load data

neonInv <- readRDS(paste0(datapath, 'neonInv.Robj'))
lmRel<-read.csv(paste0(resultspath,'cleanLengthMassRelationships_inverts.csv'))
taxa<-read.csv(paste0(resultspath,'MacroinvertebrateTaxa.csv'))

names(neonInv)

field<-neonInv$inv_fieldData
samp<-neonInv$inv_persample
tax<-neonInv$inv_taxonomyProcessed
names(field)
names(samp)
names(tax)

field<-field[is.na(field$samplingImpractical),]

# remove apparent duplicates

field <- field %>%
  distinct(across(-uid), .keep_all = TRUE)

samp <- samp %>% 
  distinct(across(-uid), .keep_all = TRUE)

tax <- tax %>%
  distinct(across(-uid), .keep_all = TRUE)

inv<-full_join(field,samp,join_by("sampleID"=="sampleID"))


dupes_field<-inv[which(inv$uid.x %in% inv$uid.x[duplicated(inv$uid.x)]),]
#seem to be true duplicates, basically only differ in protocol and sort date, safe to remove
inv<-inv[!duplicated(inv$uid.x),]

dupes_samp<-inv[which(inv$uid.y %in% inv$uid.y[duplicated(inv$uid.y)]),]
#all are just missing records for the sample having been sent to the lab entirely, treat as if sampling did not happen
inv<-inv[!duplicated(inv$uid.y),]

inv_full<-full_join(inv,tax,join_by("sampleID"=="sampleID"))

dupes_tax<-inv_full[which(inv_full$uid %in% inv_full$uid[duplicated(inv_full$uid)]),]
# no dupes here


inv<-inv_full %>% select(siteID.x,
                 namedLocation.x,
                 aquaticSiteType,
                 eventID,
                 sampleID,
                 habitatType,
                 samplerType,
                 benthicArea,
                 substratumSizeClass,
                 benthicArea,
                 ponarDepth,
                 snagLength,
                 snagDiameter,
                 targetTaxaPresent,
                 acceptedTaxonID,
                 invertebrateLifeStage,
                 sizeClass,
                 estimatedTotalCount)

# 4108 duplicates here, explore them now

dupes <- inv %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))
HOPB.20161011.SURBER.5<-inv_full[which(inv_full$sampleID=='HOPB.20161011.SURBER.5'),]
# looks like duplication happens when the size "category" differs typically due to damage to the specimen... ok to ignore

# drop couple of samples that do not match to field data
inv<-inv[!is.na(inv$siteID.x),]

#rename columns
names(inv)<-str_replace(names(inv),"[.]x","")

# find biomass for each inv record
# first change the neon taxonomy to long form
taxa$superorder[which(taxa$order %in% c("Actinedida","Trombidiformes"))]<-"Acariformes"
neontaxa <- taxa %>%
  pivot_longer(
    cols = kingdom:subspecies,  # All taxonomic ranks
    names_to = "rank",
    values_to = "name"
  ) %>%
  select(acceptedTaxonID,taxonID, rank, name)

neontaxa<-neontaxa[!is.na(neontaxa$name),]
neontaxa<-neontaxa[-which(neontaxa$name==""),]
neontaxa<-neontaxa[nrow(neontaxa):1, ]


#which taxa are found
taxaFound<-data.frame(taxa=unique(inv$acceptedTaxonID),a=NA,b=NA)

for (i in 1:nrow(taxaFound)){
    print(i)
    ntax <- neontaxa[which(neontaxa$acceptedTaxonID == taxaFound$taxa[i]), ]
    rels<-lmRel[which(lmRel$Taxon %in% ntax$name),]
    if (nrow(rels>=1)){
      taxaFound$a[i]<-rels$a[1]
      taxaFound$b[i]<-rels$b[1]
    }
    else {
      ntax <- neontaxa[which(neontaxa$taxonID == taxaFound$taxa[i]), ]
      rels<-lmRel[which(lmRel$Taxon %in% ntax$name),]
      if (nrow(rels>=1)){
        taxaFound$a[i]<-rels$a[1]
        taxaFound$b[i]<-rels$b[1]
      }
      else {
        ntax <- neontaxa[which(neontaxa$acceptedTaxonID == taxaFound$taxa[i]), ]
        rels<-lmRel[which(lmRel$UpperTaxon %in% ntax$name),]
        if (nrow(rels>=1)){
          taxaFound$a[i]<-rels$a[1]
          taxaFound$b[i]<-rels$b[1]
        }
      }
    }
}

notFound<-taxa[which(taxa$acceptedTaxonID %in% taxaFound$taxa[is.na(taxaFound$a)]),]

# leaving out Bryozoa, cnidaria, roundworms, flatworms (14 total taxa)

# calculate the biomass
# add  mass based on size class relationships
inv$mass<-NA

for (i in 1:nrow(inv)){
  print(i)
  taxon<-taxaFound[which(taxaFound$taxa==inv$acceptedTaxonID[i]),]
  if(nrow(taxon)==1){
    inv$mass[i]<-taxon$a*inv$sizeClass[i]^taxon$b
  }
}

inv$totMass<-inv$estimatedTotalCount*inv$mass

#remove rivers
inv<-inv[which(inv$aquaticSiteType!="river"),]

# if no target taxa found put mass and indivs at 0

inv$totMass[which(inv$targetTaxaPresent!="Y")]<-0
inv$estimatedTotalCount[which(inv$targetTaxaPresent!="Y")]<-0


# Summarize

sumry <- inv %>% group_by(aquaticSiteType,siteID,habitatType,eventID) %>% summarise(massPerArea=sum(totMass,na.rm=T)/sum(benthicArea,na.rm=T),indsPerArea=sum(estimatedTotalCount)/sum(benthicArea,na.rm=T))
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

write.csv(inv,paste0(resultspath,"invertebrates.csv"),row.names=FALSE)
