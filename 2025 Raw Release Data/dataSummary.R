library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)



datapath<-'/Users/kelsey/Github/RCN/2025 Raw Release Data/'

# load data
neonFish <- readRDS(paste0(datapath, 'neonFish.Robj'))
neonZoops <- readRDS(paste0(datapath, 'neonZoops.Robj'))
neonMicCol <- readRDS(paste0(datapath, 'neonMicCol.Robj'))
neonMicChem <- readRDS(paste0(datapath, 'neonMicChem.Robj'))
neonInv <- readRDS(paste0(datapath, 'neonInv.Robj'))
#neonPlantPC <- readRDS(paste0(datapath, 'neonPlantPC.Robj'))
neonPlantCH <- readRDS(paste0(datapath, 'neonPlantCH.Robj'))
neonPlantChem <- readRDS(paste0(datapath, 'neonPlantChem.Robj'))
neonPlantCov <- readRDS(paste0(datapath, 'neonPlantCov.Robj'))


########### fish ###########
names(neonFish)
fieldFish<-neonFish$fsh_fieldData
fieldFish$year<-year(fieldFish$startDate)
fieldFish$month<-month(fieldFish$startDate)

# remove 'sampling impractical
fieldFish<-fieldFish[is.na(fieldFish$samplingImpractical),]
Fishsummary<-data.frame(fieldFish %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(Fishsummary)[5]<-"Fish"

########### Zoops ###########

names(neonZoops)
fieldZoops<-neonZoops$zoo_fieldData
fieldZoops$year<-year(fieldZoops$startDate)
fieldZoops$month<-month(fieldZoops$startDate)

# remove 'sampling impractical
fieldZoops<-fieldZoops[is.na(fieldZoops$samplingImpractical),]
Zoopssummary<-data.frame(fieldZoops %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(Zoopssummary)[5]<-"Zoop"


########### Microalgae collection ###########

names(neonMicCol)
fieldMicCol<-neonMicCol$alg_fieldData
fieldMicCol$year<-year(fieldMicCol$collectDate)
fieldMicCol$month<-month(fieldMicCol$collectDate)

# remove 'sampling impractical' and rivers
fieldMicCol<-fieldMicCol[is.na(fieldMicCol$samplingImpractical),]
fieldMicCol<-fieldMicCol[which(fieldMicCol$aquaticSiteType!="river"),]
MicColsummary<-data.frame(fieldMicCol %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(MicColsummary)[5]<-"MicCol"

########### Microalgae chemistry ###########

names(neonMicChem)
fieldMicChem<-neonMicChem$alg_fieldData
fieldMicChem$year<-year(fieldMicChem$collectDate)
fieldMicChem$month<-month(fieldMicChem$collectDate)

# remove 'sampling impractical' and rivers
fieldMicChem<-fieldMicChem[is.na(fieldMicChem$samplingImpractical),]
fieldMicChem<-fieldMicChem[which(fieldMicChem$aquaticSiteType!="river"),]
MicChemsummary<-data.frame(fieldMicChem %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(MicChemsummary)[5]<-"MicChem"

########### Benthic Macroinvertebrates ###########

names(neonInv)
fieldInv<-neonInv$inv_fieldData
fieldInv$year<-year(fieldInv$collectDate)
fieldInv$month<-month(fieldInv$collectDate)

# remove  rivers

fieldInv<-fieldInv[which(fieldInv$aquaticSiteType!="river"),]
Invsummary<-data.frame(fieldInv %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(Invsummary)[5]<-"Inv"

########### Plant/Macroalgae Point counts ###########

#names(neonPlantPC)
#fieldPlantPC<-neonPlantPC$apc_pointTransect
#fieldPlantPC$year<-year(fieldPlantPC$collectDate)
#fieldPlantPC$month<-month(fieldPlantPC$collectDate)

# remove 'sampling impractical' 
#fieldPlantPC<-fieldPlantPC[is.na(fieldPlantPC$samplingImpractical),]
#PlantPCsummary<-data.frame(fieldPlantPC %>% group_by(siteID,year,month) %>% count())
#PlantPCsummary$aquaticSiteType<-'stream'
#names(PlantPCsummary)[4]<-"PlantPC"

########### plant/macroalgae clipharvests ###########

names(neonPlantCH)
fieldPlantCH<-neonPlantCH$apl_clipHarvest
fieldPlantCH$year<-year(fieldPlantCH$collectDate)
fieldPlantCH$month<-month(fieldPlantCH$collectDate)

# remove 'sampling impractical' and rivers
fieldPlantCH<-fieldPlantCH[is.na(fieldPlantCH$samplingImpractical),]
fieldPlantCH<-fieldPlantCH[which(fieldPlantCH$aquaticSiteType!="river"),]
PlantCHsummary<-data.frame(fieldPlantCH %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(PlantCHsummary)[5]<-"PlantCH"


########### plant/macroalgae chemistry ###########

names(neonPlantChem)
fieldPlantChem<-neonPlantChem$apl_clipHarvest
fieldPlantChem$year<-year(fieldPlantChem$collectDate)
fieldPlantChem$month<-month(fieldPlantChem$collectDate)

# remove 'sampling impractical' and rivers
fieldPlantChem<-fieldPlantChem[is.na(fieldPlantChem$samplingImpractical),]
fieldPlantChem<-fieldPlantChem[which(fieldPlantChem$aquaticSiteType!="river"),]
PlantChemsummary<-data.frame(fieldPlantChem %>% group_by(aquaticSiteType,siteID,year,month) %>% count())
names(PlantChemsummary)[5]<-"PlantChem"

########### plant cover ###########

names(neonPlantCov)
fieldPlantCov<-neonPlantCov$rip_percentComposition
fieldPlantCov$year<-year(fieldPlantCov$startDate)
fieldPlantCov$month<-month(fieldPlantCov$startDate)

# remove 'sampling impractical' and rivers
fieldPlantCov<-fieldPlantCov[is.na(fieldPlantCov$samplingImpractical),]
fieldPlantCov<-fieldPlantCov[which(fieldPlantCov$siteID %in% fieldFish$siteID),]
PlantCovsummary<-data.frame(fieldPlantCov %>% group_by(siteID,year,month) %>% count())
PlantCovsummary$aquaticSiteType<-'stream'
names(PlantCovsummary)[4]<-"PlantCov"


########### join data frames ###########

sumry<-full_join(Fishsummary,Zoopssummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                           "siteID"=="siteID",
                                           "year"=="year",
                                           "month"=="month"))

sumry<-full_join(sumry,Invsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                                  "siteID"=="siteID",
                                                  "year"=="year",
                                                  "month"=="month"))
sumry<-full_join(sumry,MicColsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                          "siteID"=="siteID",
                                          "year"=="year",
                                          "month"=="month"))
sumry<-full_join(sumry,MicChemsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                          "siteID"=="siteID",
                                          "year"=="year",
                                          "month"=="month"))
#sumry<-full_join(sumry,PlantPCsummary,join_by("aquaticSiteType"=="aquaticSiteType",
 #                                         "siteID"=="siteID",
  #                                        "year"=="year",
   #                                       "month"=="month"))
sumry<-full_join(sumry,PlantCHsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                              "siteID"=="siteID",
                                              "year"=="year",
                                              "month"=="month"))
sumry<-full_join(sumry,PlantChemsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                              "siteID"=="siteID",
                                              "year"=="year",
                                              "month"=="month"))
sumry<-full_join(sumry,PlantCovsummary,join_by("aquaticSiteType"=="aquaticSiteType",
                                                "siteID"=="siteID",
                                                "year"=="year",
                                                "month"=="month"))

# convert NAs to zero
sumry[is.na(sumry)]<-0

#summarize by year

sumbyyear <- sumry %>% 
  group_by(aquaticSiteType, siteID, year) %>% 
  summarise(
    Fish = sum(as.numeric(Fish)),
    Zoop = sum(as.numeric(Zoop)),
    Inv = sum(as.numeric(Inv)),
    MicCol = sum(as.numeric(MicCol)),
    MicChem = sum(as.numeric(MicChem)),
    #PlantPC = sum(as.numeric(PlantPC)),
    PlantCH = sum(as.numeric(PlantCH)),
    PlantChem = sum(as.numeric(PlantChem)),
    PlantCov = sum(as.numeric(PlantCov))
  )

# plot data availability


binary_yearly <- sumbyyear %>%
  mutate(across(Fish:PlantCov, ~ . > 0))

long_yearly <- binary_yearly %>%
  pivot_longer(cols = Fish:PlantCov, names_to = "DataProduct", values_to = "Present")

long_prepped <- long_yearly %>%
  mutate(
    year_product = paste(year, DataProduct, sep = "_"),
    siteID = factor(siteID),  
    aquaticSiteType = factor(aquaticSiteType, levels = c("lake", "stream"))
  )

site_order <- long_prepped %>%
  distinct(siteID, aquaticSiteType) %>%
  arrange(aquaticSiteType) %>%
  pull(siteID)

ggplot(long_prepped %>% filter(Present == TRUE),
       aes(
         x = year_product,
         y = factor(siteID, levels = site_order),
         color = aquaticSiteType,
         shape = DataProduct == "Fish",
         size = DataProduct == "Fish"
       )) +
  geom_point(alpha = 0.9) +
  scale_color_manual(values = c("lake" = "steelblue", "stream" = "darkorange")) +
  scale_size_manual(values = c(`FALSE` = 2.5, `TRUE` = 4)) +
  labs(
    x = "Year & Data Product",
    y = "Site"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "none"
  )
