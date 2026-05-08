################################################################################
#' @title phytoplanktonChlorophyll

#' @author
#' Robert Hensley \email{hensley@battelleecology.org} \cr

#' @description A script compiles phytoplanton chlorophyll a concentrations
#' for NEON lake site.

################################################################################
library(neonUtilities)

siteList <- c("BARC","SUGG","CRAM","LIRO","PRLA","PRPO","TOOK")
alg<- neonUtilities::loadByProduct(dpID = "DP1.20163.001",site = siteList,startdate = "2010-01",enddate = "2025-12",
                                         package = "basic",include.provisional = T,check.size = F,
                                         token=Sys.getenv("NEON_TOKEN"))
list2env(alg,.GlobalEnv)

# Combines new format and old format tables
newTable<-alg_algaeDataPerSampleCompChl[,c("domainID","siteID","collectDate","sampleID","analyte","analyteConcentration")]
oldTable<-alg_algaeExternalLabDataPerSample[,c("domainID","siteID","collectDate","sampleID","analyte","analyteConcentration")]
combinedTable<-rbind(oldTable,newTable)

# Filters just chlorophyll concentrations
chlorophyll<-combinedTable[(combinedTable$analyte=="chlorophyll a"),]

# Filters just phytoplankton samples
chlorophyllPhytoplankton <- chlorophyll[grepl("PHYTOPLANKTON", chlorophyll$sampleID), ]

chlorophyllPhytoplankton<-chlorophyllPhytoplankton[,c("domainID","siteID","collectDate","sampleID","analyteConcentration")]
colnames(chlorophyllPhytoplankton)<-c("domainID","siteID","collectDate","sampleID","chlorophyllMicrogramsPerLiter")
chlorophyllPhytoplankton<-na.omit(chlorophyllPhytoplankton)

write.csv(chlorophyllPhytoplankton,file="NEON_phytoplanton_chlorophyll.csv")



