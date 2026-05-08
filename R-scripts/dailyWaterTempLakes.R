################################################################################
#' @title dailyWaterTempLakes

#' @author
#' Robert Hensley \email{hensley@battelleecology.org} \cr

#' @description A script that calculates daily min, max and mean water temperature 
#' for NEON lake sites.

################################################################################
library(neonUtilities)
library(tidyverse)
library(plotly)

siteList<-c("all")
tsd<- neonUtilities::loadByProduct(dpID = "DP1.20264.001",site = siteList,startdate = "2010-01",enddate = "2025-12",
                                   package = "basic",include.provisional = T,check.size = F,
                                   token=Sys.getenv("NEON_TOKEN"))
list2env(tsd,.GlobalEnv)
tsm<- neonUtilities::loadByProduct(dpID = "DP1.20055.001",site = siteList,startdate = "2010-01",enddate = "2025-12",
                                   package = "basic",include.provisional = T,check.size = F,
                                   token=Sys.getenv("NEON_TOKEN"))
list2env(tsm,.GlobalEnv)

# Remove quality flagged data
TSD_30min_clean<-TSD_30_min[(TSD_30_min$tsdWaterTempFinalQF==0),]
SST_30min_clean<-SST_30min[(SST_30min$waterTempFinalQF==0),]

# Merge surface and subsurface tables
TSD_30min_clean<-TSD_30min_clean[,c("domainID","siteID","startDateTime","tsdWaterTempMean")]
names(TSD_30min_clean)<-c("domainID","siteID","startDateTime","waterTemp")
SST_30min_clean<-SST_30min_clean[,c("domainID","siteID","startDateTime","waterTemp")]
allData<-rbind(TSD_30min_clean,SST_30min_clean)

# Create column of days
allData$date<-lubridate::round_date(allData$startDateTime,unit="1 day")

# Create summary data table
waterTemp_daily <- allData %>%
  group_by(siteID, date) %>%
  summarise(
    meanTemp = mean(waterTemp, na.rm = TRUE),
    maxTemp = max(waterTemp, na.rm = TRUE),
    minTemp = min(waterTemp, na.rm = TRUE),
    .groups = 'drop' # Recommended to prevent grouping issues later
  )

# Write out file
write.csv(waterTemp_daily,file="NEON_daily_temp_stats.csv")

