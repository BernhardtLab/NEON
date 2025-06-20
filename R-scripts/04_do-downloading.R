



library(neonUtilities)
library(ggplot2)
library(tidyverse)
library(kableExtra)

waq <- loadByProduct(
  dpID = "DP1.20288.001",
  site = "WALK",
  startdate = "2024-04",
  enddate = "2024-05",
  package = "expanded",
  release = "current",
  include.provisional = TRUE,
  check.size = FALSE
)



thing <- waq$waq_instantaneous


waq_down <- thing[thing$horizontalPosition == 102, ]



ggplot(waq_down, aes(x = startDateTime, y = dissolvedOxygen)) +
  geom_line(color = "blue") +
  labs(title = "Dissolved Oxygen Concentration Over Time",
       x = "Date",
       y = "Dissolved Oxygen (mg/L)")



#Cleaning and Gap-Filling NEON Aquatic Instrument Data tutorial

waq <- neonUtilities::loadByProduct(dpID="DP1.20288.001", site="WALK", 
                                    startdate="2024-04", enddate="2024-05",
                                    package="expanded", release="current",
                                    include.provisional=T,
                                    check.size=F)

list2env(waq,.GlobalEnv)


waq_down <- waq_instantaneous[(waq_instantaneous$horizontalPosition==102),]

glimpse(waq_down)


doPlotRaw <- ggplot() +
  geom_line(data = waq_down,aes(startDateTime, localDissolvedOxygenSat, colour="raw"),na.rm=TRUE) +
  geom_ribbon(data=waq_down,aes(x=startDateTime, 
                                ymin = (localDissolvedOxygenSat - localDOSatExpUncert), 
                                ymax = (localDissolvedOxygenSat + localDOSatExpUncert)), 
              alpha = 0.3, fill = "red") +
  scale_colour_manual("",breaks=c("raw"),values=c("red")) +
  ylim(0, 120) + ylab("saturation (%)") + xlab("Date")+ ggtitle("WALK dissolvedOxygenSat with Uncertainty")

doPlotRaw



issueLog_20288 <- issueLog_20288[(grepl(unique(waq_instantaneous$siteID), issueLog_20288$locationAffected))]

issueLog_20288 <- issueLog_20288[!((issueLog_20288$dateRangeStart>
                                    max(waq_instantaneous$startDateTime))
                                 |(issueLog_20288$dateRangeEnd<
                                     min(waq_instantaneous$startDateTime))),]

issueLog_20288 %>%
  kbl() %>%
  kable_styling()


waq_down$cleanDissolvedOxygenSat <- waq_down$localDissolvedOxygenSat

waq_down$cleanDissolvedOxygenSat[waq_down$localDOSatFinalQF == 1] <- NA






stepStart <- as.POSIXct("2024-04-01 00:00:00",tz="GMT")

stepEnd <- as.POSIXct("2024-06-01 00:00:00",tz="GMT") 

maxStep <- 0.1

for(i in 2:nrow(waq_down)){if((waq_down$startDateTime[i]>stepStart)&
                              (waq_down$endDateTime[i]<stepEnd)){
  if((!is.na(waq_down$localDissolvedOxygenSat[i]))&
     (!is.na(waq_down$localDissolvedOxygenSat[i-1]))){
    if(abs(waq_down$localDissolvedOxygenSat[i]-
           waq_down$localDissolvedOxygenSat[i-1])>maxStep){
      waq_down$cleanDissolvedOxygenSat[i] <- NA
      waq_down$cleanDissolvedOxygenSat[i-1] <- NA
    }}}}



rangeStart <- as.POSIXct("2024-04-01 00:00:00",tz="GMT")

rangeEnd <- as.POSIXct("2024-06-01 00:00:00",tz="GMT") 

minRange <- 80

maxRange <- 120

for(i in 1:nrow(waq_down)){
  if((waq_down$startDateTime[i]>rangeStart)&(waq_down$endDateTime[i]<rangeEnd)){
    if((!is.na(waq_down$localDissolvedOxygenSat[i]))){
      if((waq_down$localDissolvedOxygenSat[i]<minRange)|
         (waq_down$localDissolvedOxygenSat[i]>maxRange)){
        waq_down$cleanDissolvedOxygenSat[i] <- NA
      }}}}


doPlotQF <- ggplot() +
  # geom_line(data = waq_down,aes(startDateTime, localDissolvedOxygenSat, colour="raw"),na.rm=TRUE) +
  geom_line(data = waq_down,aes(startDateTime, cleanDissolvedOxygenSat, colour="clean"),na.rm=TRUE) +
  geom_ribbon(data=waq_down,aes(x=startDateTime, 
                                ymin = (cleanDissolvedOxygenSat - localDOSatExpUncert), 
                                ymax = (cleanDissolvedOxygenSat + localDOSatExpUncert)), 
              alpha = 0.3, fill = "blue") +
  scale_colour_manual("",breaks=c("raw","clean"),values=c("red","blue")) +
  ylim(0, 120) + ylab("saturation (%)") + xlab("Date")+ ggtitle("WALK dissolvedOxygenSat with Uncertainty")

doPlotQF




zoo <- read_csv("Clean Data/zooplankton.csv")
zoo_sites <- unique(zoo$siteID)


### let's go with the approach of just filtering out data that look way off, like outside of the range of 80-120



library(neonUtilities)

library(neonUtilities)

waq <- loadByProduct(
  dpID = "DP1.20288.001",
  site = "WALK",
  startdate = "2024-06",
  enddate = "2024-06",
  package = "expanded",
  check.size = FALSE
)

waq_data <- waq$waq_instantaneous

waq_data %>%
  distinct(horizontalPosition, verticalPosition)


waq_clean <- waq_data %>%
  filter(!is.na(dissolvedOxygen),
    dissolvedOxygenFinalQF == 0,
    horizontalPosition == 102,
    verticalPosition == 100) %>% select(startDateTime,siteID, horizontalPosition, verticalPosition, dissolvedOxygen)

waq_clean %>% 
  ggplot(aes(x = startDateTime, y = dissolvedOxygen)) + geom_line()


### lets try toolink

library(neonUtilities)
library(dplyr)
library(lubridate)


# Step 1: Load full-resolution data (includes 1‑min)
waq <- loadByProduct(
  dpID = "DP1.20288.001",
  site = "TOOK",
  startdate = "2016-06",
  enddate   = "2024-06",
  package   = "expanded",
  check.size= FALSE
)

waq_data <- waq$waq_instantaneous
unique(waq_data$verticalPosition)


waq_clean <- waq_data %>%
  filter(!is.na(localDissolvedOxygenSat), localDOSatFinalQF == 0 ) %>%
  mutate(date = as_date(startDateTime)) %>% 
  select(date, localDissolvedOxygenSat, contains("xygen"), everything())


waq_clean %>% 
  ggplot(aes(x = date, y = localDissolvedOxygenSat)) + geom_point() +ggtitle("Toolik Lake")


waq_daily_max_sat <- waq_clean %>%
  group_by(siteID, horizontalPosition, verticalPosition, date) %>%
  summarize(
    max_DO_sat = max(localDissolvedOxygenSat, na.rm = TRUE),
    .groups = "drop"
  )

waq_daily_max_sat %>% 
  ggplot(aes(x = date, y = max_DO_sat)) + geom_line() +ggtitle("Toolik Lake")
