library(neonUtilities)


source("/Users/kelsey/Github/neon-biorepo-tools/configini.R")

datapath<-'/Users/kelsey/Github/RCN/2025 Raw Release Data/'

# Fish

neonFish<-loadByProduct(dpID='DP1.20107.001',
                        site='all',
                        startdate='2010-01-01',
                        enddate='2025-04-01',
                        check.size=FALSE, 
                        release='RELEASE-2025',
                        include.provisional=FALSE,
                        token=Neon_Token)
saveRDS(neonFish,paste0(datapath,'neonFish.Robj'))

# Zooplankton

neonZoops<-loadByProduct(dpID='DP1.20219.001',
                         site='all',
                         startdate='2010-01-01',
                         enddate='2025-04-01',
                         check.size=FALSE, 
                         release='RELEASE-2025',
                         include.provisional=FALSE,
                         token=Neon_Token)

saveRDS(neonZoops,paste0(datapath,'neonZoops.Robj'))

# Microalgae collection

neonMicCol<-loadByProduct(dpID='DP1.20166.001',
                          site='all',
                          startdate='2010-01-01',
                          enddate='2025-04-01',
                          check.size=FALSE, 
                          release='RELEASE-2025',
                          include.provisional=FALSE,
                          token=Neon_Token)

saveRDS(neonMicCol,paste0(datapath,'neonMicCol.Robj'))

# Microalgae chemistry

neonMicChem<-loadByProduct(dpID='DP1.20163.001',
                          site='all',
                          startdate='2010-01-01',
                          enddate='2025-04-01',
                          check.size=FALSE, 
                          release='RELEASE-2025',
                          include.provisional=FALSE,
                          token=Neon_Token)

saveRDS(neonMicChem,paste0(datapath,'neonMicChem.Robj'))

# Benthic macroinvertebrates

neonInv<-loadByProduct(dpID='DP1.20120.001',
                       site='all',
                       startdate='2010-01-01',
                       enddate='2025-04-01',
                       check.size=FALSE, 
                       release='RELEASE-2025',
                       include.provisional=FALSE,
                       token=Neon_Token)

saveRDS(neonInv,paste0(datapath,'neonInv.Robj'))

# Aquatic plants/macroalgae point counts

#neonPlantPC<-loadByProduct(dpID='DP1.20072.001',
       #                site='all',
      #                 startdate='2010-01-01',
     #                  enddate='2025-04-01',
    #                   check.size=FALSE, 
   #                    release='RELEASE-2025',
  #                     include.provisional=FALSE,
 #                      token=Neon_Token)

#saveRDS(neonPlantPC,paste0(datapath,'neonPlantPC.Robj'))

# Aquatic plants/macroalgae point counts

neonPlantCH<-loadByProduct(dpID='DP1.20066.001',
                           site='all',
                           startdate='2010-01-01',
                           enddate='2025-04-01',
                           check.size=FALSE, 
                           release='RELEASE-2025',
                           include.provisional=FALSE,
                           token=Neon_Token)

saveRDS(neonPlantCH,paste0(datapath,'neonPlantCH.Robj'))

# Aquatic plants/macroalgae clip harvests

neonPlantCH<-loadByProduct(dpID='DP1.20066.001',
                           site='all',
                           startdate='2010-01-01',
                           enddate='2025-04-01',
                           check.size=FALSE, 
                           release='RELEASE-2025',
                           include.provisional=FALSE,
                           token=Neon_Token)

saveRDS(neonPlantCH,paste0(datapath,'neonPlantCH.Robj'))

# Aquatic plants/macroalgae chemistry

neonPlantChem<-loadByProduct(dpID='DP1.20063.001',
                           site='all',
                           startdate='2010-01-01',
                           enddate='2025-04-01',
                           check.size=FALSE, 
                           release='RELEASE-2025',
                           include.provisional=FALSE,
                           token=Neon_Token)

saveRDS(neonPlantChem,paste0(datapath,'neonPlantChem.Robj'))

# Riparian percent cover

neonPlantCov<-loadByProduct(dpID='DP1.20191.001',
                             site='all',
                             startdate='2010-01-01',
                             enddate='2025-04-01',
                             check.size=FALSE, 
                             release='RELEASE-2025',
                             include.provisional=FALSE,
                             token=Neon_Token)

saveRDS(neonPlantCov,paste0(datapath,'neonPlantCov.Robj'))



