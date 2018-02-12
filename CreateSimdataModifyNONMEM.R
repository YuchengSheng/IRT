# In Simulation folder, run000.mod & pirana_auto_mpi.pnm are needed
###########################################################################
# Creat data sets for NONMEM simulation    --------------------------------
###########################################################################

library(dplyr)
Nsub  <- 800                   # number of Subjects
ITEM  <- 1:33                  # item index
ID    <- 1:Nsub                # Subject ID
EVENT <- c(0,2,4,5,6)          # Visit time (0,0.6,1,1.5 & 2 years)

# definition of prior distribution of drug effect 
NsamplePrior <- 100
# PriorMean <- 0.3
# PriorSD <- 0.09
PriorMin <- 0.1
PriorMax <- 0.5
set.seed(1)
# SampleE <- rnorm(NsamplePrior,PriorMean,PriorSD)
SampleE <- runif(NsamplePrior,PriorMin,PriorMax)
# Check the samples of drug effect
# density(SampleE) %>% plot()
hist(SampleE)

# Creat Placebo data. TIMED is time of day, DID is indicator for group
PDSIMPlacebo <- expand.grid(EVENT=EVENT,ITEM = ITEM,ID =ID ) 
PDSIMPlacebo$DRUGE <- 0
PDSIMPlacebo$DV <- NA
PDSIMPlacebo$TIME <- ifelse(PDSIMPlacebo$EVENT <5,
                            PDSIMPlacebo$EVENT*3,
                            (PDSIMPlacebo$EVENT-2)*6)
PDSIMPlacebo$TIMED <- PDSIMPlacebo$TIME*30
PDSIMPlacebo <- PDSIMPlacebo %>% 
  arrange(DRUGE,ID,EVENT,ITEM) %>% 
  select(ID,TIMED,ITEM,DV,EVENT,TIME,DRUGE) %>% 
  mutate(DID = ID)

# Creat drug arm data.DRUGI is indicator for different Effect
# DrugE is a sample from prior drug effect distribution
fCreatIndDrugData <- function(DrugE,DrugI){
  PDSIMPlacebo %>% 
    mutate(ID = Nsub +ID,
           DRUGE = DrugE) %>% 
    bind_rows(PDSIMPlacebo) %>% 
    mutate(DRUGI = DrugI) %>% 
    arrange(DRUGE,ID,EVENT,ITEM)
}

# Check the function
fCreatIndDrugData(0.5,1) %>% str()

# Creat a list include all the data for samples of drug effect
IndDrugData <- mapply(fCreatIndDrugData,SampleE,1:NsamplePrior,SIMPLIFY =F)
# Export simulated data sets into seperate csv files
setwd("C:/Users/ys278065/Desktop/IRT/Simulation/")
mapply(write.csv,IndDrugData,
       paste("PDSIME",sprintf("%03.0f", 1:NsamplePrior),".csv",sep=""),
       row.names=F,quote=F,na = ".",SIMPLIFY =F)
write.csv(SampleE,"SampleE.csv")

###########################################################################
# Creat NONMEM control files for simulation from each dataset  ------------
###########################################################################

library(metrumrg)
mod <- readLines('run000.mod')

# function for modifying the NONMEM code
fmodifyNMcode <- function(runNO){
  metaSub(
    mod,
    names=paste("run",sprintf("%03.0f", runNO),sep=""),
    pattern=c('PDSIME000.csv','sdtab000'),
    replacement=c(
      paste("PDSIME",sprintf("%03.0f", runNO),".csv",sep=""),
      paste("sdtab",sprintf("%03.0f", runNO),sep="")),
    out='.',
    suffix='.mod'
  )
}
# Export all modified mod files
mapply(fmodifyNMcode,1:NsamplePrior,SIMPLIFY =F)

# execute from psn with some arguments
psnp <- "execute -parafile=pirana_auto_mpi.pnm -nodes=8 -nm_version=nm73 -clean=3 " 
fRcallNONMEM <- function(mod) {
  shell(paste(psnp,mod,sep=""),wait=TRUE)
}

mods <- paste("run",sprintf("%03.0f", 1:NsamplePrior),".mod ",sep="")
# paste(psnp,mods[[1]],sep="")
# fRcallNONMEM(mods[[1]])

# Call nonmem files in a implicit loop
# Need more than 3 days and more than 200GB free space 
mapply(fRcallNONMEM,mods)
