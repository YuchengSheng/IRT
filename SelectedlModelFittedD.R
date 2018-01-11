### Import & analyse simulated NONMEM data file (run039,all items) &
### (run041 selected 7 items)
### Item parameters from the final model(run033) & 
### selected item mode (run034)
### Created Date: 18th November 2017
### Created by Yucheng Sheng
### Note: mirtCAT for estimating Disability from fixed item parameters
###       use function and purrr::map 

### Setup ########################################################
rm(list=ls(all.names=TRUE))
graphics.off()
library(tidyverse)
library(stringr)
library(mirtCAT)

### Objects for Disability estimation #############################
## Creat object to estimate individual Disability
## from obtained item parameters (3 objects)
## 1.obj for all 33 items from the final model(run033)
## 2.objr for selected items from the final model(run033)
## 3.objr2 for selected items from the model(run034) from the same items
## Get the item parameters from the final 33 items model result
## Transform to aD + d  (d = -ab) from aD - ab  (run033)
## ab is a hybrid parmeter in the model

setwd("C:/Users/ys278065/Desktop/IRT")
### From final model  
runname <- "run033"
extraw <- read_table(paste0(runname,".ext"),skip=1)

param <- extraw %>%
  filter(ITERATION==-1000000000) %>%
  select(THETA1:THETA165) %>%
  gather(THETA1:THETA165,key="THETA",value="Value") %>%
  mutate(THETA = as.numeric(str_replace(THETA, "THETA", "")))

param_t <- param %>%
  mutate(ITEM = (row_number() -1) %/% 5 +1,
         paraI = rep(1:5,33),
         lag1 = lag(Value),
         lag2 = lag(Value,n=2),
         lag3 = lag(Value,n=3),
         VALUE = case_when(
           (row_number() -1) %% 5 == 0 ~ Value,
           (row_number() -1) %% 5 == 1 ~ Value,
           (row_number() -1) %% 5 == 2 ~ Value + lag1,
           (row_number() -1) %% 5 == 3 ~ Value + lag1 +lag2,
           (row_number() -1) %% 5 == 4 ~ Value + lag1 +lag2 + lag3)) %>%
  select(c(ITEM,paraI,THETA,VALUE))

nthetaValue <- param_t %>% 
  group_by(ITEM) %>% 
  slice(1) %>% 
  mutate(DIS = VALUE) %>% 
  select(-c(VALUE,THETA,paraI)) %>% 
  right_join(param_t,by=c("ITEM")) %>% 
  mutate(Value = case_when(
    paraI == 1 ~ VALUE,
    # TRUE ~ -VALUE * DIS)) %>% # (d = -ab)
    TRUE ~ -VALUE)) %>% 
  ungroup() %>% 
  select(Value)

pars <- nthetaValue %>% 
  as.tibble %>% 
  mutate(ITEM = (row_number() -1) %/% 5 +1,
         paraI = rep(1:5,33)) %>% 
  spread(paraI,Value) %>% 
  rename(a1 = `1`,
         d1 = `2`,
         d2 = `3`,
         d3 = `4`,
         d4 = `5`) %>% 
  select(-ITEM)

## Selected Items  
sItem <- c(11,9,13,15,17,5,7)
parsr <- pars %>% 
  filter(row_number() %in% sItem)

## obj & objr used for Disability estimation
obj <- generate.mirt_object(pars, 'graded')
objr <- generate.mirt_object(parsr, 'graded')

## Get the item parameters from the reduced model (selected 7 items) 
## from aD -ab  (run034)

runname <- "run034"
extraw <- read_table(paste0(runname,".ext"),skip=1)
param <- extraw %>%
  filter(ITERATION==-1000000000) %>%
  select(THETA1:THETA35) %>%
  gather(THETA1:THETA35,key="THETA",value="Value") %>%
  mutate(THETA = as.numeric(str_replace(THETA, "THETA", "")))

param_t <- param %>%
  mutate(ITEM = (row_number() -1) %/% 5 +1,
         paraI = rep(1:5,7),
         lag1 = lag(Value),
         lag2 = lag(Value,n=2),
         lag3 = lag(Value,n=3),
         VALUE = case_when(
           (row_number() -1) %% 5 == 0 ~ Value,
           (row_number() -1) %% 5 == 1 ~ Value,
           (row_number() -1) %% 5 == 2 ~ Value + lag1,
           (row_number() -1) %% 5 == 3 ~ Value + lag1 +lag2,
           (row_number() -1) %% 5 == 4 ~ Value + lag1 +lag2 + lag3)) %>%
  select(c(ITEM,paraI,THETA,VALUE))

nthetaValue <- param_t %>% 
  group_by(ITEM) %>% 
  slice(1) %>% 
  mutate(DIS = VALUE) %>% 
  select(-c(VALUE,THETA,paraI)) %>% 
  right_join(param_t,by=c("ITEM")) %>% 
  mutate(Value = case_when(
    paraI == 1 ~ VALUE,
    TRUE ~ -VALUE)) %>% 
  ungroup() %>% 
  select(Value)

parsr2 <- nthetaValue %>% 
  as.tibble %>% 
  mutate(ITEM = (row_number() -1) %/% 5 +1,
         paraI = rep(1:5,7)) %>% 
  spread(paraI,Value) %>% 
  rename(a1 = `1`,
         d1 = `2`,
         d2 = `3`,
         d3 = `4`,
         d4 = `5`) %>% 
  select(-ITEM)

objr2 <- generate.mirt_object(parsr2, 'graded')

### Read simulation results & Set all scenarios (Selected 7 items) ############
# from NONMEM output (raw data) With drug effect on slope
# run41 2yrs and 0 0.3 0.5 drug effect on slope 

dat <- read_table(file='sdtab041',skip=1,col_names=T)
names(dat)
# setwd("C:/Users/ys278065/Desktop/IRT")

# Calculate the number of simulations (from ID == NA) 
Nsim <- dat %>% 
  filter(is.na(ID)) %>% 
  nrow()/2 + 1

# Get rid of all ID == NA (Table & ID in the first column) 
# Add indicator of simlation to the dataset 
dat <- dat %>% 
  filter(!is.na(ID)) %>% 
  mutate(SimNO = (row_number()-1) %/% (nrow(.)/Nsim) +1) %>% 
  mutate(DID = ifelse(ID %% 800 == 0, 800, ID %% 800)) %>%  # only for run039
  mutate(TIME = TIME/360)

### Set all scenarios the same as simulation setting

Sample <-c(100,200,300,400,500,600,700,800)
EOT <- c(0.5,1,1.5,2)
Eff <- c(0.3,0.5)
Scenariosf <- expand.grid(Sample=Sample,EOT=EOT,
                          Eff=Eff) %>% 
  as.data.frame() 
Scenariosf
# Selected items 
# sItem <- c(11,9,13,15,17,5,7)

### Function for statistical analyses#############################
### LRT for ANCOVA on dDV
flrt4 <- function(df) {
  mod0 <- lm(DV ~ BDV ,data=df)
  mod1 <- lm(DV ~ BDV + DRUGE ,data=df)
  lrtr <- anova(mod0,mod1)
  lrtr$`Pr(>F)`[[2]]
}
fglrt4 <- function(fulldf,DVI=1) {
  tmpdf <-  fulldf %>% 
    mutate(DV = dTS*DVI + dDIAB*(1-DVI),
           BDV = BTS*DVI + BDIAB*(1-DVI)) %>% 
    filter(TIME >0) %>% 
    split(.$SimNO)
  Pvaluest <- map_dbl(tmpdf,flrt4)
  sum(Pvaluest <0.05,na.rm=T)/length(Pvaluest)
}

### Power of SoS(Sum of scores) for all items ################

datSum <- dat %>% 
  group_by(ID,TIME,DIAB,DRUGE,SimNO,DID) %>% 
  summarise(TS = sum(DV)) %>% 
  ungroup() %>% 
  arrange(SimNO,ID,TIME,DID,DRUGE)
# Add change from Baseline
datSum <- datSum %>% 
  group_by(ID,SimNO) %>% 
  slice(1) %>% 
  mutate(BTS=TS,BDIAB = DIAB) %>% 
  select(ID,SimNO,BTS,BDIAB) %>% 
  right_join(datSum,by=c("ID","SimNO")) %>% 
  mutate(dTS = TS - BTS,
         dDIAB = DIAB - BDIAB) %>% 
  arrange(SimNO,ID,TIME,DID,DRUGE) %>% 
  ungroup()

# Subset dataset according to different SampleSize, EOF&Eff
fSIMdfn <- function(Samplef, EOTf, Efff) {
  datSum %>% 
    filter(DID <= Samplef & 
             TIME %in% c(0,EOTf) & 
             DRUGE %in% c(0,Efff))
}

# nest data of all scenatios to the list
Scenarios1 <-Scenariosf %>% 
  mutate(data = pmap(as.list(.),fSIMdfn)) 

Scenarios1 <- Scenarios1 %>% 
  mutate(dTSancova = map2_dbl(.$data,1,fglrt4),
         dDancova = map2_dbl(.$data,0,fglrt4))
Scenarios1R <- Scenarios1 %>% 
  select(-data)

rm(Scenarios1)
rm(datSum)
write.csv(Scenarios1R,"Scenarios1Rs.csv",row.names=F,quote=F,na = ".")


### Functions for calculating Disability ########################
### from fixed item parameters 

fDfit <- function(df,objt=obj){
  dft <- df %>% 
    mutate(dID = ifelse(TIME ==0,ID,ID+10000))
  dftI0 <- dft %>% 
    select(dID,ITEM,DV) %>% 
    spread(ITEM,DV) 
  
  dID <- dftI0$dID
  dftI0 <- dftI0 %>% 
    select(-dID)
  # key step: estimate Disability(F1)
  md <- fscores(objt,response.pattern = dftI0) %>% 
    as.tibble() %>% 
    select(F1)
  DD <- dft %>% 
    group_by(ID,TIME) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(as.tibble(cbind(dID,md)),
              by=c("dID")) %>% 
    arrange(ID,TIME)
  DD <- DD %>% 
    group_by(ID) %>% 
    slice(1) %>% 
    mutate(BF1=F1,BDIAB=DIAB) %>% 
    select(ID,BF1,BDIAB) %>% 
    right_join(DD,by=c("ID")) %>% 
    mutate(dDm = F1 - BF1,
           dDIAB = DIAB - BDIAB) %>% 
    arrange(ID,TIME,DID,DRUGE) %>% 
    select(-c(ITEM,DV)) %>% 
    ungroup()
}

fdflag <- function(df,DVI=1) {
  tmpdf <-  df %>% 
    mutate(DV = dDm*DVI + dDIAB*(1-DVI),
           BDV = BF1*DVI + BDIAB*(1-DVI)) %>% 
    filter(TIME >0) 
  Pvaluest <- flrt4(tmpdf)
}

# Assign Obj for all items
fDDcomrAll <- function(df,objt=obj,DVI=1){
  tmp<- fDfit(df,objt)
  fdflag (tmp,DVI)
}
fglrtDDrAll <- function(fulldf,DVI=1) {
  tmpdf <-  fulldf %>%
    split(.$SimNO)
  Pvalues <- map_dbl(tmpdf,fDDcomrAll)
  sum(Pvalues <0.05,na.rm=T)/length(Pvalues)
}

# Assign Obj=objr for Selected items
fDDcomrS <- function(df,objt=objr,DVI=1){
  tmp<- fDfit(df,objt)
  fdflag (tmp,DVI)
}
fglrtDDrS <- function(fulldf,DVI=1) {
  tmpdf <-  fulldf %>%
    split(.$SimNO)
  Pvalues <- map_dbl(tmpdf,fDDcomrS)
  sum(Pvalues <0.05,na.rm=T)/length(Pvalues)
}

# Assign Obj=objr2 for Selected items and reduced model
fDDcomrS2 <- function(df,objt=objr2,DVI=1){
  tmp<- fDfit(df,objt)
  fdflag (tmp,DVI)
}
fglrtDDrS2 <- function(fulldf,DVI=1) {
  tmpdf <-  fulldf %>%
    split(.$SimNO)
  Pvalues <- map_dbl(tmpdf,fDDcomrS2)
  sum(Pvalues <0.05,na.rm=T)/length(Pvalues)
}

### Power of Disabilty from all items ########################

# Subset dataset according to different SampleSize, EOF&Eff
fSIMdfnIall <- function(Samplef, EOTf, Efff ) {
  dat %>% 
    filter(DID <= Samplef & 
             TIME %in% c(0,EOTf) & 
             DRUGE %in% c(0,Efff))
}

# data is too big and read and remove object during loop
nsec <-4
ScenariosI2R <- tibble()
for (i in 1:nsec) {
  if (i == 1){
    tmp <- Scenariosf %>% 
      filter(row_number()<=n()/nsec) %>% 
      mutate(data = pmap(as.list(.),fSIMdfnIall)) 
  } else {
    tmp <- Scenariosf %>% 
      filter(row_number()>n()*(i-1)/nsec & row_number()<=n()*i/nsec) %>% 
      mutate(data = pmap(as.list(.),fSIMdfnIall)) 
  }
  tmp <- tmp %>% 
    mutate(D1 = map2_dbl(.$data,1,fglrtDDrS),
           D2 = map2_dbl(.$data,1,fglrtDDrS2)) %>% 
    select(-data)
  ScenariosI2R <- rbind(ScenariosI2R,tmp)
}

write.csv(ScenariosI2R,"ScenariosI2Rs.csv",row.names=F,quote=F,na = ".")

### Plots of power from merged results #######################
# Scenarios1Rs ----- From simulated D and sum of scores of selected 7 items
# ScenariosI2Rs ----- From fitted D of selected 7 items 
#                     (D1 from all item model,D2 from 7 items model)
# SimRcsv <- c("Scenarios1Rs.csv",
#              "ScenariosI2Rs.csv")
# simRlist <- map(SimRcsv,read_csv)
simRlist <- list(Scenarios1R,ScenariosI2R)
simRlist[[1]] <- simRlist[[1]] %>% 
  rename(Sum_of_Selected_Items = dTSancova,
         Simulated_D = dDancova)
simRlist[[2]] <- simRlist[[2]] %>% 
  rename(Estimated_D_Selected_Items = D1,
         Estimated2_D_Selected_Items = D2)


simRdf <- simRlist %>% 
  reduce(left_join,by=c("Sample","EOT","Eff"))

simRdf4plot <- simRdf %>% 
  gather(Sum_of_Selected_Items:Estimated2_D_Selected_Items,
         key="Method",value="Power") 

# Disability of selected items from 2 estimation model 
simRdf4plot %>% 
  ggplot(aes(x=Sample,y=Power)) +
  geom_line(aes(colour=Method),size=1) +
  geom_hline(yintercept =0.8,linetype=2) +
  facet_grid(EOT ~ Eff) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  xlab("Sample size")

simRdf4plot %>% 
  ggplot(aes(x=Sample,y=Power)) +
  geom_line(aes(colour=Method),size=1) +
  geom_hline(yintercept =0.8,linetype=2) +
  facet_grid(Eff ~ EOT) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  xlab("Sample size")

