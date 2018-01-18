
rm(list=ls(all.names=TRUE))
graphics.off()
setwd("C:/Users/ys278065/Desktop/IRT")
library(tidyverse)
############################################################################
############################################################################
# Read simulation result from NONMEM output (raw data)---------------------
# With drug effect on slope

# sdtab23 lower slope from the model without druge ffect
# dat <- read_table(file='sdtab023',skip=1,col_names=T)
# sdtab23 higher slope from the model with druge ffect
dat <- read_table(file='sdtab032',skip=1,col_names=T)
# run33 1.5yrs and -1.2 drug effect on slope
dat <- read_table(file='sdtab033',skip=1,col_names=T)
# run34 1.5yrs and 0 0.3 0.5 drug effect on slope
dat <- read_table(file='sdtab034',skip=1,col_names=T)
names(dat)

# Calculate the number of simulations (from ID == NA) ---------------------

Nsim <- dat %>% 
  filter(is.na(ID)) %>% 
  nrow()/2 + 1


# Get rid of all ID == NA (Table & ID in the first column) ---------------

dat <- dat %>% 
  filter(!is.na(ID))


# Add indicator of simlation to the dataset -------------------------------

dat <- dat %>% 
  mutate(SimNO = (row_number()-1) %/% (nrow(.)/Nsim) +1) %>% 
  mutate(DID = ifelse(ID %% 800 == 0, 800, ID %% 800))

head(dat)
unique(dat$TIME)
datSum <- dat %>% 
  # group_by(ID,TIME,DIAB,DRUGE,DSLOPE,SimNO) %>% 
  group_by(ID,TIME,DIAB,DRUGE,SimNO) %>% 
  summarise(TS = sum(DV)) %>% 
  ungroup() 
datSum <- datSum %>% 
  group_by(ID,SimNO) %>% 
  slice(1) %>% 
  mutate(BTS=TS) %>% 
  select(ID,SimNO,BTS) %>% 
  right_join(datSum,by=c("ID","SimNO")) %>% 
  # select(ID,TIME,TS,BTS,DIAB,DRUGE,DSLOPE,SimNO) %>% 
  select(ID,TIME,TS,BTS,DIAB,DRUGE,SimNO) %>% 
  arrange(SimNO,ID,TIME) %>% 
  ungroup()

head(datSum)
write.csv(datSum,"PDTSsim.csv",row.names=F,quote=F,na = ".")

datSum1 <- datSum %>% 
  filter(SimNO == 1)
datSum1 %>% 
  ggplot(aes(x=TIME/30,y=TS)) +
  geom_line(aes(group=ID),color="lightblue3") +
  stat_smooth(method="lm",se=F,color="darkred")+
  facet_wrap(~DRUGE)

datSum1Y <- datSum1 %>% 
  filter(DRUGE == 0) %>% 
  mutate(TIMEY = TIME/360)
lme4::lmer(TS ~ TIMEY  + (1 + TIMEY | ID) ,data=datSum1Y ,REML=F)

datSum1 %>% 
  ggplot(aes(x=TIME/30,y=TS)) +
  geom_point(color="lightblue3") +
  stat_smooth(method="lm",se=F,color="darkred")+
  facet_wrap(~DRUGE)

datSum1 %>% 
  ggplot(aes(x=factor(TIME),y=TS)) +
  geom_boxplot(aes(fill=factor(DRUGE)),notch=T) 
datSum1 %>% 
  ggplot(aes(x=factor(DRUGE),y=TS)) +
  geom_boxplot(aes(fill=factor(TIME)),notch=T) 

datSum %>% 
  ggplot(aes(x=factor(TIME),y=TS)) +
  geom_boxplot(aes(fill=factor(DRUGE)),notch=T) 
datSum %>% 
  ggplot(aes(x=factor(DRUGE),y=TS)) +
  geom_boxplot(aes(fill=factor(TIME)),notch=T) 

datSum %>% 
  ggplot(aes(x=factor(DRUGE),y=DIAB)) +
  geom_boxplot(aes(fill=factor(TIME)),notch=T) 

datSum %>% 
  ggplot(aes(x=factor(TIME),y=DIAB)) +
  geom_boxplot(aes(fill=factor(DRUGE)),notch=T) 

datSum1 %>% 
  group_by(TIME,DRUGE) %>% 
  summarise(LTS = quantile(TS,0.25),
            MTS = quantile(TS,0.5),
            HTS = quantile(TS,0.75),
            meanTS = mean(TS))

datSum %>% 
  group_by(TIME,DRUGE) %>% 
  summarise(LTS = quantile(TS,0.25),
            MTS = quantile(TS,0.5),
            HTS = quantile(TS,0.75),
            meanTS = mean(TS))

PDnonmem <- read_csv("PDNONMEMTc.csv",na=".")
names(PDnonmem)

PDRaw <- PDnonmem %>% 
  filter(STATE == 0) %>% 
  group_by(PATNO,AfterDays,EVENT) %>% 
  slice(1) %>% 
  filter(EVENT %in% c(0,1,2,4)) %>% 
  filter(AfterDays <366) %>%
  select(PATNO,AfterDays,EVENT,TotalNP3,TDM,PDMstart) %>% 
  mutate(TIME = AfterDays/365,
         TREATED = ifelse(TDM==0,0,1))

PDnonmem <-PDnonmem %>% 
  mutate(TIME = AfterDays/365)
PDnonmem %>%
  filter(STATE == 0 & AfterDays >=0) %>%
  group_by(PATNO,TIME,EVENT) %>%
  slice(1) %>%
  ggplot(aes(x=TIME)) +
  geom_point(aes(y=TotalNP3),shape=1,alpha=0.3) +
  stat_smooth(aes(y=TotalNP3),method='loess',
              span=0.4,se=F,color="darkred")+
#   stat_smooth(aes(y=TotalNP3),method='lm',se=F) +
  xlab("Time(yr)") +
  ylim(c(0,90))

PDnonmem %>% 
  filter(STATE == 0 & AfterDays >=0) %>% 
  group_by(PATNO,TIME,EVENT) %>% 
  slice(1) %>% 
  ggplot(aes(x=TIME)) +
  geom_point(aes(y=TotalNP3),shape=1,alpha=0.3) +
  # stat_smooth(aes(y=TotalNP3),method='loess',
  #             span=0.4,se=F,color="darkred")+
  stat_smooth(aes(y=TotalNP3),method='lm',se=F) +
  stat_smooth(data=PDRaw,aes(x=TIME,y=TotalNP3),
              method='lm',se=F,color="darkred") +
  xlab("Time(yr)") +
  ylim(c(0,90))

PDnonmem %>% 
  filter(STATE == 0 & AfterDays >=0) %>% 
  filter(EVENT %in% 0:12) %>% 
  group_by(PATNO,EVENT) %>% 
  slice(1) %>%
  ungroup() %>% 
  group_by(EVENT) %>% 
  summarise(mean = mean(TotalNP3,na.rm=T),
            sd = sd(TotalNP3,na.rm=T)) %>% 
  knitr::kable(format="rst")

lm(TotalNP3 ~ TIME  + (0 + TIME | PATNO) ,data=PDnonmem )
a <- lm(TotalNP3 ~ TIME + STATE ,data=PDnonmem )
summary(a)
PDRaw %>% 
  ggplot(aes(x=factor(EVENT),y=TotalNP3)) +
  geom_boxplot(notch=T) 


PDRaw %>% ggplot(aes(x=TIME)) +
  geom_point(aes(y=TotalNP3),shape=1,alpha=0.3) +
  stat_smooth(aes(y=TotalNP3),method='loess',
              span=0.4,se=F,color="darkred")+
  stat_smooth(aes(y=TotalNP3),method='lm',se=F)+
  ylim(c(0,90))
# PDRaw %>%
#   filter(PDMstart == 1) %>% 
#   group_by(PATNO) %>% 
#   select(PATNO,AfterDays,TotalNP3) %>% 
#   slice(1) %>% 
#   mutate(StartD = AfterDays) %>% 
#   select(-AfterDays) %>% 
#   right_join(PDRaw,by="PATNO") %>% 
#   mutate(TreatedV=ifelse(AfterDays>=StartD,1,0)

ggplot(PDRaw,aes(x=AfterDays/360)) +
  geom_point(aes(y=TotalNP3),shape=1) +
  stat_smooth(aes(y=TotalNP3),method='loess',span=0.7,se=F) 

ggplot(PDRaw,aes(x=AfterDays/360)) +
  geom_point(aes(y=TotalNP3),shape=1) +
  stat_smooth(aes(y=TotalNP3),method='lm',se=F) +
  facet_wrap(~TREATED)
lm(TotalNP3 ~ TIME  + (0 + TIME | PATNO) ,data=PDRaw )
(modr0 <- lme4::lmer(TotalNP3 ~ TIME  + (0 + TIME | PATNO) ,data=PDRaw ,REML=F))
(modr1 <-lme4::lmer(TotalNP3 ~ TIME  + (1 + TIME | PATNO) ,data=PDRaw ,REML=F))
anova(modr0,modr1)

PD6M <- PDRaw %>% 
  filter(TIME <=0.5)
lme4::lmer(TotalNP3 ~ TIME  + (1 + TIME | PATNO) ,data=PD6M ,REML=F)
#  6.473/2 =3.23 /yr
PDRaw %>% 
  group_by(EVENT) %>% 
  summarise(LTS = quantile(TotalNP3,0.25,na.rm =T),
            MTS = quantile(TotalNP3,0.5,na.rm =T),
            HTS = quantile(TotalNP3,0.75,na.rm =T),
            mTS =mean(TotalNP3,0.75,na.rm =T))

# fModNull <- function(df){
#   lmer(TS ~ BL + TIME)
# }
# 
# fModAlt <- function(df){
#   lmer(TS ~ BL + TIME + TIME|DRUGE)
# }
# df1 <-  datSum %>% 
#   filter(SimNO == 1) %>% 
#   filter(DRUGE==0.7 | DRUGE==0) %>% 
#   filter(TIME ==0 | TIME == 360) %>% 
#   group_by(ID,DRUGE,SimNO) %>% 
#   mutate(BL = TS[TIME==0],
#          TIMEY = TIME/360) 
#   
# head(df1)
# mod1 <- lm(TS ~ BL + TIME -1,data=df1)
# mod2 <- lm(TS ~ BL + TIME + DRUGE -1,data=df1)
# 
# anova(mod2,mod1)
# summary(mod1)
# summary(mod2)
# 
# mod5 <- lm(TS ~ TIME,data=df1)
# mod6 <- lm(TS ~ TIME + DRUGE ,data=df1)
# 
# anova(mod6,mod5)
# 
# lm(TS ~ BL+ TIME + TIME|DRUGE -1,data=df1)
# lme4::lmer(TS ~ 0 + BL + TIME + TIME|ID ,data=df1)
# mod3 <- lme4::lmer(TS ~ TIMEY + DRUGE + (0 + TIMEY | ID) ,data=df1 ,REML=F)
# coef(mod3) 
# mod4 <- lme4::lmer(TS ~ TIMEY + (0 + TIMEY| ID) ,data=df1 ,REML=F)
# coef(mod4) 
# lrtr <- anova(mod3,mod4)
# 
# lme4::lmer(TS ~  TIMEY + BL +DRUGE + (0 + TIMEY | ID) -1  ,data=df1 ,REML=F)
# mod7 <- lme4::lmer(TS ~ BL + TIMEY + DRUGE + (0 + TIMEY | ID) -1 ,data=df1 ,REML=F)
# mod8 <- lme4::lmer(TS ~ TIMEY + BL + (0 + TIMEY | ID) -1 ,data=df1 ,REML=F)
# anova(mod7,mod8)


flrt <- function(df) {
  mod0 <- lme4::lmer(TS ~ TIMEY + (0 + TIMEY| ID) ,data=df ,REML=F)
  mod1 <- lme4::lmer(TS ~ TIMEY + DRUGE + (0 + TIMEY | ID) ,data=df ,REML=F)
  # coef(mod3) 
  # coef(mod4) 
  lrtr <- anova(mod0,mod1)
  lrtr$`Pr(>Chisq)`[[2]]
}
fglrt <- function(fulldf,DE,sTIME) {
  tmpdf <-  fulldf %>% 
    # filter(SimNO == 20) %>% 
    filter(DRUGE==DE | DRUGE==0) %>% 
    filter(TIME ==0 | TIME == sTIME) %>% 
    group_by(ID,DRUGE,SimNO) %>% 
    mutate(BL = TS[TIME==0],
           TIMEY = TIME/360) %>% 
    # select(-c(BTS,DIAB,DSLOPE,BL,TIME)) %>% 
    split(.$SimNO)
  Pvalues <- map_dbl(tmpdf,flrt)
  sum(Pvalues <0.05)/length(Pvalues)
}

fglrt(datSum,DE=0.5,sTIME=540)
with(datSum,table(ID,SimNO))
datSum %>% 
  group_by(SimNO) %>% 
  filter(DID <=200) %>% 
  # with(.,table(SimNO,ID))
  fglrt(DE=1,sTIME=540)

datSum <- datSum %>% 
  # mutate(DID = ifelse(ID <=300,ID,ID %% 300 +1))
  mutate(DID = ifelse(ID %% 300 == 0, 300, ID %% 300))

map_dbl(temp,flrt)
temp[[1]]
lme4::lmer(TS ~ TIMEY + DRUGE + (0 + TIMEY| ID) ,data=temp[[2]] ,REML=F)

sItem <- c(11,9,13,15,17,5,7,23)
sItem <- c(11,9,13,15,17,5)
datSumS <- dat %>% 
  filter(ITEM %in% sItem) %>% 
  group_by(ID,TIME,DIAB,DRUGE,DSLOPE,SimNO) %>% 
  summarise(TS = sum(DV)) %>% 
  ungroup() 

datSumS <- datSumS %>% 
  arrange(SimNO,ID,TIME) 

datSumS1 <- datSumS %>% 
  filter(SimNO == 1)
datSumS1 %>% 
  ggplot(aes(x=factor(TIME),y=TS)) +
  geom_boxplot(aes(fill=factor(DRUGE)),notch=T) 
datSumS1 %>% 
  ggplot(aes(x=factor(DRUGE),y=TS)) +
  geom_boxplot(aes(fill=factor(TIME)),notch=T) 

fglrt(datSumS,0.5)

datSum1t <- datSum %>% 
  filter(SimNO == 3) %>% 
  filter(DRUGE==0.7 | DRUGE==0) %>% 
  filter(TIME == 360)
names(datSum1t)
a <-t.test(TS~DRUGE,data=datSum1t)
a$p.value

fttest <- function(df) {
  lrtr <- t.test(TS~DRUGE,data=df)
  lrtr$p.value
}
fgttest <- function(fulldf = datSum,DE) {
  tmpdf <-  fulldf %>% 
    # filter(SimNO == 20) %>% 
    filter(DRUGE==DE | DRUGE==0) %>% 
    filter(TIME == 360) %>% 
    split(.$SimNO)
  Pvalues <- map_dbl(tmpdf,fttest)
  sum(Pvalues <0.05)/length(Pvalues)
}

fgttest(datSum,0.7)

Data1Y <- PDnonmem %>%
  filter(STATE ==0) %>% 
  filter(EVENT ==0 ) %>% 
  select(PATNO,EVENT,ITEM,Score) %>%
  # mutate(ITEM = paste0("Item",ITEM)) %>%
  spread(ITEM,Score) %>%
  bind_rows(PDnonmem %>%
              filter(STATE ==0) %>% 
              filter(EVENT ==4 ) %>% 
              select(PATNO,EVENT,ITEM,Score) %>%
              spread(ITEM,Score))

covdat <-Data1Y %>% 
  select(PATNO, EVENT) %>% 
  mutate(EVENT = paste0("'",EVENT,"'"),
         ID = paste0("'",PATNO,"'")) %>% 
  select(-PATNO)

dat<-Data1Y %>% 
  select(-c(PATNO, EVENT))
library(mirt)
mod0 <- mixedmirt(dat, covdat, model=1, lr.fixed = ~ 0 + EVENT ,
                  itemtype = 'graded')   

coef(mod0)
summary(mod0)

mod1 <- mixedmirt(dat, covdat, model=1, lr.fixed = ~ 0 + EVENT ,
                  lr.random = ~ -1 + EVENT|ID,
                  itemtype = 'graded')
summary(mod1)
coef(mod1)

set.seed(1)
Ntrial <-1000
Nsub <- 500
SLOPE <- 0.282
# SLOPE <- 0.396
DE <- SLOPE*(1-0.5)
IOV <- 0.172
TIME <-1.5
ftP <- function(DG,PG){
  t.test(DG,PG)$p.value
}

pv <- vector()
for (i in 1:Ntrial){
  DG = rnorm(Nsub,DE*TIME,1+IOV)
  PG = rnorm(Nsub,SLOPE*TIME,1+IOV)
  pv[i] <- ftP(DG,PG)
}
(power <- sum(pv<0.05)/Ntrial)

A <- expand.grid(SubID = 1: Nsub,TrialNo = 1:Ntrial,Time = TIME ) %>% 
  mutate(DG = rnorm(Nsub*Ntrial,DE*TIME,1+IOV),
         PG = rnorm(Nsub*Ntrial,SLOPE*TIME,1+IOV)) %>% 
  gather(DG:PG,key = "Group", value = "Disa") %>% 
  group_by(TrialNo) %>% 
  nest(Group,Disa) %>% 
  mutate(PV = map_dbl(data, ~ t.test(Disa ~ Group,data=.)$p.value)) %>% 
  select(-data)
A %>% 
  with(sum(PV<0.05)/nrow(.))
with(A,sum(PV<0.05)/Ntrial)

fttestPower <- function(Nsub,Ntrial,Time){
  expand.grid(SubID = 1: Nsub,TrialNo = 1:Ntrial,Time = TIME ) %>% 
    mutate(DG = rnorm(Nsub*Ntrial,DE*TIME,1+IOV),
           PG = rnorm(Nsub*Ntrial,SLOPE*TIME,1+IOV)) %>% 
    gather(DG:PG,key = "Group", value = "Disa") %>% 
    group_by(TrialNo) %>% 
    nest(Group,Disa) %>% 
    mutate(PV = map_dbl(data, ~ t.test(Disa ~ Group,data=.)$p.value)) %>% 
    with(sum(PV<0.05)/nrow(.))
}
fttestPower(500,1000,1.5)


A$PV[[1]]
A$data[[1]]
t.test(Disa ~ Group,data=A$data[[1]])$p.value
pv <- vector()
for (i in 1:Ntrial){
  BL <- rnorm(Nsub,0.13,1)
  LT <- rnorm(Nsub,0.28,1.1)
  pv[i] <- ftP(BL,LT)
}
(power <- sum(pv<0.05)/Ntrial)



# Model fitting for the simulated data (confirm)
dat0 <- read_table(file='sdtab036',skip=1,col_names=T) %>% 
  group_by(ID,  TIME) %>% 
  slice(1) %>% 
  ungroup
names(dat0)
unique(dat0$ID)
dat <- dat0 %>% 
  group_by(ID,DRUGE) %>% 
  slice(1) %>% 
  select(ID,DRUGE,DIAB) %>% 
  mutate(BL = DIAB) %>% 
  select(-DIAB) %>% 
  right_join(dat0,by=c("ID","DRUGE"))
  
mod <- lm(DIAB ~ BL + TIME + DRUGE,data=dat)
summary(mod)

dat1 <- dat0 %>% 
  group_by(ID,DRUGE) %>% 
  slice(1) %>% 
  select(ID,DRUGE,RDIAB) %>% 
  mutate(BL = RDIAB) %>% 
  select(-RDIAB) %>% 
  right_join(dat0,by=c("ID","DRUGE"))

mod1 <- lm(RDIAB ~ BL + TIME + DRUGE,data=dat1)
summary(mod1)

dat1 %>% 
  ggplot(aes(y=DIAB,factor(DRUGE))) +
  geom_boxplot(notch=T) +
  facet_wrap(~TIME)

dat1 %>% 
  ggplot(aes(y=RDIAB,factor(DRUGE))) +
  geom_boxplot(notch=T) +
  facet_wrap(~TIME)
