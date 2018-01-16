
rm(list=ls(all.names=TRUE))
library(tidyverse)

Nsub  <- 100
Nevent<- 12
ITEM  <- 1:33

ID    <- 1:Nsub
EVENT <- 0:Nevent

PDSIM <- expand.grid(ID=ID,EVENT=EVENT,ITEM = ITEM) %>% 
  arrange(ID,EVENT,ITEM) %>% 
  mutate(TIME = case_when(
    EVENT < 5 ~ EVENT*3,
    TRUE ~ (EVENT-2)*6),
    TIMED = TIME*30,
    DV = NA) %>% 
  select(ID,TIME,ITEM,DV,EVENT,TIMED)

write.csv(PDSIM,"PDSIM.csv",row.names=F,quote=F,na = ".")


# Simulate drug effect on slope -------------------------------------------
# for run32
DRUG <- c(0,0.2,0.3,0.5,0.7)
EVENT <- c(0,1,2,4)

# for run33
DRUG <- c(0,0.5,0.7,1,1.2)
EVENT <- c(0,2,4,5)


# for run34
DRUG <- c(0,0.3,0.5)
EVENT <- c(0,2,4,5)

# for run39
DRUG <- c(0,0.3,0.5)
EVENT <- c(0,2,4,5,6)

Nsub  <- 800
ITEM  <- 1:33
ID    <- 1:Nsub
PDSIMD <- expand.grid(DRUGE=DRUG,EVENT=EVENT,
                      ITEM = ITEM,ID =ID ) %>% 
  mutate(TIME = case_when(
    EVENT < 5 ~ EVENT*3,
    TRUE ~ (EVENT-2)*6),
    TIMED = TIME*30,
    DV = NA) %>% 
  arrange(DRUGE,ID,EVENT,ITEM) %>% 
  select(ID,TIMED,ITEM,DV,EVENT,TIME,DRUGE) %>% 
  mutate(DID = ID)
# write.csv(PDSIMD,"PDSIMD.csv",row.names=F,quote=F,na = ".")

# Unique ID as one study 
for (i in seq_along(DRUG)) {
  if (i==1) {tmpdf <- PDSIMD %>% 
      filter(DRUGE == unique(DRUGE)[[i]]) 
  } else {
    tmpdf <- bind_rows(tmpdf, PDSIMD %>% 
      filter(DRUGE == unique(DRUGE)[[i]]) %>% 
      mutate(ID = (i-1)*Nsub +ID))
  }
}
# write.csv(tmpdf,"PDSIMD.csv",row.names=F,quote=F,na = ".")

# write.csv(tmpdf,"PDSIMD33.csv",row.names=F,quote=F,na = ".")
# write.csv(tmpdf,"PDSIMD34.csv",row.names=F,quote=F,na = ".")
setwd("C:/Users/ys278065/Desktop/IRT")
write.csv(tmpdf,"PDSIMD39.csv",row.names=F,quote=F,na = ".")

## Selected Items  
sItem <- c(11,9,13,15,17,5,7)
tmpdf %>% 
  filter(ITEM %in% sItem) %>% 
  write.csv("PDSIMD41.csv",row.names=F,quote=F,na = ".")

unique(tmpdf$TIMED)
sqrt(0.176)
0.42/0.26
rnorm(0.26,0.176)

hist(rnorm(10000,0.26,sqrt(0.0176)))
