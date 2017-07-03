
library(foreign)
library(dplyr)
library(tidyr)
library(survey)

# Load meps data from website
year <- 2014

setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/household_component/combined_hc_tables/")

sop_vars <- sop_keep <- 
  paste0(c('XP','SF','MR','MD','PV','OF','TR',
           'SL','WC','OT','OR','OU','VA'),'14X')

FYC   = read.xport("data/h171.ssp")
cond  = read.xport("data/h170.ssp")
clink = read.xport("data/h168if1.ssp")

RX <- read.xport('data/h168a.ssp')
DV <- read.xport('data/h168b.ssp')
OM <- read.xport('data/h168c.ssp')
IP <- read.xport('data/h168d.ssp')
ER <- read.xport('data/h168e.ssp')
OP <- read.xport('data/h168f.ssp')
OB <- read.xport('data/h168g.ssp')  
HH <- read.xport('data/h168h.ssp')


OB <- OB %>% mutate(event = 'Office-based Events',
                    event_v2 = recode_factor(SEEDOC,.default='Missing',
                                             '1'='OB Physician Visits',
                                             '2'='OB Non-physician Visits'),
                    count_events=(SEETLKPV==1)*1)

OP <- OP %>% mutate(event = 'Outpatient Events',
                    event_v2 = recode_factor(SEEDOC,.default='Missing',
                                             '1'='OP Physician Visits',
                                             '2'='OP Non-physician Visits'))

if(year > 1998) OP <- OP %>% mutate(count_events=(SEETLKPV==1)*1)

ER <- ER %>% mutate(event = 'Emergency Room Visits',
                    count_events=(ERHEVIDX==-1)*1)

RX <- RX %>% mutate(event = 'Prescription Medicines')
IP <- IP %>% mutate(event = 'Inpatient Stays')

OM <- OM %>% mutate(event = 'Other Medical Expenses',
                    count_events=0)           # don't count other medical purchases as events

HH <- HH %>% mutate(event = 'Home Health Events',
                    event_v2 = recode_factor(MPCELIG,.default='Missing',
                                             '1'='HH Agency Provider',
                                             '2'='HH Independent Provider',
                                             '3'='HH Informal Care'),
                    count_events=(HHXP14X>0)*1)

DV <- DV %>% mutate(event = 'Dental Visits')


RX[,sop_vars] <- RX[,paste0('RX',sop_vars)] 
DV[,sop_vars] <- DV[,paste0('DV',sop_vars)] 
OM[,sop_vars] <- OM[,paste0('OM',sop_vars)]

IP[,sop_vars] <- IP[,paste0('IPD',sop_vars)] + IP[,paste0('IPF',sop_vars)]
ER[,sop_vars] <- ER[,paste0('ERD',sop_vars)] + ER[,paste0('ERF',sop_vars)]
OP[,sop_vars] <- OP[,paste0('OPD',sop_vars)] + OP[,paste0('OPF',sop_vars)]

OB[,sop_vars] <- OB[,paste0('OB',sop_vars)] 
HH[,sop_vars] <- HH[,paste0('HH',sop_vars)] 

EVENTS <- list('RX'=RX,'DV'=DV,'OM'=OM,'IP'=IP,'ER'=ER,'OP'=OP,'OB'=OB,'HH'=HH) %>% bind_rows 

condevent <- full_join(cond2,link,by=c("CONDIDX","DUPERSID"))
eventmerge <- full_join(condevent,EVENTS,
               by=c("DUPERSID","EVNTIDX","PERWT14F","VARSTR","VARPSU"))

all <- eventmerge %>% distinct(DUPERSID, EVNTIDX, Condition, XP14X,.keep_all=T)

all2 <- all %>%
  filter(!is.na(CONDN))

# 
# all <- all %>%
#   filter(!is.na(CONDN)) %>%
#   filter(IPXP14X >= 0)

all2$ind = 1

mepsdsgn = svydesign(id = ~VARPSU,
                     strata = ~VARSTR,
                     weights = ~PERWT14F,
                     data = all2,
                     nest = TRUE) 

r1 <- svyby(~ind,by = ~Condition+event,
            FUN=svytotal, 
            design = mepsdsgn)

round(coef(r1)/1000)




m1 <- full_join(cond,link,by=c("DUPERSID","CONDIDX"))
m2 <- full_join(IP,m1,by=c("DUPERSID","EVNTIDX","PERWT14F","VARPSU","VARSTR"))
m3 <- left_join(m2,cnd_codes2,by=c("CCCODEX"="CCS_Codes"))

m4 <- m3 %>% distinct(DUPERSID,EVNTIDX,Condition, .keep_all=T)

m5 <- m4 %>%
  #filter(!is.na(CONDN)) %>%
  filter(IPXP14X >= 0)

m5$ind = 1

mepsdsgn = svydesign(id = ~VARPSU,
                     strata = ~VARSTR,
                     weights = ~PERWT14F,
                     data = m5,
                     nest = TRUE) 

svytotal(~ind,subset(mepsdsgn,CCCODEX == 122))

svyby(~ind,by = ~Condition,FUN=svytotal,design=mepsdsgn)




mepsdsgn2 = svydesign(id = ~VARPSU,
                      strata = ~VARSTR,
                      weights = ~PERWT14F,
                      data = cond,
                      nest = TRUE) 

svytotal(~HHNUM,design=subset(mepsdsgn2,CCCODEX == '122'))










## convert CCCODEX to collapsed condition categories
cnd_codes <- read.csv("C:/Users/emily.mitchell/Desktop/Programming/GitHub/hhs_ahrq/MEPS/Quick_Reference_Guides/meps_condition_codes.csv")


cond <- cond %>% 
  mutate(CCS_Codes = as.numeric(as.character(CCCODEX)))

head(cond)
head(cnd_codes)


cnd_codes2 <- cnd_codes %>% 
  separate_rows(CCS.Codes,sep=",") %>%
  separate(CCS.Codes,sep="-",c("min","max"),fill='right') %>%
  mutate(max = ifelse(is.na(max),min,max)) %>%
  rowwise %>%
  mutate(CCS_Codes = paste0(min:max,collapse=",")) %>%
  separate_rows(CCS_Codes,sep=",") %>%
  select(Condition, CCS_Codes)
  
    
cond2 <- merge(cond,cnd_codes2,by = "CCS_Codes")
    

head(cond2)


## Number of people with care for condition (not doing just number of people)

cond3 <- cond2 %>% 
  group_by(DUPERSID,Condition) %>%
  mutate(evnum = HHNUM + IPNUM + OPNUM + OBNUM + ERNUM + RXNUM,
         anyev = 1*(evnum > 1))


mepsdsgn = svydesign(id = ~VARPSU,
                     strata = ~VARSTR,
                     weights = ~PERWT14F,
                     data = cond3,
                     nest = TRUE) 

svyby(~anyev,by = ~Condition, FUN = svytotal,design=mepsdsgn)

head(cond3)

cond3 %>% select(evnum, HHNUM, OBNUM, ERNUM) %>%
  filter(evnum == 0)




# Table 2: Number of events for selected conditions by Type of Service




