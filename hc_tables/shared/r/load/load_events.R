rename_sops = function(df,evnt,year,hosp=F){
  sops = c("SF","MR","MD","PV","VA","TR","OF",
           "SL","WC","OT","OR","OU","XP")

  if(year <= 1999) sops <- replace(sops,sops=='TR','CH')

  yr = substring(year,3,4)
  new_sops = paste0(sops,yr,"X")
  if(hosp){
    doc_sops = paste0(evnt,"D",sops,yr,"X")
    fac_sops = paste0(evnt,"F",sops,yr,"X")
    df[,new_sops] = df[,doc_sops] + df[,fac_sops]
  }else{
    old_sops = paste0(evnt,sops,yr,"X")
    df[,new_sops] = df[,old_sops]
  }
  
  if(year <= 1999) df <- df %>% rename(TR.yy.X = CH.yy.X)

  df %>% mutate(event = evnt)
}

RX <- read.xport('.PUFdir./.RX..ssp')%>% rename_sops('RX',year,hosp=F)
DV <- read.xport('.PUFdir./.DV..ssp')%>% rename_sops('DV',year,hosp=F)
OM <- read.xport('.PUFdir./.OM..ssp')%>% rename_sops('OM',year,hosp=F)
IP <- read.xport('.PUFdir./.IP..ssp')%>% rename_sops('IP',year,hosp=T)
ER <- read.xport('.PUFdir./.ER..ssp')%>% rename_sops('ER',year,hosp=T)
OP <- read.xport('.PUFdir./.OP..ssp')%>% rename_sops('OP',year,hosp=T)
OB <- read.xport('.PUFdir./.OB..ssp')%>% rename_sops('OB',year,hosp=F)
HH <- read.xport('.PUFdir./.HH..ssp')%>% rename_sops('HH',year,hosp=F)

# Define sub-levels for office-based, outpatient, and home health
if(year == 1996){
  HH <- HH %>% mutate(MPCELIG = SELFAGEN)
  OP <- OP %>% select(-EVENTRN)
}

HH <- HH %>% mutate(
  event_v2X = recode_factor(MPCELIG,
    .default='Missing','1'='HHA','2'='HHN'))

OB <- OB %>% mutate(
  event_v2X = recode_factor(SEEDOC,
    .default='Missing','1'='OBD','2'='OBO'))

OP <- OP %>% mutate(
  event_v2X = recode_factor(SEEDOC,
    .default='Missing','1'='OPY','2'='OPZ'))

# Stack events into single dataset
stacked_events <- bind_rows(RX,DV,OM,IP,ER,OP,OB,HH) %>%
  mutate(PR.yy.X = PV.yy.X+TR.yy.X, 
         OZ.yy.X = OF.yy.X+SL.yy.X+OT.yy.X+OR.yy.X+OU.yy.X+WC.yy.X+VA.yy.X) %>%
  select(DUPERSID,event,event_v2X,
         XP.yy.X,SF.yy.X,PR.yy.X,MR.yy.X,MD.yy.X,OZ.yy.X)


# Merge EVENTS data onto FYC file
FYCsub <- FYC %>% select(.subgrps.DUPERSID,PERWT.yy.F,VARSTR,VARPSU)
EVENTS <- stacked_events %>% full_join(FYCsub, by = c('DUPERSID'))
