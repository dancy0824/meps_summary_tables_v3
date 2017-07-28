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

  df %>% 
    mutate(event = evnt,
           PR.yy.X = PV.yy.X+TR.yy.X, 
           OZ.yy.X = OF.yy.X+SL.yy.X+OT.yy.X+OR.yy.X+OU.yy.X+WC.yy.X+VA.yy.X) %>%
    select(one_of("DUPERSID","event","SEEDOC",
                  "XP.yy.X","SF.yy.X","MR.yy.X","MD.yy.X","PR.yy.X","OZ.yy.X"))
}

RX <- read.xport('.PUFdir./.RX..ssp')%>% rename_sops('RX',year,hosp=F)
DVT <- read.xport('.PUFdir./.DV..ssp')%>% rename_sops('DV',year,hosp=F)
OMA <- read.xport('.PUFdir./.OM..ssp')%>% rename_sops('OM',year,hosp=F)
IPT <- read.xport('.PUFdir./.IP..ssp')%>% rename_sops('IP',year,hosp=T)
ERT <- read.xport('.PUFdir./.ER..ssp')%>% rename_sops('ER',year,hosp=T)
OPT <- read.xport('.PUFdir./.OP..ssp')%>% rename_sops('OP',year,hosp=T)
OBV <- read.xport('.PUFdir./.OB..ssp')%>% rename_sops('OB',year,hosp=F)
HHT <- read.xport('.PUFdir./.HH..ssp')%>% rename_sops('HH',year,hosp=F)

# Define sub-levels for office-based, outpatient, and home health
OBV <- OBV %>% mutate(
  event_v2X = recode_factor(SEEDOC,
    .default='Missing','1'='OBD','2'='OBO'))

OPT <- OPT %>% mutate(
  event_v2X = recode_factor(SEEDOC,
    .default='Missing','1'='OPY','2'='OPZ'))

# Stack events into single dataset
stacked_events <- bind_rows(RX,DVT,OMA,IPT,ERT,OPT,OBV,HHT)

# Merge EVENTS data onto FYC file
FYCsub <- FYC %>% select(.subgrps.DUPERSID,PERWT.yy.F,VARSTR,VARPSU)
EVENTS <- stacked_events %>% full_join(FYCsub, by='DUPERSID')
