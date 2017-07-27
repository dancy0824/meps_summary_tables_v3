
edit_hosp <- function(df,evnt,sops,yr){
  new_sops = paste0(sops,yr,"X")
  doc_sops = paste0(evnt,"D",sops,yr,"X")
  fac_sops = paste0(evnt,"F",sops,yr,"X")
  df[,new_sops] = df[,doc_sops] + df[,fac_sops]
  df
}

edit_mv <- function(df,evnt,sops,yr){
  new_sops = paste0(sops,yr,"X")
  old_sops = paste0(evnt,sops,yr,"X")
  df[,new_sops] = df[,old_sops]
  df
}

edit_CCCs <- function(df,evnt){
  CCCs <- names(df)[grepl("CCC",names(df))]
  newCCCs <- gsub(evnt,"",CCCs)
  df[,newCCCs] = df[,CCCs]
  df
}

rename_sops = function(df,evnt,year,hosp=F){
  yr = substring(year,3,4)
  
  sops = c("SF","MR","MD","PV","VA","TR","OF",
           "SL","WC","OT","OR","OU","XP")
  if(year <= 1999) sops <- replace(sops,sops=='TR','CH')

  if(hosp) df <- edit_hosp(df,evnt,sops,yr) else df <- edit_mv(df,evnt,sops,yr)
  if(year <= 1999) df <- df %>% rename(TR.yy.X = CH.yy.X)

  df %>% edit_CCCs(evnt) %>%
    mutate(event = evnt,
           PR.yy.X = PV.yy.X+TR.yy.X, 
           OZ.yy.X = OF.yy.X+SL.yy.X+OT.yy.X+OR.yy.X+OU.yy.X+WC.yy.X+VA.yy.X) %>%
    select(DUPERSID,event,starts_with("CCC"),
           XP.yy.X,SF.yy.X,MR.yy.X,MD.yy.X,PR.yy.X,OZ.yy.X,
           one_of("LINKIDX","EVNTIDX"))
}

# Load and combine event files
  RX <- read.xport('.PUFdir./.RX..ssp') %>% rename_sops('RX',year,hosp=F)
  IPT <- read.xport('.PUFdir./.IP..ssp')%>% rename_sops('IP',year,hosp=T)
  ERT <- read.xport('.PUFdir./.ER..ssp')%>% rename_sops('ER',year,hosp=T)
  OPT <- read.xport('.PUFdir./.OP..ssp')%>% rename_sops('OP',year,hosp=T)
  OBV <- read.xport('.PUFdir./.OB..ssp')%>% rename_sops('OB',year,hosp=F)
  HHT <- read.xport('.PUFdir./.HH..ssp')%>% rename_sops('HH',year,hosp=F)

# Sum RX purchases for each event
  RX <- RX %>%
    rename(EVNTIDX = LINKIDX) %>%
    group_by(DUPERSID,EVNTIDX,event) %>%
    summarise_at(vars(XP.yy.X:OZ.yy.X),sum) %>%
    ungroup

# Stack events into single dataset
  stacked_events <- bind_rows(RX,IPT,ERT,OPT,OBV,HHT)

# Merge EVENTS data onto FYC file
  FYCsub <- FYC %>% select(.subgrps.DUPERSID,PERWT.yy.F,VARSTR,VARPSU)
  EVENTS <- stacked_events %>% full_join(FYCsub, by='DUPERSID')


# Read in conditions file
  cond = read.xport('.PUFdir./.Conditions..ssp') %>%
    select(DUPERSID,CONDIDX,CCCODEX) %>% 
    mutate(CCS_Codes = as.numeric(as.character(CCCODEX)))

# Add collapsed condition codes
  cnd_codes <- read.csv("C:/Users/emily.mitchell/Desktop/Programming/GitHub/hhs_ahrq/MEPS/Quick_Reference_Guides/meps_condition_codes.csv")
  
                ### !!! CHANGE link to be to GITHUB instead !!! ###
  
  cnd_expanded <- cnd_codes %>% 
    separate_rows(CCS.Codes,sep=",") %>%
    separate(CCS.Codes,sep="-",c("min","max"),fill='right') %>%
    mutate(max = ifelse(is.na(max),min,max)) %>% 
    rowwise %>%
    mutate(CCS_Codes = paste0(min:max,collapse=",")) %>% 
    ungroup %>%
    separate_rows(CCS_Codes,sep=",") %>%
    mutate(CCS_Codes = as.numeric(as.character(CCS_Codes))) %>%
    select(Condition, CCS_Codes)
  
  cond <- left_join(cond,cnd_expanded,by="CCS_Codes") 

# Read in event-condition linking file
  clink1 = read.xport('.PUFdir./.CLNK..ssp') %>%
    select(DUPERSID,CONDIDX,EVNTIDX)

# Merge conditions and link file
  cond <- full_join(cond,clink1,by=c("DUPERSID","CONDIDX")) %>%
    distinct(DUPERSID,EVNTIDX,Condition, .keep_all=T)

# Merge events with conditions-link file
  all_events <- full_join(EVENTS,cond,by=c("DUPERSID","EVNTIDX")) %>%
    filter(!is.na(Condition),XP.yy.X >= 0) %>%
    mutate(count = 1, ind="Total")
