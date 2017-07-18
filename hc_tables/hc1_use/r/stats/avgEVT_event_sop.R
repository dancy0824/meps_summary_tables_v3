avgEVT <- function(df){
  
  pers_events <- df %>%
    group_by(DUPERSID) %>% 
    summarise(EXP = sum(XP.yy.X > 0),
              SLF = sum(SF.yy.X > 0),
              MCR = sum(MR.yy.X > 0),
              MCD = sum(MD.yy.X > 0),
              PTR = sum(PR.yy.X > 0),
              OTZ = sum(OZ.yy.X > 0)) 
  
  
  n_events <- full_join(pers_events,FYCsub,by='DUPERSID') %>%
    mutate_at(vars(EXP,SLF,MCR,MCD,PTR,OTZ), 
              function(x) ifelse(is.na(x),0,x))

  nEVTdsgn <- svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = ~PERWT14F,           
    data = n_events,
    nest = TRUE)
  
  svyby(~.sop.,by=~ind,FUN=svymean,design=nEVTdsgn)
}

OBD = OBV %>% filter(event_v2X == "OBD")
OBO = OBV %>% filter(event_v2X == "OBO")
OPY = OPT %>% filter(event_v2X == "OPY")
OPZ = OPT %>% filter(event_v2X == "OPZ")

