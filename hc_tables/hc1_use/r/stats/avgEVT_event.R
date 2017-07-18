avgEVT <- function(df){
  
  pers_events <- df %>%
    group_by(DUPERSID) %>% 
    summarise(EXP = sum(XP.yy.X >= 0)) 
  
  n_events <- full_join(pers_events,FYCsub,by='DUPERSID') %>%
    replace_na(list(EXP=0))
  
  nEVTdsgn <- svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = ~PERWT.yy.F,           
    data = n_events,
    nest = TRUE)
  
  svyby(~EXP,FUN=svymean,by=.by.,design=nEVTdsgn)
}

OBD = OBV %>% filter(event_v2X == "OBD")
OBO = OBV %>% filter(event_v2X == "OBO")
OPY = OPT %>% filter(event_v2X == "OPY")
OPZ = OPT %>% filter(event_v2X == "OPZ")



