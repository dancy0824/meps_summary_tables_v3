pers_events <- stacked_events %>%
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

