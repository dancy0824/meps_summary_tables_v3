# Sum by person, condition, across event
all_pers <- all_events %>%
  group_by(.subgrps.DUPERSID,VARSTR,VARPSU,PERWT.yy.F,Condition,ind,count) %>%
  summarize_at(vars(XP.yy.X:OZ.yy.X),sum) %>% ungroup

PERSdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT.yy.F,           
  data = all_pers,
  nest = TRUE) 