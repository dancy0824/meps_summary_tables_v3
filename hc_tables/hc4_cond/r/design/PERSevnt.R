# Sum by person, condition, event;
all_persev <- all_events %>%
  group_by(.subgrps.DUPERSID,VARSTR,VARPSU,PERWT.yy.F,Condition,event,ind,count) %>%
  summarize_at(vars(XP.yy.X:OZ.yy.X),sum) %>% ungroup

PERSevnt <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT.yy.F,           
  data = all_persev,
  nest = TRUE) 