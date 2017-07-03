
if(year == 1996) FYC <- FYC %>% mutate(AGE42X=AGE2X,AGE31X=AGE1X)

FYC <- FYC %>%
  mutate_at(vars(starts_with("AGE")),funs(replace(.,.< 0,NA))) %>%
  mutate(AGELAST = coalesce(AGE.yy.X,AGE42X,AGE31X))

