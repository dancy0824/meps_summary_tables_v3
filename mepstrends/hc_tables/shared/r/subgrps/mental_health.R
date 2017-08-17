 
if(year == 1996) FYC <- FYC %>% mutate(MNHLTH53=MNTHLTH2,MNHLTH42=MNTHLTH2,MNHLTH31=MNTHLTH1)

FYC <- FYC %>%
  mutate_at(vars(starts_with("MNHLTH")),funs(replace(.,.< 0,NA))) %>%
  mutate(mental_health = coalesce(MNHLTH53,MNHLTH42,MNHLTH31)) %>%
  mutate(mental_health = recode_factor(mental_health, .missing="Missing",.default="Missing",
    "1" = "Excellent",
    "2" = "Very good",
    "3" = "Good",
    "4" = "Fair",
    "5" = "Poor"))

