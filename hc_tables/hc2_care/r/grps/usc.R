if(year <= 2001) FYC <- FYC %>% rename(LOCATN42 = LOCATI42)

FYC <- FYC %>%
  mutate(usc = ifelse(HAVEUS42==2,0,LOCATN42)) %>%
  mutate(usc = recode_factor(usc, .default="Missing",
                             "0" = "No usual source of health care",
                             "1" = "Office-based",
                             "2" = "Hospital",
                             "3" = "Emergency room")
  )