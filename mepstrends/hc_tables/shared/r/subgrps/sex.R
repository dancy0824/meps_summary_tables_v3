
FYC <- FYC %>% 
  mutate(sex = recode_factor(SEX,
    "1" = "Male",
    "2" = "Female"))

