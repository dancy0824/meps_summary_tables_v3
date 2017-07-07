FYC <- FYC %>%
  mutate(adult_routine = recode_factor(
    ADRTWW42,  
    "4" = "Always",
    "3" = "Usually",
    "2" = "Sometimes/Never",
    "1" = "Sometimes/Never",
    "-8" = "Don\'t know",
    "-7" = "Non-response",
    "-9" = "Non-response",
    "-1" = "Inapplicable"))