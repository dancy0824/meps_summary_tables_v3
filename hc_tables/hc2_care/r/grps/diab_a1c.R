FYC <- FYC %>% 
  mutate(diab_a1c = ifelse(0 < DSA1C53 & DSA1C53 < 96, 1, DSA1C53)) %>%
  mutate(diab_a1c = replace(diab_a1c,DSA1C53==96,0)) %>%
  mutate(diab_a1c = recode_factor(diab_a1c,
    "1" = "Had measurement",
    "0" = "Did not have measurement",
    "-8" = "Don\'t know",
    "-9" = "Non-response",
    "-1" = "Inapplicable"))