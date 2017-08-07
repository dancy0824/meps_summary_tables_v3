FYC <- FYC %>% 
  mutate(
    adult_rating = as.factor(case_when(
      .$ADHECR42 >= 9 ~ "9-10 rating",      
      .$ADHECR42 >= 7 ~ "7-8 rating",
      .$ADHECR42 >= 0 ~ "0-6 rating",
      .$ADHECR42 == -8 ~ "Don\'t know",
      .$ADHECR42 == -9 ~ "Non-response",
      .$ADHECR42 == -1 ~ "Inapplicable",
      TRUE ~ "Missing")))
