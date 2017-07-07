FYC <- FYC %>% 
  mutate(
    child_rating = as.factor(case_when(
      .$CHHECR42 >= 9 ~ "9-10 rating",
      .$CHHECR42 >= 7 ~ "7-8 rating",
      .$CHHECR42 >= 0 ~ "0-6 rating",
      .$CHHECR42 == -8 ~ "Don\'t know",
      .$CHHECR42 == -9 ~ "Non-response",
      .$CHHECR42 == -1 ~ "Inapplicable",
      TRUE ~ "Missing"))
  )