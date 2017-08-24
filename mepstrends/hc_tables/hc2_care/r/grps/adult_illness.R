FYC <- FYC %>%
  mutate(adult_illness = recode_factor(
    ADILWW42,.freq.))
