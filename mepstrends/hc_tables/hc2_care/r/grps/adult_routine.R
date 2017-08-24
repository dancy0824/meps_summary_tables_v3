FYC <- FYC %>%
  mutate(adult_routine = recode_factor(
    ADRTWW42,.freq.))
