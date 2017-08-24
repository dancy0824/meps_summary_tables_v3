FYC <- FYC %>%
  mutate(adult_time = recode_factor(
    ADPRTM42,.freq.))
