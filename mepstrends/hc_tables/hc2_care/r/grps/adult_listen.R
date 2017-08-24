FYC <- FYC %>%
  mutate(adult_listen = recode_factor(
    ADLIST42,.freq.))
