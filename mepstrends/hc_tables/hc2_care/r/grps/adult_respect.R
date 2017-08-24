FYC <- FYC %>%
  mutate(adult_respect = recode_factor(
    ADRESP42,.freq.))
