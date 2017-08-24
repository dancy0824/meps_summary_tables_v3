FYC <- FYC %>%
  mutate(adult_explain = recode_factor(
    ADEXPL42,.freq.))
