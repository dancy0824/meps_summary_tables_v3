FYC <- FYC %>%
  mutate(child_illness = recode_factor(
    CHILWW42,.freq.))
