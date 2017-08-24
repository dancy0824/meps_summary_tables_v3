FYC <- FYC %>%
  mutate(child_listen = recode_factor(
    CHLIST42,.freq.))
