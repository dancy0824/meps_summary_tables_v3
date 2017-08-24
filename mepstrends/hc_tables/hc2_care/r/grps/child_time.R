FYC <- FYC %>%
  mutate(child_time = recode_factor(
    CHPRTM42,.freq.))
