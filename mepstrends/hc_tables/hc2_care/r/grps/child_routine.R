FYC <- FYC %>% 
  mutate(child_routine = recode_factor(
    CHRTWW42,.freq.))
