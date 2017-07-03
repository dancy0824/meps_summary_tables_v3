
FYC <- FYC %>% 
  mutate(agegrps = cut(AGELAST,
    breaks = c(-1,4.5,17.5,44.5,64.5,Inf),
    labels = c("Under 5","5-17","18-44","45-64","65+"))) %>%
  mutate(agegrps_v2X = cut(AGELAST,
    breaks = c(-1,17.5,64.5,Inf),
    labels = c("Under 18","18-64","65+")))

