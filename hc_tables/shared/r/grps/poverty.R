 
if(year == 1996) FYC <- FYC %>% rename(POVCAT96 = POVCAT)

FYC <- FYC %>%
  mutate(poverty = recode_factor(POVCAT.yy.,
    "1" = "Negative or Poor",
    "2" = "Near-poor",
    "3" = "Low income",
    "4" = "Middle income",
    "5" = "High income"))

