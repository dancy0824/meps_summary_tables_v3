if(year > 2007){
  FYC <- FYC %>% 
    mutate(
      past_year = (DSFT.yy.53==1 | DSFT.ya.53==1),
      more_year = (DSFT.yb.53==1 | DSFB.yb.53==1),
      never_chk = (DSFTNV53 == 1),
      dontknow  = (DSFT.yy.53 == -8),
      non_resp  = (DSFT.yy.53 %in% c(-7,-9)),
      not_past_year = FALSE
    )
}else{
  FYC <- FYC %>% 
    mutate(
      past_year = (DSCKFT53 == 1),
      more_year = FALSE,
      never_chk = FALSE,
      not_past_year = (DSCKFT53 == 0),
      dontknow  = (DSCKFT53 == -8),
      non_resp  = (DSCKFT53 %in% c(-7,-9))
    )
}

FYC <- FYC %>% 
  mutate(
    diab_foot = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$not_past_year ~ "Never had feet checked",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing"))
  )