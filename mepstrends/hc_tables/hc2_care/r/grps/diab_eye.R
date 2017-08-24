if(year > 2001){
  FYC <- FYC %>% 
    mutate(past_year = (DSEY.yy.53==1 | DSEY.ya.53==1),
           more_year = (DSEY.yb.53==1 | DSEB.yb.53==1),
           never_chk = (DSEYNV53 == 1),
           non_resp = (DSEY.yy.53 %in% c(-7,-8,-9))
    )
}else{
  FYC <- FYC %>%
    mutate(
      past_year = (DSEYE53 %in% c(1,2)),
      more_year = (DSEYE53 %in% c(3,4)),
      never_chk = (DSEYE53 == 5),
      non_resp  = (DSEYE53 %in% c(-7,-8,-9))
    )
}

FYC <- FYC %>% 
  mutate(
    diab_eye = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$never_chk ~ "Never had eye exam",
      .$non_resp ~ "Don\'t know/Non-response",
      TRUE ~ "Missing")))
