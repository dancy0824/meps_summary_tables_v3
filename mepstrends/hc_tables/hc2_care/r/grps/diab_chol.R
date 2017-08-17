if(year > 2007){
  FYC <- FYC %>% 
    mutate(
      past_year = (DSCH.yy.53==1 | DSCH.ya.53==1),
      more_year = (DSCH.yb.53==1 | DSCB.yb.53==1),
      never_chk = (DSCHNV53 == 1),
      dontknow  = (DSCH.yy.53 == -8),
      non_resp  = (DSCH.yy.53 %in% c(-7,-9))
    )
}else{
  FYC <- FYC %>% 
    mutate(
      past_year = (CHOLCK53 == 1),
      more_year = (1 < CHOLCK53 & CHOLCK53 < 6),
      never_chk = (CHOLCK53 == 6),
      dontknow  = (CHOLCK53 == -8),
      non_resp  = (CHOLCK53 %in% c(-7,-9))
    )
}

FYC <- FYC %>% 
  mutate(
    diab_chol = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$never_chk ~ "Never had cholesterol checked",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing")))
