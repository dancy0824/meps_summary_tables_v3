source("run_global.R")

check_avgEVT <- function(year){
  
  print(year)
  
  avgEVT <- read.csv(sprintf("r/tables/%s/avgEVT.csv",year),stringsAsFactors = F) %>% rm_v2 %>% tbl_df
  totEVT <- read.csv(sprintf("r/tables/%s/totEVT.csv",year),stringsAsFactors = F) %>% rm_v2 %>% tbl_df
  totPOP <- read.csv(sprintf("r/tables/%s/totPOP.csv",year),stringsAsFactors = F) %>% rm_v2 %>% tbl_df
  
  totEVT <- totEVT %>%
    filter(!(levels2=="Missing" & grp2=="event")) %>% 
    filter(!levels2 %in% c("HHA","HHN")) %>%
    reorder_cols %>%
    add_labels(evnt_keys) %>%
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary) 
  
  totPOP <- totPOP %>%
    reorder_cols %>%
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary)
  
  tots <- full_join(totEVT,totPOP)
  
  ################################
  
  avgEVT <- avgEVT %>%
    reorder_cols %>% 
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary)
  
  allEVT <- full_join(avgEVT,tots) 
  
  # Check for missings (=> merge error)
  
    allEVT %>% filter(is.na(totPOP))
    allEVT %>% filter(is.na(totEVT))
    allEVT %>% filter(is.na(avgEVT) & !is.na(totEVT)) 
  
    
  # Alter totPOP on event / sop files, since looking at total pop, not just those with event / expenditure
    
    tottot = totPOP %>% filter(grp1=="ind",grp2=="ind") %>% select(totPOP) %>% unlist
    
    DEMO <- allEVT %>% filter(!grp1 %in% c("event","sop"), !grp2 %in% c("event","sop"))
    EVSOP <- allEVT %>% filter(grp1 %in% c("event","sop") | grp2 %in% c("event","sop")) 
    
    marg <- totPOP %>% filter(grp1=="ind") %>% 
      select(-grp1,-levels1) %>%
      rename(grp1=grp2,levels1=levels2)
    
    EVSOP2 <- EVSOP %>%
      select(-totPOP,-totPOP_se) %>%
      full_join(marg) %>%
      mutate(totPOP = replace(totPOP,grp1=="sop"&grp2=="event",tottot),
             totPOP = replace(totPOP,grp1=="event"&grp2=="sop",tottot))
    
    ALL <- bind_rows(DEMO,EVSOP2) %>%
      mutate(check = totEVT/totPOP,
             diff = check-avgEVT)
    
    summary(ALL$diff)
    
    #ALL %>% filter(is.na(diff))
}

checks = lapply(1996:2014,check_avgEVT)

check_avgEVT(2014)

