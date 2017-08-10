## MERGE FYC AND EVENT TABLES

app <- 'hc1_use'

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/app_preamble.R",chdir=F)
source("../shared/r/run_preamble.R",chdir=T)
source("dictionaries.R",chdir=T)

years = 2014:1996

#head(use_tables)

load_stat <- function(year,stat){
  cat(year,'..')
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  df = read.csv(file,stringsAsFactors = F) 
  df %>% mutate(Year = year)
}

load_years <- function(stats,years){
  out <- list()
  for(stat in stats){                 print(stat)
    out[[as.character(stat)]] <- 
      lapply(years,load_stat,stat=stat) %>%
      bind_rows %>% rm_v2 %>% add_labels(evnt_keys) %>% reorder_cols 
  }
  join_all(out) %>% dedup
}

########################################################################

### FYC FILES ###
  FYCS <- load_years(c(fyc_stats,"n","n_exp"),years=years)
  all_fyc <- FYCS %>% 
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary) 

  
### EVENT FILES ###
  EVNTS <- load_years(evnt_stats,years=years)
  all_evnt <- EVNTS %>%
    filter(!(levels1=="Missing" & grp1=="event")) %>%  # for event_v2, 'missing' is all events with no sub-types
    filter(!(levels2=="Missing" & grp2=="event")) %>%  
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary) 
  
  # Add 'all event types' to match FYC
  all_evnt <- 
    get_totals("event",label="All event types",df=all_evnt) %>% 
    bind_rows(all_evnt) %>% 
    reorder_cols 
  

## Merge FYC and Event stats    
   use_tables <- 
     full_join(all_fyc,all_evnt,by=c("grp1","grp2","levels1","levels2", "Year")) %>%
     as.data.frame

  # use_tables <- use_tables %>% filter(Year==2014)
   
   use_tables <- use_tables %>% 
     filter(!levels1 %in% c("HHA","HHN"),
            !levels2 %in% c("HHA","HHN"))
   
   use_tables %>% filter(is.na(totPOP)) %>% head(100)
   use_tables %>% filter(is.na(n) & totPOP > 0)  %>% head(100)
   use_tables %>% filter(is.na(totEVT) & pctEXP > 0) %>% head(100)
   use_tables %>% filter(is.na(avgEVT)) %>% head(100)
   
   save(use_tables, file="TABLES.Rdata")
   
#load("USE_TABLES_backup.Rdata")
