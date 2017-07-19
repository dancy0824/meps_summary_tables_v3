## MERGE FYC AND EVENT TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/global.R",chdir=T)
source("global.R",chdir=T) 
source("run_global.R")

years = 2014:1996

load_stats <- function(stat,year){
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  df <- read.csv(file,stringsAsFactors = F) 
  df %>% rm_v2 %>% reorder_cols
}

load_years <- function(stats,years){
  out <- list()
  for(year in years){                 cat(year,"..")
    out[[as.character(year)]] <- 
      lapply(stats,load_stats,year=year) %>%
      join_all(by=c("grp1","grp2","levels1","levels2")) %>%
      mutate(Year = year)
  }
  bind_rows(out) %>% dedup
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
    filter(!(levels2=="Missing" & grp2=="event")) %>%  # for event_v2, 'missing' is all events with no sub-types
    add_labels(evnt_keys) %>%
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary) 
  
  # Add 'all event types' to match FYC
  all_evnt <- 
    get_totals("event",label="All event types",df=all_evnt) %>% 
    bind_rows(all_evnt)
  
### avgEVT: Average event per person ###
  
  AVGEVNTS <- load_years("avgEVT",years=years)
  
  avg_evnt <- AVGEVNTS  %>%
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary)
 
  avg_evnt <- 
    get_totals("event",label="All event types",df=avg_evnt) %>% 
    bind_rows(avg_evnt)
  
  
## Merge FYC and Event stats    
   use_tables <- 
     join_all(list(all_fyc,all_evnt,avg_evnt),
              by=c("grp1","grp2","levels1","levels2", "Year")) %>%
     as.data.frame

   use_tables <- use_tables %>% 
     filter(!levels1 %in% c("HHA","HHN"),
            !levels2 %in% c("HHA","HHN"))
   
   use_tables %>% filter(is.na(totPOP)) 
   use_tables %>% filter(is.na(n) & totPOP > 0) 
   use_tables %>% filter(is.na(totEVT) & pctEXP > 0)
   use_tables %>% filter(is.na(avgEVT))
   
   save(use_tables, file="USE_TABLES.Rdata")
   
#load("USE_TABLES_backup.Rdata")

   #new = use_tables
   
   #old = use_tables
   
   
   head(new)
   head(old)
   