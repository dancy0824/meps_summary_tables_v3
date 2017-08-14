## MERGE FYC AND EVENT TABLES

print("Merging data")
Sys.sleep(1)

app <- 'hc1_use'

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/app_preamble.R",chdir=F)
source("../shared/r/run_preamble.R",chdir=T)
source("dictionaries.R",chdir=T)

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

all_years = list.files("r/tables") %>% as.numeric %>% na.omit

if("TABLES.Rdata" %in% list.files()){
  load("TABLES.Rdata")
  done_years = use_tables$Year %>% unique
}else{
  done_years = NULL
  use_tables = NULL
}

years = all_years[!all_years%in%done_years]

print("years is:")
print(years); Sys.sleep(1);

if(length(years) == 0){
  
  print("EVERYTHING UP-TO-DATE")
  Sys.sleep(1)
  
}else{
  
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
     new_tables <-
       full_join(all_fyc,all_evnt,by=c("grp1","grp2","levels1","levels2", "Year")) %>%
       as.data.frame
  
     new_tables <- new_tables %>%
       filter(!levels1 %in% c("HHA","HHN"),
              !levels2 %in% c("HHA","HHN"))
  
     new_tables <- new_tables %>%
       mutate(levels1 = replace(levels1,levels1=="Negative or Poor","Negative or poor"),
              levels2 = replace(levels2,levels2=="Negative or Poor","Negative or poor"))
  
     new_tables <- new_tables %>% reorder_levels(age_levels)
  
    # QC
     new_tables %>% filter(is.na(totPOP)) %>% head(100) %>% print
     new_tables %>% filter(is.na(n) & totPOP > 0)  %>% head(100) %>% print
     new_tables %>% filter(is.na(totEVT) & pctEXP > 0) %>% head(100) %>% print
     new_tables %>% filter(is.na(avgEVT)) %>% head(100) %>% print
  
     use_tables <- bind_rows(new_tables,use_tables) %>% arrange(-Year)
     
     save(use_tables, file="TABLES.Rdata")
     
     print("Use tables updated")
     Sys.sleep(1)
}




