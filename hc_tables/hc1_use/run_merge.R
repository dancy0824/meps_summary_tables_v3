
library(dplyr)

evnt_keys <-
  list("DV"="DVT","ER"="ERT","HH"="HHT","IP"="IPT",
       "OB"="OBV","OM"="OMA","OP"="OPT") %>% stack

event_dictionary <- 
  list("TOT"="All event types",
       "DVT"="Dental visits",
       "RX" ="Prescription medicines",
       "OBV"="Office-based events",
       "OBD"="Physician Office visits",
       "OBO"="Non-physician office visits",
       "OPT"="Outpatient events",
       "OPY"="Physician hosp. visits",
       "OPZ"="Non-physician hosp. visits",
       "ERT"="Emergency room visits",
       "IPT"="Inpatient stays",
       "HHT"="Home health events",
       "OMA"="Other medical expenses") %>% stack

sop_dictionary <- 
  list("EXP"="All sources",
       "SLF"="Out of pocket",
       "PTR"="Private",
       "MCR"="Medicare",
       "MCD"="Medicaid",
       "OTZ"="Other") %>% stack

# total_dictionary <- list(
#   "event"         = "All event types",
#   "agegrps"       = "All age groups",
#   "region"        = "All regions",
#   "married"       = "All marital statuses",
#   "race"          = "All races/ethnicities",
#   "sex"           = "All persons",
#   "insurance"     = "All coverage statuses",
#   "health"        = "All health statuses",
#   "mental_health" = "All mental health statuses",
#   "education"     = "All education levels",
#   "employed"      = "All employment statuses",
#   "poverty"       = "All poverty levels"
# ) %>% stack # framify(ncol=2,names=c("total","label"))
# 
# total_grps <- total_dictionary$ind

#####################################################################################

join_all <- function(df_list,...){
  out <- df_list[[1]]
  for(df in df_list[-1]) out <- suppressWarnings(full_join(out,df,...))
  out
}

reverse <- function(df) df[nrow(df):1,]

dedup <- function(df){
  df %>% 
    reverse %>% 
    distinct(Year,grp1,grp2,levels1,levels2,.keep_all=TRUE) %>% 
    reverse
}

rm_v2 <- function(df){
  df%>% mutate(grp1 = grp1 %>% gsub("_v2X","",.),
               grp2 = grp2 %>% gsub("_v2X","",.))
}


# Add event and SOP labels
add_labels <- function(df,dictionary, key="ind",vars=c("levels1","levels2")){
  for(var in vars){
    df <- df %>%
      mutate_(temp=var) %>%
      left_join(dictionary,by=c("temp"=key)) %>%
      mutate(temp = coalesce(values,temp))
    df[,var] = df$temp
    df <- df %>% select(-temp,-values)
  }
  return(df)
}

switch_labels <- function(df){
  df %>% 
    mutate(g1=grp1,g2=grp2,l1=levels1,l2=levels2) %>%
    mutate(grp1=g2,grp2=g1,levels1=l2,levels2=l1) %>%
    select(-g1,-g2,-l1,-l2)
}

get_totals <- function(grp,df,label="All persons"){
  totals <- df %>% filter(grp1=="ind",grp2!=grp)
  totals %>%
    mutate(grp1=grp,levels1=label) %>%
    switch_labels
}

##################################################################

## MERGE FYC AND EVENT TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/global.R",chdir=T)
source("global.R",chdir=T) 

years = 2014:1996

load_stats <- function(stat,year){
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  read.csv(file,stringsAsFactors = F) 
}


### FYC FILES ###
  fyc <- list()
  for(year in years){ cat(year,"..")
    fyc[[as.character(year)]] <- 
      lapply(c(fyc_stats,"n","n_exp"),load_stats,year=year) %>%
      join_all(by=c("grp1","grp2","levels1","levels2")) %>%
      mutate(Year = year)
  }

  all_fyc <- bind_rows(fyc) %>% rm_v2 %>% dedup

  all_fyc <- all_fyc %>% 
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary)

### EVENT FILES ###
  evnt <- list()
  for(year in years){ cat(year,"..")
    evnt[[as.character(year)]] <- 
      lapply(evnt_stats,load_stats,year=year) %>%
      join_all(by=c("grp1","grp2","levels1","levels2")) %>%
      mutate(Year = year)
  }
  
  all_evnt <- bind_rows(evnt) %>% rm_v2 %>% dedup 
  
  all_evnt <- all_evnt %>%
    filter(!(levels2=="Missing" & grp2=="event")) %>%  # for event_v2, 'missing' is all events with no sub-types
    add_labels(evnt_keys) %>%
    add_labels(event_dictionary) %>%
    add_labels(sop_dictionary) 
  
  # Add 'all event types' to match FYC
  all_evnt <- get_totals("event",label="All event types",df=all_evnt) %>% 
    bind_rows(all_evnt)
  
 

## Merge FYC and Event stats    
   use_tables <- full_join(all_fyc,all_evnt,by=c("grp1","grp2","levels1","levels2", "Year"))

   # use_tables %>% filter(is.na(totPOP))
   # use_tables %>% filter(is.na(totEVT))
   # use_tables %>% filter(is.na(totEVT)&!is.na(meanEXP))
   
   save(use_tables, file="USE_TABLES.Rdata")
   
#load("USE_TABLES_backup.Rdata")

   
   
   
   