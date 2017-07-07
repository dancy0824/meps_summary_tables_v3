
library(dplyr)

join_all <- function(df_list,...){
  out <- df_list[[1]]
  for(df in df_list[-1]) out <- suppressWarnings(full_join(out,df,...))
  out
}

# Converting key values to labels based on dictionaries
add_labels <- function(df,dictionary, key="value",var="levels1"){
    df <- df %>%
      mutate_(temp=var) %>%
      left_join(dictionary,by=c("temp"=key)) %>%
      mutate(temp = coalesce(label,temp))
    df[,var] = df$temp
    df %>% select(-temp,-one_of(names(dictionary)))
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

#######################################################

##################################################################

## MERGE FYC AND EVENT TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/r/run_global.R",chdir=T) # loads functions.R too

years = 2014:1996

evt_stats = c("totEVT","meanEVT")
fyc_stats = c("totPOP","pctEXP","totEXP",
              "meanEXP0","meanEXP","medEXP",
              "n","n_exp")

load_stats <- function(stat,year){
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  read.csv(file,stringsAsFactors = F) 
}


### FYC FILES ###
  fyc <- list()
  for(year in years){ cat(year,"..")
    fyc[[as.character(year)]] <- 
      lapply(fyc_stats,load_stats,year=year) %>%
      join_all(by=c("grp1","grp2","levels1","levels2")) %>%
      mutate(Year = year)
  }

  all_fyc <- bind_rows(fyc) %>% rm_v2 %>% dedup
  all_fyc <- all_fyc %>% 
    add_labels(event_dictionary,key="event",var="levels1") %>%
    add_labels(event_dictionary,key="event",var="levels2") %>%
    add_labels(sop_dictionary, key="sop",var="levels1") %>%
    add_labels(sop_dictionary, key="sop",var="levels2")
  
### EVENT FILES ###
  evnt <- list()
  for(year in years){ cat(year,"..")
    evnt[[as.character(year)]] <- 
      lapply(evt_stats,load_stats,year=year) %>%
      join_all(by=c("grp1","grp2","levels1","levels2")) %>%
      mutate(Year = year)
  }
  
  all_evnt <- bind_rows(evnt) %>% rm_v2 %>% dedup 
  
  all_evnt <- all_evnt %>%
    filter(!(levels2=="Missing" & grp2=="event")) %>%  # for event_v2, 'missing' is all events with no sub-types
    add_labels(event_dictionary,key="evnt",var="levels1") %>%
    add_labels(event_dictionary,key="evnt",var="levels2") %>%
    add_labels(sop_dictionary, key="sp",var="levels1") %>%
    add_labels(sop_dictionary, key="sp",var="levels2")
  

## Merge FYC and Event stats    
  use_tables <- full_join(all_fyc,all_evnt)
  save(use_tables , file="USE_TABLES.Rdata")
   

#   
#   
# ### Maybe don't do this....  
#   
#   total_dictionary <- c(
#     "ind"           ,"Total",
#     "event"         , "All Event Types",
#     "sop"           , "All Sources",
#     "agegrps"       , "All Age Groups",
#     "region"        , "All Regions",
#     "married"       , "All Marital Statuses",
#     "race"          , "All Races/Ethnicities",
#     "sex"           , "All Persons",
#     "insurance"     , "All Coverage Statuses",
#     "health"        , "All Health Statuses",
#     "mental_health" , "All Mental Health Statuses",
#     "education"     , "All Education Levels",
#     "employed"      , "All Employment Statuses",
#     "poverty"       , "All Poverty Levels"
#   ) %>% framify(ncol=2,names=c("total","label"))
#   ### Add 'all people' for groups 
#   
#   get_totals <- function(grp,df){
#     totals <- df %>% filter(grp1=="ind") 
#     totals %>% 
#       mutate(grp1=grp) %>% 
#       left_join(total_dictionary,by=c("grp1"="total")) %>%
#       mutate(levels1 = coalesce(label,levels1)) 
#   }
#   
#   tot_grps <- subgrps[!subgrps%in%c("ind","agevar")]
#   totals_df <- lapply(tot_grps,get_totals,df=use_tables) %>% bind_rows
#   
#   use_tables <- use_tables %>% bind_rows(totals_df)
  