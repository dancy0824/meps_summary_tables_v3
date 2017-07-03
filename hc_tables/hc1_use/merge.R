# Convert vector to dataframe
framify <- function(vec,ncol=1,names=NULL){
  vec %>% matrix(ncol=ncol,byrow=T) %>%
    as.data.frame(stringsAsFactors=F) %>%
    setNames(names)
}

fyc_stats  = c("totPOP","pctEXP","totEXP","meanEXP0","meanEXP","medEXP","n","n_exp")
evnt_stats = c("totEVT","meanEVT")

event_dictionary = c(
  "TOT",   "ind", "All event types",
  "DVT",   "DV",  "Dental visits",
  "RX",    "RX",  "Prescription medicines",
  "OBV",   "OB",  "Office-based events",
  "OBD",   "OBD", "Physician office visits",
  "OBO",   "OBO", "Non-physician office visits",
  "OPT",   "OP",  "Outpatient events",
  "OPY",   "OPY", "Physician hosp. visits",
  "OPZ",   "OPZ", "Non-physician hosp. visits",
  "ERT",   "ER",  "Emergency room visits",
  "IPT",   "IP",  "Inpatient stays",
  "HHT",   "HH",  "Home health events",
  "HHA",   "HHA", "Home health agency",
  "HHN",   "HHN", "Home health independent",
  "OMA",   "OM",  "Other medical expenses",
  "Missing","Missing","Missing"
) %>%
  framify(ncol=3,names=c("event","use","label")) 

#event_list = event_dictionary$event
#use_list = event_dictionary$use %>% setNames(event_dictionary$event)


sop_dictionary = c(
  "EXP", "XP", "All sources",
  "SLF", "SF", "Out of Pocket",
  "PTR", "PR", "Private",
  "MCR", "MR", "Medicare" ,
  "MCD", "MD", "Medicaid" ,
  "OTZ", "OZ", "Other"
) %>% 
  framify(ncol=3,names=c("sop","sp","label"))

sop_list = sop_dictionary$sop
sp_list  = sop_dictionary$sp %>% setNames(sop_dictionary$sop)

##################################################################

## MERGE FYC AND EVENT TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../run_preamble.R",chdir=T) # loads run_functions.R too
source("use_dictionaries.R")

### FYC FILES ###

# add an 'all people' category for each group + add event labels to FYC

  fyc <- list()
  for(year in yearlist){ cat(year,"..")
    filename  = sprintf("tables/FYC%s.csv",year)
    filenameX = sprintf("tables/edited/FYC%sX.csv",year)
    df <- read.csv(filename,stringsAsFactors = F) %>% mutate(Year=year) 
    df <- df %>% rm_v2 %>% dedup
    
    fyc[[as.character(year)]] <- df
    
    write.csv(df,filenameX,row.names=F)
    rm(df)
  }
  all_fyc <- bind_rows(fyc) #%>% rm_v2 %>% dedup
  
  all_fyc <- all_fyc %>% 
    add_labels(event_dictionary,key="event") %>%
    add_labels(sop_dictionary, key="sop")
  
  
  
### EVENT FILES ###
  evnt <- list()
  for(year in yearlist){ cat(year,"..")
    filename  = sprintf("tables/EVT%s.csv",year)
    filenameX = sprintf("tables/edited/EVT%sX.csv",year)
    df <- read.csv(filename,stringsAsFactors = F) %>% mutate(Year=year)
    df <- df %>% rm_v2 %>% dedup
    
    evnt[[as.character(year)]] <- df
    
    write.csv(df,filenameX,row.names=F)
    rm(df)
  }
  all_evnt <- bind_rows(evnt) #%>% rm_v2 %>% dedup 
  
  all_evnt <- all_evnt %>%
    filter(!(levels2=="Missing" & grp2=="event")) %>%  # for event_v2, 'missing' is all events with no sub-types
    add_labels(event_dictionary,key="event") %>%
    add_labels(sop_dictionary, key="sp")
  
  use_tables <- full_join(all_fyc,all_evnt)
  

  
  # QC -- still need to add 'all events, etc.' to make this fit
  use_tables %>% filter(is.na(totPOP)) %>% head(30)
  use_tables %>% filter(is.na(totEVT),n > 10, levels2!="All event types") %>% head(30)
  
  save(use_tables , file="tables/USE_TABLES.Rdata")
  # 
  
