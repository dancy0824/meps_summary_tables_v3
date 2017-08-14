## MERGE CARE TABLES

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

print("Merging data")
Sys.sleep(1)

app <- "hc2_care"

source("../shared/app_preamble.R",chdir=F)
source("../shared/r/run_preamble.R",chdir=T)
source("dictionaries.R")

care_grps <- care_subgrps %>% unlist

load_stats <- function(stat,year){
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  df <- read.csv(file,stringsAsFactors = F) 
  df %>% rm_v2 #%>% reorder_cols
}

delay_dictionary = list(
  "delay_ANY" = "Any care",
  "delay_MD" = "Medical care",
  "delay_DN" = "Dental care",
  "delay_PM" = "Prescription medicines") %>% stack

########################################################

all_years = list.files("r/tables") %>% as.numeric %>% na.omit

if("TABLES.Rdata" %in% list.files()){
  load("TABLES.Rdata")
  done_years = care_tables$Year %>% unique
}else{
  done_years = NULL
  care_tables = NULL
}

years = all_years[!all_years%in%done_years]

print("years is:")
print(years); Sys.sleep(1);

if(length(years) == 0){
  
  print("EVERYTHING UP-TO-DATE")
  Sys.sleep(1)
  
}else{
    
  ### CARE FILES ###
  out <- list()
  for(year in years){                 cat(year,"..")
    out[[as.character(year)]] <- 
      lapply(care_grps,load_stats,year=year) %>% bind_rows %>%  mutate(Year = year)
  }
  all_care <- bind_rows(out)
  
  all_care <- all_care %>% 
    mutate(
      levels2 = replace(levels2,startsWith(levels2,"afford"),"Couldn't afford"),
      levels2 = replace(levels2,startsWith(levels2,"insure"),"Insurance related"),
      levels2 = replace(levels2,startsWith(levels2,"other"), "Other")) 
  
  new_tables <- all_care %>% 
    add_labels(delay_dictionary) %>% 
    mutate(n=counts) %>%
    select(-counts)
  
  new_tables <- new_tables %>% reorder_levels(age_levels)
  
  care_tables <- bind_rows(new_tables,care_tables) %>% arrange(-Year)
  
  save(care_tables, file="TABLES.Rdata")
  
  print("Care tables updated")
  Sys.sleep(1)
}
  



