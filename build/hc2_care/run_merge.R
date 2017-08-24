## MERGE CARE TABLES

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

print("Merging data")
Sys.sleep(1)

app <- "hc2_care"

#source("../shared/app_preamble.R",chdir=F)
#source("../../mepstrends/hc_tables/hc2_care/dictionaries.R")
source("../run_preamble.R",chdir=T)


care_grps <- care_subgrps %>% unlist

load_stats <- function(stat,year){
  file = sprintf("tables/%s/%s.csv",year,stat)
  df <- read.csv(file,stringsAsFactors = F) 
  df %>% rm_v2 #%>% reorder_cols
}

delay_dictionary = list(
  "delay_ANY" = "Any care",
  "delay_MD" = "Medical care",
  "delay_DN" = "Dental care",
  "delay_PM" = "Prescription medicines") %>% stack

########################################################

all_years = list.files("tables") %>% as.numeric %>% na.omit

if("TABLES.Rdata" %in% list.files("../../mepstrends/hc_tables/hc2_care")){
  load("../../mepstrends/hc_tables/hc2_care/TABLES.Rdata")
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
  
  freq_levels <- c(
    "9-10 rating","7-8 rating","0-6 rating",
    "Don't know/Non-response",
    "Not ascertained",
    "Inapplicable",
    "Missing")
  
  new_tables <- new_tables %>% 
    reorder_levels(age_levels) %>% 
    reorder_levels(freq_levels)
  
  care_tables <- bind_rows(new_tables,care_tables) # Don't order by year -- will mess up levels for diab_foot
  
  save(care_tables, file="../../mepstrends/hc_tables/hc2_care/TABLES.Rdata")
  
  print("Care tables updated")
  Sys.sleep(1)
}
  



