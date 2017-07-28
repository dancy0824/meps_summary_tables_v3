## MERGE CARE TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc2_care"

source("../shared/app_global.R",chdir=T)
source("../shared/r/run_preamble.R",chdir=T)
source("dictionaries.R")

library(dplyr)

########################################################
 
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

care_tables <- all_care %>% add_labels(delay_dictionary)

save(care_tables, file="TABLES.Rdata")





