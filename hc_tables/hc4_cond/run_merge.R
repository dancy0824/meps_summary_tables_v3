## MERGE CONDITION TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc4_cond"

source("../shared/app_global.R",chdir=T)
source("../shared/r/run_preamble.R",chdir=T)
source("dictionaries.R")

library(dplyr)

########################################################

load_stats <- function(stat,year){
  file = sprintf("r/tables/%s/%s.csv",year,stat)
  df <- read.csv(file,stringsAsFactors = F) 
  df %>% rm_v2 #%>% reorder_cols
}


years = 2003:2014


out <- list()
for(year in years){                 cat(year,"..")
  out[[as.character(year)]] <- 
    lapply(cond_stats,load_stats,year=year) %>%
    join_all(by=c("grp1","grp2","levels1","levels2")) %>%
    mutate(Year = year)
}

cond_tables <- bind_rows(out) %>% dedup

### Fake sample size for now

cond_tables$n = 10000
cond_tables$n_exp = 10000


save(cond_tables, file="TABLES.Rdata")


