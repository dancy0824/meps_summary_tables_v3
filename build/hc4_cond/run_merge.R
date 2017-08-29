## MERGE CONDITION TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc4_cond"

source("../run_preamble.R",chdir=T)

library(dplyr)

########################################################

load_stats <- function(stat,year){
  file = sprintf("tables/%s/%s.csv",year,stat)
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


### Add checkboxes to conditions labels -- using 'cond' namespace
### Need these for 508 compliance keyboard navigation with DT

# label = cond_tables$levels2
# id = paste0("cond-cols-levels-",label)
# 
# cond_tables$levels2 <- sprintf(
#     '<input id="%s" type="checkbox" name="%s" value="%s" class="shiny-input-container"/><label for="%s">%s</label>',
#     id,id,id,id,label
# )


### Fake sample size for now

cond_tables$n = 10000
cond_tables$n_exp = 10000

save(cond_tables, file="../../mepstrends/hc_tables/hc4_cond/TABLES.Rdata")
