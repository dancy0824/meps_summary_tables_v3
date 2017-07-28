## MERGE CONDITION TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../shared/global.R",chdir=T)
#source("global.R",chdir=T) 
#source("run_global.R")

########################################################

delay_dictionary = c(
  "delay_ANY", "Any care",
  "delay_MD", "Medical care",
  "delay_DN", "Dental care",
  "delay_PM", "Prescription medicines"
) %>%
  framify(ncol=2,names=c("value","label")) 


### CARE FILES ###
care <- list()
for(year in yearlist){ cat(year,"..")
  filename  = sprintf("tables/CARE%s.csv",year)
  filenameX = sprintf("tables/edited/CARE%sX.csv",year)
  df <- read.csv(filename,stringsAsFactors = F) %>% mutate(Year=year) 
  df <- df %>% rm_v2 %>% dedup
  
  care[[as.character(year)]] <- df
  
  write.csv(df,filenameX,row.names=F)
  rm(df)
}
all_care <- bind_rows(care) #%>% rm_v2 %>% dedup

all_care <- all_care %>% 
  mutate(
    levels2 = replace(levels2,startsWith(levels2,"afford"),"Couldn't afford"),
    levels2 = replace(levels2,startsWith(levels2,"insure"),"Insurance related"),
    levels2 = replace(levels2,startsWith(levels2,"other"), "Other")) 

care_tables <- all_care  %>% add_labels(delay_dictionary,key="value")

save(care_tables, file="tables/CARE_TABLES.Rdata")


