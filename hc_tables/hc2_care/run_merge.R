## MERGE CARE TABLES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../run_preamble.R",chdir=T) # loads run_functions.R too

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

  save(care_tables, file="tables/TABLES.Rdata")

  
  
##### Compare new to old Care files -- do for all of them
setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables/hc2_care/r/tables")
list.files("2014")

file = "adult_explain.csv"

for(file in list.files("2014_copy")){

file1 = read.csv(sprintf("2014/%s",file),stringsAsFactors = F)
file2 = read.csv(sprintf("2014_copy/%s",file),stringsAsFactors = F)

both <- full_join(file2,file1,by=c("grp1","grp2","levels1","levels2"))

print(both %>% filter(pctPOP.x != pctPOP.y | pctPOP_se.x != pctPOP_se.y))
}




