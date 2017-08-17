################################################################
## Update list of MEPS files and create expanded version

print("initiating update.R...")
Sys.sleep(1)

suppressMessages(library(dplyr))

meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

puf_names_current = read.csv(meps_file,stringsAsFactors = F)

puf_names <- puf_names_current %>% 
  mutate(Year = suppressWarnings(as.numeric(Year))) %>% 
  filter(!is.na(Year)) 

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.csv(puf_names,file="../mepstrends/hc_tables/shared/puf_names.csv",row.names=F) # for file_transfer.R

################################################################
## Expand puf_names.csv for easy access in run files

event_letters <- list(DV="b",OM="c",IP="d",ER="e",OP="f",OB="g",HH="h")
hc_list <- c("h10a","h26bf1",
             sprintf("h16%sf1",letters[2:8]),
             sprintf("h10%sf1",letters[2:8]))

puf_expanded <- puf_names %>% rename(RX=RX.Events)

puf_expanded <- puf_expanded %>% 
  mutate(RX = replace(RX,RX=="h10a","hc10a"),
         CLNK = replace(CLNK,CLNK=="h10if1","hc10if1"),
         Conditions = replace(Conditions,Conditions=="h06r","hc006r"))

for(evnt in names(event_letters)){
  letter = event_letters[[evnt]]
  value = puf_expanded$Events %>% gsub("\\*",letter,.)
  special = value %in% hc_list
  value[special] = value[special] %>% sub("h","hc",.)
  puf_expanded[,evnt] = value
}

write.csv(puf_expanded,file="../mepstrends/hc_tables/shared/puf_expanded.csv",row.names=F)

print("...Completed")
Sys.sleep(1)