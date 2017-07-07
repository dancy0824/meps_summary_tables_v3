library(dplyr)

setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables/shared/PUFS")

rm_empty <- function(vec) vec[vec!=""]

downloadSSP <- function(filename){
  file.ssp = paste0(filename,".ssp")
  file.alt = file.ssp %>% sub("h","hc",.)
  all_files = tolower(list.files())
  if(any(c(file.ssp,file.alt) %in% all_files)) return(sprintf("File %s already loaded",filename))
  
  url = sprintf('https://meps.ahrq.gov/mepsweb/data_files/pufs/%sssp.zip',filename)
  download.file(url, temp <- tempfile())
  unzipped_file = unzip(temp)
  unlink(temp)
}

################################################

# Load MEPS names from csv
meps_names <- read.csv("../puf_names.csv",stringsAsFactors = F)

meps_names_fyc <- meps_names %>% select(FYC) %>% rm_empty
meps_names_RX <- meps_names %>% select(RX.Events) %>% rm_empty
meps_names_evnt <- meps_names %>% select(Events) %>% rm_empty

lapply(meps_names_fyc,downloadSSP)
lapply(meps_names_RX,downloadSSP)

for(letter in letters[2:8]){
  event_files <- gsub("\\*",letter,meps_names_evnt)
  lapply(event_files,downloadSSP)
}
