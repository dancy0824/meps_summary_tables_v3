
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../run_preamble.R",chdir=T) # shared


year = 1998

RXs <- list()
for(year in 1996:2014){
  
  yr = substring(year,3,4)
  this_year = meps_names %>% filter(Year==year) 
  file_names = as.list(this_year$url) %>% setNames(this_year$file_type)
  
  rxName = file_names$RX
  
  temp <- downloadMEPS(rxName)
  
  RXs[[paste0('year',year)]] <- temp
  
  print(temp %>% head)
  
  rm(temp)
  
  #RX %>% select(RXNDC,RXNAME) %>% head(30)
  
  #  filter(grepl("LANTUS",RXNAME)) %>% head
}

# 1996-2012: RXNAME, RXNDC
# 2013-2014: RXNAME, RXNDC, RXDRGNAM

RX2012 = RXs[["year2012"]]

RX2012 %>% select(RXNAME,RXNDC) %>% 
  filter(grepl("ADVAIR",RXNAME)) %>%
  head(10)


RX2014 = RXs[["year2014"]]

RX2014 %>% select(RXNAME,RXNDC, RXDRGNAM) %>%
  filter(grepl("ESOMEP",RXNAME)) %>%
  head(10)


RX2014 %>% select(RXNAME,RXNDC, RXDRGNAM) %>%
  filter(grepl("LIPITOR",RXNAME)) %>%
  head(10)
