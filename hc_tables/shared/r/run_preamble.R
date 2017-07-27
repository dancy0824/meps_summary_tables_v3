library(tidyr)
library(dplyr)

dir = "C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables"
shared = paste0(dir,"/shared/r")
PUFdir = sprintf("%s/shared/PUFS",dir) 
path   = sprintf("%s/%s/r",dir,app)
tables = sprintf("%s/tables",path) 

meps_names <-
  read.csv(sprintf("%s/shared/puf_expanded.csv",dir),
           stringsAsFactors=F)

no_miss = meps_names %>% filter(FYC != "") %>% arrange(-Year) 
year_list = no_miss$Year

subgrps <- unlist(subgrps)
subgrp_load <- c("agevar",subgrps)
subgrp_list  <- c(subgrps,"insurance_v2X","agegrps_v2X")

sl = length(subgrp_list)

#########################################################
##                      FUNCTIONS                      ##
#########################################################


run <- function(codeString,verbose=T){
  if(verbose) writeLines(codeString)
  eval(parse(text=codeString),envir=.GlobalEnv)
}

runSource <- function(file,...){
  codeString <- readSource(file,...) %>% run
}

update.csv <- function(add,file,dir){
  init = !(file %in% list.files(dir,recursive=T))
  fileName <- sprintf("%s/%s",dir,file) %>% gsub("//","/",.)
  write.table(add,file=fileName,append=(!init),sep=",",col.names=init,row.names=F)
}

done <- function(outfile,...,dir="/"){
  if(!outfile %in% list.files(dir,recursive=T)) return(FALSE)
  
  df <- read.csv(paste0(dir,"/",outfile))
  chk <- list(...)
  for(i in 1:length(chk)){
    name=names(chk)[i]
    value=chk[[i]]
    df <- df %>% filter_(sprintf("%s=='%s'",name,value))
  }
  is.done = (nrow(df)>0)
  if(is.done) print('skipping')
  return(is.done)
}





# 
# 
# ########################################################
# ## Functions to remove duplicates from top 
# ## (since adding updated estimates to bottom of files) 
# 
# reverse <- function(df){
#   df[nrow(df):1,]
# }
# switch_labels <- function(df){
#   names(df)[names(df) %in% 
#     c("grp1","grp2","levels1","levels2")] <- 
#     c("grp2","grp1","levels2","levels1")
#   df
# }

