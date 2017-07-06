library(foreign)
library(survey)
library(dplyr)
library(tidyr)

source("../functions.R")


meps_names = read.csv("../puf_expanded.csv",stringsAsFactors = F)


subgrps = c("ind", "agevar",
            "agegrps", "region", "married", "race", "sex", # Demographics
            "insurance", "health", "mental_health",        # Health Variables
            "education", "employed", "poverty")            # Socio-economic status

subgrp_list = c(subgrps,"insurance_v2X","agegrps_v2X")
subgrp_list = subgrp_list[subgrp_list!='agevar']
sl = length(subgrp_list)

#########################################################
##                      FUNCTIONS                      ##
#########################################################

get_file_names <- function(year){
 meps_names %>% 
   filter(Year==year) %>%
   select(-Year,-ends_with('Panel'))
}

readSource <- function(file,...,dir=".",verbose=T){
  fileName <- sprintf("%s/%s",dir,file) %>% gsub("//","/",.)
  codeString <- readChar(fileName,file.info(fileName)$size)
  codeString <- codeString %>% rsub(...) %>% gsub("\r","",.)
  
  if(verbose) codeString %>% writeLines
  codeString
}

run <- function(codeString){
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
# 

# 
# switch_labels <- function(df){
#   names(df)[names(df) %in% 
#     c("grp1","grp2","levels1","levels2")] <- 
#     c("grp2","grp1","levels2","levels1")
#   df
# }

# is.done <- function(file,check){   
#   file_exists = file %in% list.files(recursive=T)
#   if(!file_exists | length(check)==0 ) return(FALSE)
#   df <- read.csv(file,stringsAsFactors = F)
#   
#   for(i in 1:length(check)){
#     name  = names(check)[i]
#     value = check[i]
#     df <- df %>% filter_( sprintf("%s=='%s'",name,value) )
#   }
#   return(nrow(df)>0)
# }


# 
# run <- function(FUN,file,force=F,...){
#   
#   extras <- list(...)
#   
#   extras$levels1 <- extras[[extras$grp1]]
#   extras$levels2 <- extras[[extras$grp2]]
#   
#   chk = extras[names(extras) %in% c("grp1","grp2","levels1","levels2")]
#   
#   if(force | !is.done(file,check=chk)){
#     tab <- FUN(...)
#     update.csv(tab,file_name=file)
#     return(tab)
#   }
#   return("skipped")
# }
# 
# std <- function(df,key,addSE=T){
#   df <- df %>% select(-contains("FALSE"))
#   cc <- length(names(df))
#   
#   if(addSE) key=c(key,paste0(key,"_se"))
#   names(df)[(cc-1):cc] = key
#   
#   df
# }
# 
# switch_labels <- function(df){
#   names(df)[names(df) %in% 
#     c("grp1","grp2","levels1","levels2")] <- 
#     c("grp2","grp1","levels2","levels1")
#   df
# }

