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


join_all <- function(df_list,...){
  out <- df_list[[1]]
  for(df in df_list[-1]) out <- suppressWarnings(full_join(out,df,...))
  out
}

reverse <- function(df) df[nrow(df):1,]

dedup <- function(df){
  df %>% 
    reverse %>% 
    distinct(Year,grp1,grp2,levels1,levels2,.keep_all=TRUE) %>% 
    reverse
}

rm_v2 <- function(df){
  df%>% mutate(grp1 = grp1 %>% gsub("_v2X","",.),
               grp2 = grp2 %>% gsub("_v2X","",.))
}


# Add event and SOP labels
add_labels <- function(df,dictionary, key="ind",vars=c("levels1","levels2")){
  for(var in vars){
    df <- df %>%
      mutate_(temp=var) %>%
      left_join(dictionary,by=c("temp"=key)) %>%
      mutate(temp = coalesce(values,temp))
    df[,var] = df$temp
    df <- df %>% select(-temp,-values)
  }
  return(df)
}

switch_labels <- function(df){
  df %>% 
    mutate(g1=grp1,g2=grp2,l1=levels1,l2=levels2) %>%
    mutate(grp1=g2,grp2=g1,levels1=l2,levels2=l1) %>%
    select(-g1,-g2,-l1,-l2)
}

get_totals <- function(grp,df,label="All persons"){
  totals <- df %>% filter(grp1=="ind",grp2!=grp)
  totals %>%
    mutate(grp1=grp,levels1=label) %>%
    switch_labels
}


is_ordered <- function(v1,v2) mapply(function(x1,x2) sort(c(x1,x2))[1]==x1,v1,v2)

reorder_cols <- function(df){
  
  df2 <- df %>% 
    mutate(ordered = is_ordered(grp1,grp2),
           ordered = replace(ordered,grp1=='ind',TRUE))
  
  Ordered <- df2 %>% filter(ordered)
  Unordered <- df2 %>% filter(!ordered) %>% switch_labels
  
  bind_rows(Ordered,Unordered) %>% select(-ordered)
}

