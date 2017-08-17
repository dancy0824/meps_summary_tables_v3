suppressMessages(library(tidyr))
suppressMessages(library(dplyr))

source("../mepstrends/hc_tables/shared/app_global.R")
source(sprintf("../mepstrends/hc_tables/%s/dictionaries.R",app))
source(sprintf("../mepstrends/hc_tables/%s/app_code.R",app),chdir=T)

shared = "../../mepstrends/hc_tables/shared/r"
path = sprintf("../../mepstrends/hc_tables/%s/r",app)

meps_names <- read.csv("../mepstrends/hc_tables/shared/puf_expanded.csv",stringsAsFactors=F)

year_list <- meps_names$Year[meps_names$FYC!=""]

subgrps <- unlist(subgrps)
subgrp_load <- c("agevar",subgrps)
subgrp_list  <- c(subgrps,"insurance_v2X","agegrps_v2X")

sl = length(subgrp_list)

age_levels = c("Under 18","Under 5","5-17","18-64","18-44","45-64","65+")

#########################################################
##                      FUNCTIONS                      ##
#########################################################

rm_empty <- function(vec) vec[vec!=""]

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

reorder_levels <- function(df,new_levels){
  orig_l1 = unique(df$levels1)
  orig_l2 = unique(df$levels2)
  
  new_l1 = c(orig_l1[!orig_l1 %in% new_levels],new_levels)
  new_l2 = c(orig_l2[!orig_l2 %in% new_levels],new_levels)
  
  df %>% 
    mutate(levels1 = factor(levels1,levels=new_l1),
           levels2 = factor(levels2,levels=new_l2)) %>%
    arrange(-Year,levels1,levels2) %>%
    mutate(levels1 = as.character(levels1),
           levels2 = as.character(levels2))
}
