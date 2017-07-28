##################################################
###            MEDICAL CONDITIONS              ###
##################################################

gather_sop <- function(df){
  df %>% 
    gather(group,coef,-Condition) %>%
    mutate(group = gsub(" > 0TRUE","",group)) %>%
    separate(group,c("stat","sop"),sep="\\.",fill="left") %>%
    mutate(stat = replace(stat,is.na(stat),"stat")) %>%
    spread(stat,coef) %>%
    mutate(sop = substr(sop,1,2))
}

standardize <- function(df,grp1,grp2,stat,type=""){
  out <- df %>% select(-contains("FALSE")) 
  
  if(type=="sop") out <- gather_sop(out)
  
  key = c(stat,paste0(stat,"_se"))
  names(out)[!names(out) %in% c(grp1,grp2)] = key
  
  out %>%
    mutate(grp1=grp1,grp2=grp2) %>%
    mutate_(levels1=grp1,levels2=grp2) %>%
    select(grp1,grp2,levels1,levels2,one_of(key))
}

##################################################
###                  LISTS                     ###
##################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc4_cond"

# Shared
source("../../shared/global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
source("../dictionaries.R")
source("stats.R")

##################################################
###                   RUN                      ###
##################################################

# Load packages
runSource('load/load_pkg.R',dir=shared)

args = commandArgs(trailingOnly = TRUE)

if(length(args) > 0){
  year_start = as.numeric(args[1])
  year_end = as.numeric(args[2])
}else{
  year_start = min(meps_names$Year)
  year_end = max(meps_names$Year)
}

year_list = year_start:year_end

# year = 2013
# year_list = 2014:1996

for(year in year_list){ 

  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 
  sg = paste0(subgrp_list,",",collapse="")
  
  # Load data
  runSource('load/load_fyc.R', dir=shared,year=year,yy=yr,PUFdir=PUFdir,get_file_names(year))
  
  # Add subgroups 
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,"yy"=yr)
  
  # Load EVENTS and merge with FYC 
  runSource('load_cond.R',yy=yr,PUFdir=PUFdir,subgrps=sg, get_file_names(year))
  
  # Define survey designs
  runSource("design/EVNTdsgn.R",yy=yr) # event-level
  runSource("design/PERSdsgn.R",yy=yr,subgrps=sg) # person-level
  runSource("design/PERSevnt.R",yy=yr,subgrps=sg) # person-level, by event type

  for(stat in cond_stats){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
  
    # by demographic subgroups
      for(grp1 in subgrp_list){
        if(done(outfile,dir=tables,grp1=grp1,grp2="Condition")) next
        by = sprintf("~Condition+%s",grp1)
        out <- meps_demo[[stat]] %>% rsub(by=by,subgrps=sg,yy=yr) %>% run
        results <- out %>% standardize(grp1,"Condition",stat)
        update.csv(results,file=outfile,dir=tables)
      }

    # by event type
      if(!done(outfile,dir=tables,grp1="event",grp2="Condition")){
        out <- meps_event[[stat]] %>% rsub(yy=yr) %>% run
        results <- out %>% standardize("event","Condition",stat)
        update.csv(results,file=outfile,dir=tables)
      }
      
    # by SOP
      if(!done(outfile,dir=tables,grp1="sop",grp2="Condition")){
        out <- meps_sop[[stat]] %>% rsub(yy=yr)  %>% run
        results <- out %>% standardize("sop","Condition",stat,type="sop")
        update.csv(results,file=outfile,dir=tables)
      }
  }
  
}   

  