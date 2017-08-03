#############################################
###   Accessibility and quality of care   ###
#############################################

standardize <- function(results,grp1,stat){
  results %>% 
    rename_(levels1=grp1) %>%
    gather(group,percent,-levels1) %>%
    separate(group,c("pctPOP","levels2"),sep="\\.",fill="left") %>%
    mutate(levels2 = gsub(stat,"",levels2)) %>%
    mutate(levels2 = factor(levels2,levels = unique(levels2))) %>%
    mutate(pctPOP = replace(pctPOP,is.na(pctPOP),"pctPOP")) %>%
    spread(pctPOP,percent) %>%
    rename(pctPOP_se = se) %>%
    mutate(grp1=grp1,grp2=stat) %>%
    select(grp1,grp2,levels1,levels2,pctPOP,pctPOP_se)
}

standardize_n <- function(results,grp1,grp2){
  results %>%
    mutate(grp1=grp1,grp2=grp2,n=counts) %>%
    mutate_(levels1=grp1) %>%
    select(grp1,grp2,levels1,counts)
}

##################################################
###                  LISTS                     ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc2_care"

# Shared
source("../../shared/app_global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
source("../dictionaries.R",chdir=T)
source("stats.R")

caregrps = care_subgrps %>% unlist %>% setNames(NULL)

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
  year_start = 2002
  year_end = max(meps_names$Year)
}
year_list = year_start:year_end


for(year in year_list){   print(year)
  
  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 
  yb <- substring(year-1,3,4)
  ya <- substring(year+1,3,4)
  
# Load data
  runSource('load/load_fyc.R',dir=shared,year=year,yy=yr,PUFdir=PUFdir,get_file_names(year))  
  
# Add subgroups  
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,yy=yr)
  for(grp in caregrps) runSource(sprintf("grps/%s.R",grp),dir=path,yy=yr,ya=ya,yb=yb)
  
# Define design
  runSource("svydesign/design_fyc.R", dir=shared,yy=yr)
  runSource("svydesign/design_diab.R",dir=shared,yy=yr)
  runSource("svydesign/design_saq.R", dir=shared,yy=yr)
    
# Run for each statistic
  
  for(stat in caregrps){

    outfile <- sprintf("%s/%s.csv",year,stat)

    svy = stat
    if(!svy %in% names(meps_svyby)) svy = strsplit(stat,"_")[[1]][1]
  
    for(grp1 in subgrp_list){
      #if(done(outfile,dir=tables,grp1=grp1,grp2=stat)) next
      
      out <- meps_svyby[[svy]] %>% rsub(by=grp1,formula=stat,FUN="svymean") %>% run
      results <- out %>% standardize(grp1=grp1,stat=stat)
      
      # sample size
      out_n <- meps_svyby[[svy]] %>% rsub(by=grp1,formula=stat,FUN="unwtd.count") %>% run
      results_n <- out_n %>% standardize_n(grp1=grp1,grp2=stat)
      
      both <- full_join(results,results_n,by=c("grp1","grp2","levels1"))
      
      update.csv(both,file=outfile,dir=tables)
    }
  }

} # end of yearlist loop
