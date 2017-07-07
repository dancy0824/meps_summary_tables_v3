#############################################
###  Access to care and quality of care   ###
#############################################

survey <- function(grp1,stat,yr,...){
  svyString <- r_svy(grps=grp1,stat=stat,...)
  
  cat(stat,":",svyString %>% writeLines)
  
  results <- run(svyString)
  results %>% standardize(grp1=grp1,stat=stat) 
} 

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

#####################################################################

dir = "C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables"
shared = paste0(dir,"/shared/r")
path = paste0(dir,"/hc2_care/r")
tables = paste0(path,"/tables")
PUFdir = paste0(dir,"/shared/PUFS")

setwd(dir)

source(paste0(shared,"/run_global.R"),chdir=T) # shared
source("hc2_care/app_info.R",chdir=T)

# Load packages
  runSource('load_pkg.R',dir=shared)

# Define lists (from shared/r/run_global and dictionaries.R)

  caregrps = care_subgrps %>% unlist
   
  year_list = 2014:1996


##################################################
###                   RUN                      ###
##################################################


for(year in year_list){   print(year)
  
  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 
  yb <- substring(year-1,3,4)
  ya <- substring(year+1,3,4)
  
# Load data
  runSource('load_fyc.R',dir=shared,
            "year"=year,"yy"=yr,"PUFdir"=PUFdir,
             get_file_names(year))  
  
  yr <- substring(year,3,4); 
  
# Add subgroups  
  for(grp in subgrps) runSource(sprintf("grps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in caregrps) runSource(sprintf("grps/%s.R",grp),dir=path,
                                 "yy"=yr,"ya"=ya,"yb"=yb)
  
# Define design
  runSource("design_fyc.R", dir=path,"yy"=yr)
  runSource("design_diab.R",dir=path,"yy"=yr)
  runSource("design_saq.R", dir=path,"yy"=yr)
    
# Run for each statistic
  
  for(stat in caregrps){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
    
    for(grp1 in subgrp_list){
      if(done(outfile,dir=tables,grp1=grp1,grp2=stat)) next
      
      results <- survey(grp1=grp1,stat=stat,yr=yr)
      update.csv(results,file=outfile,dir=tables)
    }
  }

} # end of yearlist loop


 