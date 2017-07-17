#############################################
###     UTILIZATION AND EXPENDITURES      ###
#############################################

survey <- function(grp1,grp2,stat,...){
  svyString <- r_svy(grps=c(grp1,grp2),stat=stat,...)  
  cat(stat,":",svyString %>% writeLines)
  results <- run(svyString)
  results %>% standardize(grp1=grp1,grp2=grp2,stat=stat) 
}

standardize <- function(results,grp1,grp2,stat){
  out <- results %>% select(-contains("FALSE")) 
  cc <- length(names(out))
  key = c(stat,paste0(stat,"_se"))
  names(out)[(cc-1):cc] = key
  
  out %>%
    mutate(grp1=grp1,grp2=grp2) %>%
    mutate_(levels1=grp1,levels2=grp2) %>%
    select(grp1,grp2,levels1,levels2,one_of(key))
}

##################################################
###                  LISTS                     ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc1_use"

# Shared
source("../../shared/global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
source("../global.R",chdir=T)

usegrps = c("sop", "event", "event_sop")
stat_list = c(fyc_stats,evnt_stats,"n","n_exp")


stat_list = c("nEVT","avgEVT")
          
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
  
  
for(year in year_list){  
  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 

# Load data
  runSource('load/load_fyc.R', dir=shared,
            "year"=year,"yy"=yr,"PUFdir"=PUFdir,
            get_file_names(year))

# Add subgroups  
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in usegrps) runSource(sprintf("grps/%s.R",grp),dir=path,"yy"=yr)

# Load EVENTS and merge with FYC 
  runSource('load/load_events.R',dir=shared,"yy"=yr,"PUFdir"=PUFdir,
            "subgrps"=paste0(subgrp_list,",",collapse=""),
            get_file_names(year))
  
# Define survey design  
  runSource("svydesign/design_fyc.R",dir=shared,"yy"=yr)
  runSource("svydesign/design_evnt.R",dir=shared,"yy"=yr)
  
# Run for each statistic  
  for(stat in stat_list){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
    
    # Demographic subgroups, crossed 
    #  Dear future Emily: don't try to change i and j back to all_subgrps.
    #  You will end up with many redundancies. Bad juju.
    
    for(i in 1:sl){   
      for(j in i:sl){ 
        grp1 = subgrp_list[i]; strp1 <- gsub("_v2X","",grp1); 
        grp2 = subgrp_list[j]; strp2 <- gsub("_v2X","",grp2);
        if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
        if(done(outfile,dir=tables,grp1=grp1,grp2=grp2)) next
        
        results <- survey(grp1=grp1,grp2=grp2,stat=stat,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }
    
    # Demographic subgroups x source of payment 
    for(grp1 in subgrp_list){   
      for(SOP in sop_list){ 
        if(done(outfile,dir=tables,grp1=grp1,levels2=SOP)) next
        
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        FYCdsgn <- update(FYCdsgn,sop=SOP)
        results <- survey(grp1=grp1,grp2='sop',sop=SOP,stat=stat,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }

    # Demographic subgroups x event type
    for(grp1 in subgrp_list){  
      
      if(stat %in% evnt_stats){
        if(done(outfile,dir=tables,grp1=grp1,grp2='event_v2X')) next
        
        results <- survey(grp1=grp1,grp2='event',stat=stat,yr=yr)
        update.csv(results,file=outfile,dir=tables)
        
        results_v2 <- survey(grp1=grp1,grp2='event_v2X',stat=stat,yr=yr)
        update.csv(results_v2,file=outfile,dir=tables)
        
      }else{
        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,grp1=grp1,levels2=EVNT)) next
          FYCdsgn <- update(FYCdsgn,event=EVNT)
          results <- survey(grp1=grp1,grp2='event',event=EVNT,stat=stat,yr=yr)
          update.csv(results,file=outfile,dir=tables)
        }
      }
      
    }  
    
    # Source of payment x event type
    for(SOP in sop_list){
      
      if(stat %in% evnt_stats){
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        results <- survey(grp1='sop',grp2='event',sop=SOP,stat=stat,yr=yr)
        update.csv(results,file=outfile,dir=tables)
        
        results_v2 <- survey(grp1='sop',grp2='event_v2X',sop=SOP,stat=stat,yr=yr)
        update.csv(results_v2,file=outfile,dir=tables)
        
      }else{
        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,levels1=SOP,levels2=EVNT)) next
          
          FYCdsgn <- update(FYCdsgn,event=EVNT,sop=SOP)
          results <- survey(grp1='sop',grp2='event',sop=SOP,event=EVNT,stat=stat,yr=yr)
          update.csv(results,file=outfile,dir=tables)
        }
      }
    }  
    
  }# end of stat_list loop
}# end of year_list loop
  
  
  