survey <- function(grp1,grp2,stat,...){
  svyString <- r_svy(grp1=grp1,grp2=grp2,stat=stat,...)
  
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

dir = "C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables"
shared = paste0(dir,"/shared/r")
path = paste0(dir,"/hc1_use/r")
tables = paste0(path,"/tables")
PUFdir = paste0(dir,"/shared/PUFS")

setwd(dir) 

source(paste0(shared,"/run_global.R"),chdir=T) # shared
source("hc1_use/app_info.R",chdir=T)

# Load packages
  runSource('load_pkg.R',dir=shared)

# Define lists (subgrp_list defined in shared/r/run_functions.R)
  usegrps = c("sop", "event", "event_sop")

  stat_list = c(fyc_stats,evnt_stats,"n","n_exp")
              
  year_list = 2009:1996
 
##################################################
###                   RUN                      ###
##################################################

for(year in year_list){  
  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 

# Load data
  runSource('load_fyc.R',dir=shared,
            "year"=year,"yy"=yr,"PUFdir"=PUFdir,
            get_file_names(year))

# Add subgroups  
  for(grp in subgrps) runSource(sprintf("grps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in usegrps) runSource(sprintf("grps/%s.R",grp),dir=path,"yy"=yr)

# Load EVENTS and merge with FYC 
  runSource('load_events.R',dir=shared,"yy"=yr,"PUFdir"=PUFdir,
            "subgrps"=paste0(subgrp_list,",",collapse=""),
            get_file_names(year))
  
# Define survey design  
  runSource("design_fyc.R",dir=path,"yy"=yr)
  runSource("design_evnt.R",dir=path,"yy"=yr)
  
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
  
  
  