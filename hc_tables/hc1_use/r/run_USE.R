#############################################
###     UTILIZATION AND EXPENDITURES      ###
#############################################

# survey <- function(grp1,grp2,stat,...){
#   svyString <- r_svy(grps=c(grp1,grp2),stat=stat,...)  
#   cat(stat,":",svyString %>% writeLines)
#   results <- run(svyString)
#   results %>% standardize(grp1=grp1,grp2=grp2,stat=stat) 
# }

standardize <- function(results,grp1,grp2,stat){
  out <- results %>% select(-contains("FALSE")) 
  
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

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc1_use"

# Shared
source("../../shared/global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
source("../global.R",chdir=T)

usegrps = c("sop", "event", "event_sop")
stat_list = c(fyc_stats,evnt_stats,"n","n_exp")


#stat_list = c("avgEVT")
          
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
    
    runSource("stats/avgEVT.R",yy=yr)
    
    for(i in 1:sl){   
      for(j in i:sl){ 
        grp1 = subgrp_list[i]; strp1 <- gsub("_v2X","",grp1); 
        grp2 = subgrp_list[j]; strp2 <- gsub("_v2X","",grp2);
        if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
        if(done(outfile,dir=tables,grp1=grp1,grp2=grp2)) next

        results <- r_svy(c(grp1,grp2),stat,yr) %>% run %>% standardize(grp1,grp2,stat)
        update.csv(results,file=outfile,dir=tables)
      }
    }
    
  # Demographic subgroups x source of payment 
    
    runSource("stats/avgEVT_sop.R",yy=yr)
    
    for(grp1 in subgrp_list){   
      for(SOP in sop_list){ 
        if(done(outfile,dir=tables,grp1=grp1,levels2=SOP)) next
        
        nEVTdsgn <- update(nEVTdsgn,sop=SOP)
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        FYCdsgn <- update(FYCdsgn,sop=SOP)
        
        results <- r_svy(c(grp1,'sop'),stat,yr,sop=SOP) %>% run %>% standardize(grp1,'sop',stat)
        update.csv(results,file=outfile,dir=tables)
      }
    }

  # Demographic subgroups x event type

    for(grp1 in subgrp_list){  
      
      ## EVENT STATS: totEVT, meanEVT ##
      if(stat %in% evnt_stats){
        if(done(outfile,dir=tables,grp1=grp1,grp2='event_v2X')) next
        
        results <- r_svy(c(grp1,'event'),stat,yr) %>% run %>% standardize(grp1,'event',stat)
        update.csv(results,file=outfile,dir=tables)
        
        results_v2 <- r_svy(c(grp1,'event_v2X'),stat,yr) %>% run %>% standardize(grp1,'event_v2X',stat)
        update.csv(results_v2,file=outfile,dir=tables)
        
      ## FYC STATS: totPOP, totEXP,... ##  
      }else if(stat %in% fyc_stats){
        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,grp1=grp1,levels2=EVNT)) next
          FYCdsgn <- update(FYCdsgn,event=EVNT)
          results <- r_svy(c(grp1,'event'),stat,yr,event=EVNT) %>% run %>% standardize(grp1,'event',stat)
          update.csv(results,file=outfile,dir=tables)
        }
      
      ## avgEVT (skip 'TOT')##
      }else{ 
        
        runSource("stats/avgEVT_event.R",yy=yr,by=sprintf("~%s",grp1))
        
        for(EV in event_list[-1]){
          if(done(outfile,dir=tables,grp1=grp1,levels2=EV)) next
          
          results <- r_svy(c(grp1,'event'),stat,yr,event=EV) %>% run %>% 
            mutate(event = EV) %>%standardize(grp1,'event',stat)
          
          update.csv(results,file=outfile,dir=tables)
        }
      }
      
    }  
    
  # Source of payment x event type

    for(SOP in sop_list){
      
      ## EVENT STATS: totEVT, meanEVT ##
      if(stat %in% evnt_stats){
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        results <- r_svy(c('sop','event'),stat,yr,sop=SOP) %>% run %>% standardize('sop','event',stat)
        update.csv(results,file=outfile,dir=tables)
        
        results_v2 <- r_svy(c('sop','event_v2X'),stat,yr,sop=SOP) %>% run %>% standardize('sop','event_v2X',stat)
        update.csv(results_v2,file=outfile,dir=tables)
      
      ## FYC STATS: totPOP, totEXP,... ## 
      }else if(stat %in% fyc_stats){

        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,levels1=SOP,levels2=EVNT)) next
          
          FYCdsgn <- update(FYCdsgn,event=EVNT,sop=SOP)
          results <- r_svy(c('sop','event'),stat,yr,sop=SOP,event=EVNT) %>% run %>% standardize('sop','event',stat)
          update.csv(results,file=outfile,dir=tables)
        }
        
      ## avgEVT (skip 'TOT')##
      }else{ 
        
        runSource("stats/avgEVT_event_sop.R",yy=yr,sop=SOP)
        
        for(EV in event_list[-1]){
          if(done(outfile,dir=tables,levels1=SOP,levels2=EV)) next
          
          FYCsub$sop = SOP
          results <- r_svy(c('sop','event'),stat,yr,sop=SOP,event=EV) %>% run %>%
            select(-ind) %>% mutate(event = EV, sop=SOP) %>% standardize('sop','event',stat)
          
          update.csv(results,file=outfile,dir=tables)
        }
      }
    
    }  
    
  }# end of stat_list loop
}# end of year_list loop
  
  
  