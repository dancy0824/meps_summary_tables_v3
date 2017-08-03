#############################################
###     UTILIZATION AND EXPENDITURES      ###
#############################################

gather_sop <- function(df){
  df %>% 
    gather(group,coef) %>%
    separate(group,c("stat","sop"),sep="\\.",fill="left") %>%
    mutate(stat = replace(stat,is.na(stat),"stat")) %>%
    spread(stat,coef) %>%
    select(sop,stat,se)
}

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
source("../../shared/app_global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
source("../dictionaries.R",chdir=T)
source("stats/stats.R")

usegrps = c("sop", "event", "event_sop")
stat_list = c(fyc_stats,evnt_stats,"n","n_exp")

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
  yr <- substring(year,3,4)
  sg <- paste0(subgrp_list,",",collapse="")

# Load data
  runSource('load/load_fyc.R',dir=shared,year=year,yy=yr,PUFdir=PUFdir,get_file_names(year))

# Add subgroups  
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in usegrps) runSource(sprintf("grps/%s.R",grp),dir=path,"yy"=yr)

# Load EVENTS and merge with FYC 
  runSource('load_events.R',yy=yr,PUFdir=PUFdir,subgrps=sg,get_file_names(year))
  
# Define survey design  
  runSource("svydesign/design_fyc.R",dir=shared,"yy"=yr)
  runSource("svydesign/design_evnt.R",dir=shared,"yy"=yr)
  
# Run for each statistic  
  for(stat in stat_list){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
  
  # Demographic subgroups, crossed 
  #  Dear future Emily: don't try to change i and j back to all_subgrps.
  #  You will end up with many redundancies. Bad juju.
    
    if(stat=="avgEVT") runSource("stats/avgEVT.R",yy=yr)
    
    for(i in 1:sl){   
      for(j in i:sl){ 
        
        grp1 = subgrp_list[i]; strp1 <- gsub("_v2X","",grp1); 
        grp2 = subgrp_list[j]; strp2 <- gsub("_v2X","",grp2);
        if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
        if(done(outfile,dir=tables,grp1=grp1,grp2=grp2)) next
 
        by <- sprintf("~%s+%s",grp1,grp2)
        count <- sprintf("PERWT%sF",yr)
        use <- sprintf("(XP%sX >= 0)",yr)
        
        out <- meps_svyby[[stat]] %>% rsub(by=by,count=count,use=use,yy=yr,event="TOT",sop="EXP",sp="XP") %>% run
        results <- out %>% standardize(grp1,grp2,stat)
        update.csv(results,file=outfile,dir=tables)

      }
    }
    
    
  # Demographic subgroups x source of payment 
    
    if(stat=="avgEVT") runSource("stats/avgEVT_sop.R",yy=yr)
    
    for(grp1 in subgrp_list){   
      for(SOP in sop_list){ 
        if(done(outfile,dir=tables,grp1=grp1,levels2=SOP)) next
        
        nEVTdsgn <- update(nEVTdsgn,sop=SOP)
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        FYCdsgn <- update(FYCdsgn,sop=SOP)
 
        by    <- sprintf("~%s+sop",grp1)
        count <- sprintf("TOT%s%s",SOP,yr)
        sp    <- sp_list[[SOP]]
        use   <- sprintf("(%s%sX > 0)",sp,yr)
        
        out <- meps_svyby[[stat]] %>% rsub(by=by,count=count,use=use,yy=yr,event="TOT",sop=SOP,sp=sp) %>% run
        results <- out %>% standardize(grp1,'sop',stat)
        update.csv(results,file=outfile,dir=tables)
 
      }
    }

  # Demographic subgroups x event type

    for(grp1 in subgrp_list){  
      
      ## avgEVT: (check first since avgEVT in evnt_stats) ##
      if(stat == "avgEVT"){
        by=sprintf("~%s",grp1)
        runSource("stats/avgEVT_event.R",yy=yr,by=by)
        
        for(EV in event_list[-1]){
          if(done(outfile,dir=tables,grp1=grp1,levels2=EV)) next
          
          out <- meps_svyby[["avgEVT_event"]] %>% rsub(event=EV) %>% run 
          results <- out %>% mutate(event=EV) %>% standardize(grp1,'event',stat)
          update.csv(results,file=outfile,dir=tables)
        }
      
      ## EVENT STATS: totEVT, meanEVT ##
      }else if(stat %in% evnt_stats){
        if(done(outfile,dir=tables,grp1=grp1,grp2='event_v2X')) next
        use <- sprintf("(XP%sX >= 0)",yr)
        
        out <- meps_svyby[[stat]] %>% rsub(use=use,by=sprintf("~%s+event",grp1),sp="XP",yy=yr) %>% run 
        results <- out %>% standardize(grp1,'event',stat)
        update.csv(results,file=outfile,dir=tables)
        
        out <- meps_svyby[[stat]] %>% rsub(use=use,by=sprintf("~%s+event_v2X",grp1),sp="XP",yy=yr) %>% run 
        results <- out %>% standardize(grp1,'event_v2X',stat)
        update.csv(results,file=outfile,dir=tables)
        
      ## FYC STATS: totPOP, totEXP,... ##  
      }else if(stat %in% c(fyc_stats,"n","n_exp")){
        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,grp1=grp1,levels2=EVNT)) next
          FYCdsgn <- update(FYCdsgn,event=EVNT)
          
          by <- sprintf("~%s+event",grp1)
          count <- paste0(use_list[[EVNT]],yr)
          
          out <- meps_svyby[[stat]] %>% rsub(by=by,count=count,event=EVNT,sop="EXP",yy=yr) %>% run 
          results <- out %>% standardize(grp1,'event',stat)
          update.csv(results,file=outfile,dir=tables)
          
        }
      }
    }  
    
  # Source of payment x event type

    if(stat == "avgEVT"){
      
      runSource("stats/avgEVT_event_sop.R",yy=yr,sop="EXP+SLF+MCR+MCD+PTR+OTZ")
      
      for(EV in event_list[-1]){
        if(done(outfile,dir=tables,grp1="sop",levels2=EV)) next
        out <- meps_svyby[["avgEVT_event"]] %>% rsub(event=EV) %>% run 
        results <- out %>% select(-ind) %>% gather_sop %>% mutate(event=EV) %>% standardize('sop','event',stat)
        update.csv(results,file=outfile,dir=tables)
      }
    }
    
  
    for(SOP in sop_list){

      ## EVENT STATS: totEVT, meanEVT ##
      if(stat %in% c("totEVT","meanEVT")){
        if(done(outfile,dir=tables,levels1=SOP,grp2='event_v2X')) next
        
        sp <- sp_list[[SOP]]
        use <- sprintf("(%s%sX > 0)",sp,yr)
        
        EVNTdsgn <- update(EVNTdsgn,sop=SOP)
        
        out <- meps_svyby[[stat]] %>% rsub(use=use,by="~sop+event",sp=sp,yy=yr) %>% run 
        results <- out %>% standardize('sop','event',stat)
        update.csv(results,file=outfile,dir=tables)
        
        out <- meps_svyby[[stat]] %>% rsub(use=use,by="~sop+event_v2X",sp=sp,yy=yr) %>% run 
        results <- out %>% standardize('sop','event_v2X',stat)
        update.csv(results,file=outfile,dir=tables)
        
        
      ## FYC STATS: totPOP, totEXP,... ## 
      }else if(stat %in% c(fyc_stats,"n","n_exp")){

        for(EVNT in event_list){ 
          if(done(outfile,dir=tables,levels1=SOP,levels2=EVNT)) next
          
          FYCdsgn <- update(FYCdsgn,event=EVNT,sop=SOP)
          count <- paste0(EVNT,SOP,yr)

          out <- meps_svyby[[stat]] %>% rsub(by="~sop+event",count=count,event=EVNT,sop=SOP,yy=yr) %>% run 
          results <- out %>% standardize('sop','event',stat)
          update.csv(results,file=outfile,dir=tables)
        }
        
      }
    }  
    
  }# end of stat_list loop
  
}# end of year_list loop
  
  
  