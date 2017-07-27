
###############################################################
###                    CODE STRINGS                         ###
###############################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load_code <- function(grps,stat,year,lang="r"){
  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  yr <- substring(year,3,4)
  
  code <- readSource(sprintf("../shared/%s/load/load_fyc.%s",lang,lang)) # Load FYC
  code <- code %>% add(subgrp_code(grps=grps,lang=lang)) # Define subgroups

  if(type == "FYC"){
    local_subgrps <- grps[grps %in% local_grps] %>% unique
    if(all(c("sop","event") %in% grps)) local_subgrps <- c("event_sop")
    
    code <- code %>% add(sapply(local_subgrps, function(x)
      readSource(sprintf("%s/grps/%s.%s",lang,x,lang))) %>% paste(collapse="\n"))
  
  }else if(type == "EVNT"){
    code <- code %>% 
      add(readSource(sprintf("../shared/%s/load/load_events.%s",lang,lang)))
  }

  code %>% rsub(type=lang,
                year=year,yy=yr,PUFdir = "C:/MEPS",
                get_file_names(year))
}

#################################################################


### !!! load design here !!!


source("r/stats/stats.R")

r_svy <- function(grps,stat,yr,sop="EXP",event="TOT",display=F,verbose=T){
  
  # Remove filler subgroups if using display
  gp <- grps  
  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  if(display){
    if(type == "FYC") gp <- grps[!grps %in% c("ind","event","sop")]
    if(type == "EVNT") gp <- grps[!grps %in% c("ind","sop")]
  }
  
  # Set pre-code if stat == 'avgEVT'
  precode <- ""
  if(stat == 'avgEVT'){
    extension = grps[grps %in% c("sop","event")] %>% sort
    codeName = paste(c("avgEVT",extension),collapse="_")
    if(display) precode <- readSource(sprintf("stats/%s.R",codeName),verbose=F)
    if('event' %in% grps){
      stat = "avgEVT_event"
      gp = gp[gp!="event"]
    }
  }
  
  # Set count and use variables if event or sop in grps
  count <- "PERWT.yy.F"
  if('event' %in% grps) count <- paste0(use_list[[event]],yr)
  if('sop' %in% grps) count <- paste0(event,sop,yr)
  
  sp <- sp_list[[sop]]
  use <- "(.sp..yy.X >= 0)"
  if('sop' %in% grps) use <- "(.sp..yy.X > 0)"
  
  if(length(gp)==0) meps_code = meps_svy else meps_code = meps_svyby
  
  code <- meps_code[[stat]] 
  
  out <- paste(precode,code) %>% 
    rsub(by = sprintf("~%s",paste0(gp,collapse="+")),
         subgrps = paste(paste0(gp,collapse=", "),","),
         grps=gp, stat=stat, count=count, use=use, 
         event=event, sop=sop, sp=sp, yy=yr) 
  
  if(verbose) out %>% writeLines
  return(out)
}



get_r_code <- function(grps,stat,yr){
  
  code <- ""
  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  
  if('sop' %in% grps) sops = sop_list else sops = "EXP"
  if('event' %in% grps) events = event_list else events = "TOT"
  
  # Run code
  if(type=="FYC"){
    for(sop in sops){
      if('event'%in% grps) code <- code %>% add(sprintf("# Source of payment: %s",sop))
      for(event in events) code <- code %>% add(r_svy(grps,stat,yr,sop=sop,event=event,display=T))  
    }
    
  }else if(type=="EVNT"){
    for(sop in sops) code <- code %>% add(r_svy(grps,stat,yr,sop=sop,display=T))  
    if('event' %in% grps){
      grps_v2X <- grps %>% replace(grps=="event","event_v2X")
      for(sop in sops) code <- code %>% add(r_svy(grps_v2X,stat,yr,sop=sop,display=T))  
    }
  }
  
  return(code)
}




get_sas_code <- function(grps,stat,yr,...){
  
  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  brk = 60
  exdent = 12
  
  yy = yr
  counts = "count"
  vars = "TOTEXP&yy."
  uses = "XP&yy.X"
  gt = ">="
  
  if(all(c('event','sop') %in% grps)){
    gt = ">"
    uses <- paste0(sp_list,"&yy.X",collapse=" ")
    counts <- vars <- 
      paste0(outer(event_list,sop_list,paste0),"&yy.",collapse=" ") %>% 
      str_wrap(brk,exdent=exdent) 
    
  }else if('sop' %in% grps){
    gt <- ">"
    uses <- paste0(sp_list,"&yy.X",collapse = " ") 
    counts <- vars <- paste0("TOT",sop_list,"&yy.",collapse = " ") 
    
  }else if('event' %in% grps){
    gt = ">=" ;
    uses = "XP&yy.X";
    counts <- paste0(use_list,"&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent) 
    vars <- paste0(event_list,"EXP&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent) 
  }
  
  if(length(grps) > 0){
    format <- sprintf('FORMAT %s',paste(grps,paste0(grps,"."),collapse=" "))
    domain <- sprintf('DOMAIN %s',paste(grps,collapse="*"))
  }else{
    format <- domain <- ""
  }
  
  readSource(sprintf("sas/stats/%s.sas",stat),type='sas',verbose=T,
             format=format,domain=domain,
             gt=gt,uses=uses,counts=counts,vars=vars,yy=yr)
}



### ADD EVENT_V2X FOR evnt GROUPS -- maybe in DOMAIN statement??

