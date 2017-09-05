#  Cyu #
###############################################################
###                    CODE STRINGS                         ###
###############################################################

source("r/stats/stats.R")

avgFUN <- readSource("r/stats/avgEVT_event.R")

pop <- function(vec,...) vec[!vec %in% unlist(list(...)) ] 

v2X_message <- function(grps,include=NULL,...){
  v2X <- grps[grps %in% c(include,"agegrps","insurance")]
  if(length(v2X) > 0) return("# For groups with sub-categories, re-run with the '_v2X' subscript")
  return()
}

load_data <- function(grps,stat,lang="r"){
  
  if(lang=="r") LANG = "R" else LANG = lang
  
  code <- readSource(sprintf("../shared/%s/load/load_fyc.%s",lang,LANG))
  code <- code %>% add(subgrp_code(grps=grps,lang=lang)) 
  
  if(stat %in% fyc_stats){
    if(all(c("event","sop") %in% grps)){code <- code %>% add(readSource(sprintf("%s/grps/event_sop.%s",lang,LANG)))
    }else if("event" %in% grps) { code <- code %>% add(readSource(sprintf("%s/grps/event.%s",lang,LANG)))
    }else if("sop" %in% grps) { code <- code %>% add(readSource(sprintf("%s/grps/sop.%s",lang,LANG)))}
    
    if(lang=="r") code <- code %>% add(readSource("../shared/r/svydesign/design_fyc.R"))
    return(code)
  }

  if(stat == "avgEVT"){ # Load pre-code (R includes svydesign)
    code <- code %>% add(readSource(sprintf("%s/load_events.%s",lang,LANG)))
    if(lang=="r"& !('event' %in% grps)) code <- code %>% add(readSource("r/stats/avgEVT.R"))
    return(code)
  }
  
  if(stat %in% evnt_stats){
    code <- code %>% add(readSource(sprintf("%s/load_events.%s",lang,LANG)))
    code <- code %>% add(readSource(sprintf("%s/merge_events.%s",lang,LANG)))
    if(lang=="r") code <- code %>% add(readSource("../shared/r/svydesign/design_evnt.R"))
    return(code)
  }
  
  stop("Not a valid statistic.")
}

###############################################################

base_code <- function(stat,grps,ignore=NULL,display=T){
  gp <- grps %>% pop(ignore)
  if(length(gp)==0 & !display)  gp <- 'ind'
  if(length(gp)==0) meps_code = meps_svy[[stat]] else meps_code = meps_svyby[[stat]]
  meps_code %>% rsub(by=subgrp_formula(gp))
}

run_main <- function(meps_code,...,event="TOT",sop="EXP",display=T){
  main <- meps_code %>% rsub(event=event,sop=sop,...)
  if(!display) main = sprintf("%s = %s",paste0(event,sop),main) # assign variables if running estimates
  return(main)
}


r_fyc <- function(grps,stat,display=T){
  code <- v2X_message(grps)
  
  sops = "EXP"
  events = "TOT"
  countfun = function(event="",sop="") "PERWT.yy.F"

  if('event' %in% grps){
    events = event_list
    countfun = function(event,sop="") sprintf("%s.yy.",use_list[[event]])}
  
  if('sop' %in% grps){
    sops = sop_list
    countfun = function(event,sop) sprintf("%s%s.yy.",event,sop)}
  
  meps_code <- base_code(stat,grps,ignore=c("ind","event","sop"),display=display)
  for(sop in sops){
    if(length(events) > 1){
      code <- code %>% add(sprintf("# Source of payment: %s",sop),collapse="\n\n")}
    for(event in events){
      code <- code %>% add(run_main(meps_code,display=display,event=event,sop=sop,count=countfun(event,sop)))}}

  return(code)
}


r_avgEVT <- function(grps,stat="avgEVT",display=T){
  code <- v2X_message(grps)
  
  if("event" %in% grps){
    codeName = "avgEVT_event"
    code <- code %>% add(avgFUN)
  }else{
    codeName = "avgEVT"}
  
  # Iterate over events, sops, if needed  
  if('event' %in% grps) events = event_list %>% pop("TOT") else events = "TOT"
  if('sop' %in% grps) sop_formula = "EXP+SLF+MCR+MCD+PTR+OTZ" else sop_formula = "ANY"
  
  meps_code <- base_code(codeName,grps,ignore=c("ind","event","sop"),display=display)
  for(event in events){
    code <- code %>% add(run_main(meps_code,display=display,event=event))}
  
  gp <- grps %>% pop("ind","event","sop")
  if(length(gp)==0) gp = "ind"
  
  return(code %>% rsub(sop_formula = sop_formula,by=subgrp_formula(gp)))
}


r_evnt <- function(grps,stat,display=T){
  code <- v2X_message(grps,include="event")
  
  # Iterate over sops, if needed
  if('sop' %in% grps){
    usefun = function(sp) sprintf("(%s.yy.X > 0)",sp)
    sops = sop_list
  }else{
    usefun = function(sp) sprintf("(%s.yy.X >= 0)",sp)
    sops = "EXP"
  }
  
  meps_code <- base_code(stat,grps,ignore=c("ind","sop"),display=display)
  for(sop in sops){
    code <- code %>% add(run_main(meps_code,display=display,sop=sop,sp=sp_list[[sop]],use=usefun(sp_list[[sop]])))}
  
  return(code )
}


get_r_code <- function(rows,cols,stat,year,display=T){
  
  yr <- substring(year,3,4)
  grps <- c(rows,cols)
  code <- ""
  
  ## Load packages
  if(display){
    code <- code %>% add(readSource('../shared/r/load/load_pkg.R'))
    code <- code %>% add(load_data(grps,stat,lang="r"))
  }
  
  if(stat == "avgEVT"){ code <- code %>% add(r_avgEVT(grps=grps,display=display)) 
  }else if(stat %in% fyc_stats){code <- code %>% add(r_fyc(grps=grps,stat=stat,display=display)) 
  }else if(stat %in% evnt_stats){code <- code %>% add(r_evnt(grps=grps,stat=stat,display=display))}

  code %>% rsub(PUFdir="C:/MEPS", get_file_names(year), year=year, yy=yr)
}


############################################################


get_sas_code <- function(rows,cols,stat,year){
  
  yr <- substring(year,3,4)
  grps <- c(rows,cols)
  
  brk = 60
  exdent = 12
  counts = "count"
  vars = "TOTEXP&yy."
  uses = "XP&yy.X"
  gt = ">="
  addon = ""
  
  code <- load_data(grps,stat,lang="sas")

  if(all(c('event','sop') %in% grps)){
    addon = "_event_sop"
    gt = ">"
    uses <- paste0(sp_list,"&yy.X",collapse=" ")
    counts <- vars <-
      paste0(outer(event_list,sop_list,paste0),"&yy.",collapse=" ") %>%
      str_wrap(brk,exdent=exdent)

  }else if('sop' %in% grps){
    addon = '_sop'
    gt <- ">"
    uses <- paste0(sp_list,"&yy.X",collapse = " ")
    counts <- vars <- paste0("TOT",sop_list,"&yy.",collapse = " ")

  }else if('event' %in% grps){
    addon = '_event'
    gt = ">=" ;
    uses = "XP&yy.X";
    counts <- paste0(use_list,"&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent)
    vars <- paste0(event_list,"EXP&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent)
  }

  gp <- gp_format <- grps %>% pop("ind","sop","event")
  gp_domain <- grps %>% pop("ind","sop")
  
  if(!stat %in% c("totEVT","meanEVT")) gp_domain <- gp_domain[gp_domain!="event"]
  
  format <- domain <- ""
  if(length(gp_format) > 0) format <- sprintf('FORMAT %s',paste(gp_format,paste0(gp_format,"."),collapse=" "))
  if(length(gp_domain) > 0) domain <- sprintf('DOMAIN %s',paste(gp_domain,collapse="*"))
  
  if(stat == 'avgEVT') stat = paste0('avgEVT',addon)
  
  code <- code %>% add(readSource(sprintf("sas/stats/%s.sas",stat)))
  
  code %>% 
    rsub(type = 'sas',PUFdir="C:\\\\MEPS", get_file_names(year),ods="",
         format=format,domain=domain, subgrps=paste(gp,collapse=" "),
         gt=gt,uses=uses,counts=counts,vars=vars,year=year, yy=yr) %>%
    gsub("\n\t;","",.) %>%
    gsub("\t\n;","",.) 
    
}
