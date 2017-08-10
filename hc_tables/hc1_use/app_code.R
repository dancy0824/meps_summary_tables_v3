
###############################################################
###                    CODE STRINGS                         ###
###############################################################

source("r/stats/stats.R")

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
    extension = grps[grps %in% c("sop","event")] %>% sort
    codeName = paste(c("avgEVT",extension),collapse="_")
    if(lang=="r") code <- code %>% add(readSource(sprintf("%s/stats/%s.%s",lang,codeName,LANG)))
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

r_fyc <- function(grps,stat){

  code <- load_data(grps,stat,lang="r")
  
  gp <- grps[!grps %in% c("ind","event","sop")]
  if(length(gp)==0) meps_code = meps_svy[[stat]] else meps_code = meps_svyby[[stat]]
  
  # Iterate over events, sops, if needed  
  if('sop' %in% grps) sops = sop_list else sops = "EXP"
  if('event' %in% grps) events = event_list else events = "TOT"
  
  countfun = function(event="",sop="") "PERWT.yy.F"
  if('event' %in% grps) countfun = function(event,sop="") sprintf("%s.yy.",use_list[[event]])
  if('sop' %in% grps) countfun = function(event,sop) sprintf("%s%s.yy.",event,sop)
  
  # Add instructions for 'v2X' variables
  v2X <- grps[grps %in% c("agegrps","insurance")]
  if(length(v2X) > 0) code <- code %>% add("# For groups with sub-categories, re-run with the '_v2X' subscript")
  
  # Run code
  for(sop in sops){
    if('event'%in% grps) code <- code %>% add(sprintf("# Source of payment: %s",sop),collapse="\n\n")
    for(event in events){
      count = countfun(event,sop)
      code <- code %>% add(meps_code %>% rsub(count=count,event=event,sop=sop))
    }
  }
  
  code %>% rsub(by=subgrp_formula(gp),subgrps=subgrp_comma(gp))
}


r_evnt <- function(grps,stat){
 
  code <- load_data(grps,stat,lang="r")
  
  gp <- grps[!grps %in% c("ind","sop")]
  if(length(gp)==0) meps_code = meps_svy[[stat]] else meps_code = meps_svyby[[stat]]
  
  # Iterate over sops, if needed  
  if('sop' %in% grps){
    usefun = function(sp) sprintf("(%s.yy.X > 0)",sp)
    sops = sop_list
  }else{
    usefun = function(sp) sprintf("(%s.yy.X >= 0)",sp)
    sops = "EXP"
  }
 
  # Add instructions for 'v2X' variables
  v2X <- grps[grps %in% c("event","agegrps","insurance")]
  if(length(v2X) > 0) code <- code %>% add("# For groups with sub-categories, re-run with the '_v2X' subscript")
  
  # Run code
  for(sop in sops){
    sp  = sp_list[[sop]]
    use = usefun(sp)
    code <- code %>% add(meps_code %>% rsub(sop=sop,use=use,sp=sp))
  }
  
  gp2 <- gp[gp != 'event']
  code %>% rsub(by=subgrp_formula(gp),subgrps=subgrp_comma(gp2))
}


r_avgEVT <- function(grps,stat="avgEVT"){

  code <- load_data(grps,stat,lang="r")
  
  # Remove extra subgroups
  gp <- grps[!grps %in% c("event","sop")]
  if('event' %in% grps) read_stat = "avgEVT_event" else read_stat = stat
  if(length(gp)==0) meps_code = meps_svy[[read_stat]] else meps_code = meps_svyby[[read_stat]]
  
  # Iterate over events, sops, if needed  
  if('event' %in% grps) events = event_list[event_list!="TOT"] else events = "TOT"
  if('sop' %in% grps) sop_formula = "EXP+SLF+MCR+MCD+PTR+OTZ" else sop_formula = "EXP"
  
  # Add instructions for 'v2X' variables
  v2X <- grps[grps %in% c("agegrps","insurance")]
  if(length(v2X) > 0) code <- code %>% add("# For groups with sub-categories, re-run with the '_v2X' subscript")
  
  # Run code
    for(event in events){
      code <- code %>% add(meps_code %>% rsub(event=event))
    }
  
  if(length(gp)==0) gp = "ind"
  
  code %>% rsub(by=subgrp_formula(gp),subgrps=subgrp_comma(gp),sop=sop_formula)
}


get_r_code <- function(rows,cols,stat,year){
  
  yr <- substring(year,3,4)
  
  ## Load packages
  code <- readSource('../shared/r/load/load_pkg.R')
  
  grps = c(rows,cols)
  
  if(stat == "avgEVT"){ code <- code %>% add(r_avgEVT(grps=grps)) 
  }else if(stat %in% fyc_stats){code <- code %>% add(r_fyc(grps=grps,stat=stat)) 
  }else if(stat %in% evnt_stats){code <- code %>% add(r_evnt(grps=grps,stat=stat))}

  code %>% rsub(PUFdir="C:/MEPS", get_file_names(year),year=year, yy=yr)
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

  gp <- gp_format <- grps[!grps %in% c("ind","sop","event")]
  gp_domain <- grps[!grps %in% c("ind","sop")]
  
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

#get_sas_code(rows="event",cols="sop",stat="totEVT",year=2013) %>% writeLines

# 
# g1 = "sex"
# g2 = "event"
# st = "pctEXP"
# 
# get_r_code(g1,g2,stat=st,year=2014) %>% writeLines
# get_sas_code(g1,g2,stat=st,year=2014) %>% writeLines
# 
# # # 
# sas_fyc(grps=c(g1,g2),stat="avgEVT") %>% writeLines
# get_sas_code(g1,g2,stat="avgEVT",year=2014) %>% writeLines
# 
# 
# substring(get_sas_code(g1,g2,stat="avgEVT",year=2014),2000,2753)
# ### ADD EVENT_V2X FOR evnt GROUPS -- maybe in DOMAIN statement??

