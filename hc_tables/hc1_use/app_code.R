
###############################################################
###                    CODE STRINGS                         ###
###############################################################

source("r/stats/stats.R")

r_fyc <- function(grps,stat){
  
  code <- readSource("../shared/r/load/load_fyc.R")
  code <- code %>% add(subgrp_code(grps=grps,lang='r')) 

  if(all(c("event","sop") %in% grps)){code <- code %>% add(readSource("r/grps/event_sop.R"))
  }else if("event" %in% grps) { code <- code %>% add(readSource("r/grps/event.R"))
  }else if("sop" %in% grps) { code <- code %>% add(readSource("r/grps/sop.R"))}

  code <- code %>% add(readSource("../shared/r/svydesign/design_fyc.R"))
  
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

  code <- readSource("../shared/r/load/load_fyc.R")
  code <- code %>% add(subgrp_code(grps=grps,lang='r'))
  code <- code %>% add(readSource("r/load_events.R"))
  code <- code %>% add(readSource("r/merge_events.R"))
  code <- code %>% add(readSource("../shared/r/svydesign/design_evnt.R"))
   
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
  
  code <- readSource("../shared/r/load/load_fyc.R")
  code <- code %>% add(subgrp_code(grps=grps,lang='r'))
  code <- code %>% add(readSource("r/load_events.R"))

  # Load pre-code (includes svydesign)
  if('event' %in% grps) read_stat = "avgEVT_event" else read_stat = stat
  extension = grps[grps %in% c("sop","event")] %>% sort
  codeName = paste(c("avgEVT",extension),collapse="_")
  code <- code %>% add(readSource(sprintf("r/stats/%s.R",codeName),verbose=F))

  ## Load analysis code
  
  # Remove extra subgroups
  gp <- grps[!grps %in% c("event","sop")]
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


get_r_code <- function(stat,grps,year){
  
  yr <- substring(year,3,4)
  
  ## Load packages
  code <- readSource('../shared/r/load/load_pkg.R')
  
  if(stat == "avgEVT"){ code <- code %>% add(r_avgEVT(grps=grps)) 
  }else if(stat %in% fyc_stats){code <- code %>% add(r_fyc(grps=grps,stat=stat)) 
  }else if(stat %in% evnt_stats){code <- code %>% add(r_evnt(grps=grps,stat=stat))}

  code %>% rsub(PUFdir="C:/MEPS", get_file_names(year),year=year, yy=yr)
}

# 
# get_r_code <- function(grps,stat,year){
#   
#   type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
#   yr <- substring(year,3,4)
#   
#   read_stat <- stat
# 
#   ## Load packages
#     code <- readSource('../shared/r/load/load_pkg.R')
#   
#   ## Load MEPS files
# 
#     # if FYC, load FYC 
#       code <- code %>% add(readSource("../shared/r/load/load_fyc.R"))
#       
#     # Add subgroups to FYC
#       code <- code %>% add(subgrp_code(grps=grps,lang='r'))
#       
#       if(type == "FYC"){
#         if(all(c("event","sop") %in% grps)){code <- code %>% add(readSource("r/grps/event_sop.R"))
#         }else if("event" %in% grps) { code <- code %>% add(readSource("r/grps/sop.R"))
#         }else if("sop" %in% grps) { code <- code %>% add(readSource("r/grps/event.R"))}
#       }
# 
#     # if EVNT, load FYC + Event Files
#       if(type == "EVNT") code <- code %>% add(readSource("r/load_events.R"))
# 
#     # if avgEVT, load pre_code
#       if(stat == 'avgEVT'){
#         if('event' %in% grps) read_stat = "avgEVT_event"
#         extension = grps[grps %in% c("sop","event")] %>% sort
#         codeName = paste(c("avgEVT",extension),collapse="_")
#         code <- code %>% add(readSource(sprintf("r/stats/%s.R",codeName),verbose=F))
#       }
# 
#   ## Define survey design
#       
#       if(type == "FYC" & stat != 'avgEVT') code <- code %>% add(readSource("../shared/r/svydesign/design_fyc.R"))
#       if(type == "EVNT" & stat != 'avgEVT') code <- code %>% add(readSource("../shared/r/svydesign/design_evnt.R"))
#       
#   ## Load analysis code
# 
#     # Remove extra subgroups
#     if(type == "FYC")  gp <- grps[!grps %in% c("ind","event","sop")]
#     if(type == "EVNT") gp <- grps[!grps %in% c("ind","sop")]
# 
#     if(length(gp)==0) meps_code = meps_svy[[read_stat]] else meps_code = meps_svyby[[read_stat]]
#       
#     # Iterate over events, sops, if needed  
#     if('sop' %in% grps) sops = sop_list else sops = "EXP"
#     if('event' %in% grps & (type=="FYC"|stat=="avgEVT")) events = event_list else events = "TOT"
#     
#     countfun = function(event="",sop="") "PERWT.yy.F"
#     if('event' %in% grps) countfun = function(event,sop="") sprintf("%s.yy.",use_list[[event]])
#     if('sop' %in% grps) countfun = function(event,sop) sprintf("%s%s.yy.",event,sop)
#       
#     usefun = function(sp) sprintf("(%s.yy.X >= 0)",sp)
#     if('sop' %in% grps) usefun = function(sp) sprintf("(%s.yy.X > 0)",sp)
#     
#     # Add instructions for 'v2X' variables
#     
#     v2X <- grps[grps %in% c("event","agegrps","insurance")]
#     
#     if(length(v2X) > 0){
#       code <- code %>% add("# For groups with sub-categories, re-run with the '_v2X' subscript")
#     }
#     
#     # Run code
#     for(sop in sops){
#       if('event'%in% grps & type=="FYC") code <- code %>% add(sprintf("# Source of payment: %s",sop),collapse="\n\n")
#       for(event in events){
#         
#         count = countfun(event,sop)
#         sp  = sp_list[[sop]]
#         use = usefun(sp)
#         
#         code <- code %>% add(meps_code %>% rsub(count=count,event=event,sop=sop,use=use,sp=sp))
#       }
#     }
#       
#   ## RSUB values in code
# 
#     subgrps <- paste(gp,collapse=",")
#     if(subgrps!="") subgrps = paste0(subgrps,",")
#     by <- sprintf("~%s",paste(gp,collapse="+"))
#     
#     code <- code %>% rsub(
#       PUFdir = "C:/MEPS", get_file_names(year),
#       by=by,subgrps=subgrps,year=year, yy=yr
#     )
#     
#     #  code %>% writeLines
# 
#   return(code)
# }
# 



get_sas_code <- function(grps,stat,yr,...){
  
  return("Need some code!")
  
  # type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  # brk = 60
  # exdent = 12
  # 
  # yy = yr
  # counts = "count"
  # vars = "TOTEXP&yy."
  # uses = "XP&yy.X"
  # gt = ">="
  # 
  # if(all(c('event','sop') %in% grps)){
  #   gt = ">"
  #   uses <- paste0(sp_list,"&yy.X",collapse=" ")
  #   counts <- vars <- 
  #     paste0(outer(event_list,sop_list,paste0),"&yy.",collapse=" ") %>% 
  #     str_wrap(brk,exdent=exdent) 
  #   
  # }else if('sop' %in% grps){
  #   gt <- ">"
  #   uses <- paste0(sp_list,"&yy.X",collapse = " ") 
  #   counts <- vars <- paste0("TOT",sop_list,"&yy.",collapse = " ") 
  #   
  # }else if('event' %in% grps){
  #   gt = ">=" ;
  #   uses = "XP&yy.X";
  #   counts <- paste0(use_list,"&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent) 
  #   vars <- paste0(event_list,"EXP&yy.",collapse=" ") %>% str_wrap(brk,exdent=exdent) 
  # }
  # 
  # if(length(grps) > 0){
  #   format <- sprintf('FORMAT %s',paste(grps,paste0(grps,"."),collapse=" "))
  #   domain <- sprintf('DOMAIN %s',paste(grps,collapse="*"))
  # }else{
  #   format <- domain <- ""
  # }
  # 
  # readSource(sprintf("sas/stats/%s.sas",stat),type='sas',verbose=T,
  #            format=format,domain=domain,
  #            gt=gt,uses=uses,counts=counts,vars=vars,yy=yr)
}



### ADD EVENT_V2X FOR evnt GROUPS -- maybe in DOMAIN statement??

