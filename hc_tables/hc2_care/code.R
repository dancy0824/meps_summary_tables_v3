# 
# code_string_use <- function(stat,grp1,grp2,year){
#   
#   fyc_stat = stat %in% c("totPOP","pctEXP","totEXP","meanEXP0","meanEXP","medEXP")
#   evt_stat = stat %in% c("totEVT","meanEVT")
#   
#   # Load data
#   yearC = as.character(year)
#   yr = substring(year,3,4)
#   
#   this_year = meps_names %>% filter(Year==year)
#   file_names = as.list(this_year$url) %>% setNames(this_year$file_type)
#   
#   load_fyc <- snp_load$load_fyc %>% rsub(file_names)
#   load_evnts <- snp_load$load_events %>% rsub(file_names,yy=yr)
#   
#   # Subgroup code
#   
#   subgrps = c(grp1,grp2)
#   
#   subgrps = gsub('_v2','',subgrps)
#   true_subgrps = subgrps[!subgrps %in% c("ind","event","sop")]
#   
#   if(all(c("event","sop")%in%subgrps)) subgrps = c("event_sop")
#   if(any(c("agegrps","insurance","education","employed") %in% subgrps)) subgrps = c("agevar",subgrps)
#   
#   subgrp_code <- c(snp_subgrps, snp_use)[subgrps]
#   
#   # Design code
#   design_code <- dsgn_code(stat,subgrps)
#   
#   # Combine code components
#   code <- c(load_fyc, subgrp_code, ifelse(evt_stat,load_evnts,""), design_code, svy_code)
#   
#   
#   # ###### lapply if subgrp is event or sop
#   # selected_sops <- "EXP"
#   # selected_events <- "TOT"
#   #
#   # if("sop"%in%subgrps)   selected_sops   <- c("SLF","PTR","MCR","MCD","OTZ")
#   # if("event"%in%subgrps) selected_events <- c("DVT","RX","OBV","OPT","ERT","IPT","HHT")
#   #
#   # run_analyses <- lapply(selected_events,
#   #                        function(x,sops=selected_sops){
#   #                          EXPs <- list(exp=sprintf("%s%s",x,sops) %>% paste0(collapse="+"),
#   #                                       pct=sprintf("(%s%s>0)",x,sops) %>% paste0(collapse="+"),
#   #                                       use=sprintf("(%s>0)",use_list[[x]]),
#   #                                       EXP=sprintf("%sEXP",x),
#   #                                       evtUSE="count_events",
#   #                                       evtEXP="XPX")
#   #
#   #                          stat_code(stat,true_subgrps,listEXP = EXPs)
#   #                        })
#   # #####
#   
#   
#   if(pretty) return(out %>% writeLines)
#   return(out)
# }

# #####################################################################
# #
# # stat = pctPOP
# # cols = 'diab_a1c'
# # rows = 'agegrps'
# # year = 2014
# #
# #
#
# # maybe throw this wherever code_string ends up
#
# dsgn_code <- function(stat,subgrps){
#
#   # change all of this to 'startswith' :
#   fycstats = c("totPOP","pctEXP","totEXP","meanEXP0","meanEXP","medEXP")
#   evntstats = c("totEVT","meanEVT")
#
#   if(stat %in% fycstats) return(snp_dsgn$fyc)
#   if(stat %in% evntstats) return(snp_dsgn$evnt)
#
#   if(any(subgrps %>% startsWith("diab"))) return(snp_dsgn$diab)
#   if(any(subgrps %>% startsWith("adult"))) return(snp_dsgn$saq)
#
#   return(snp_dsgn$fyc)
# }
#

#
# code_string_care <- function(care,grp,year){
#
#   this_year = meps_names %>% filter(Year==year)
#   file_names = as.list(this_year$url) %>% setNames(this_year$file_type)
#
#   yr <- substring(year,3,4);
#   yb <- substring(year-1,3,4)
#   ya <- substring(year+1,3,4)
#
#   load_fyc <- snp_load$load_fyc %>% rsub(file_names,yy=yr)
#
#   # Subgroup code
#
#   subgrps = gsub('_v2','',grp)
#   true_subgrps = subgrps[!subgrps %in% c("ind")]
#
#   if(any(c("agegrps","insurance","education","employed") %in% subgrps)) subgrps = c("agevar",subgrps)
#
#   subgrp_code <- c(snp_subgrps, snp_care)[subgrps] %>% paste0(collapse="\n")
#
#   # Define design
#
#   dsgn_code = snp_dsgn$fyc
#   if(care %>% startsWith("diab"))  dsgn_code = snp_dsgn$diab
#   if(care %>% startsWith("adult")) dsgn_code = snp_dsgn$saq
#
#   stat_code = care_code(care,grp,yr,force_svyby=F)
#
#   paste(load_fyc,subgrp_code,dsgn_code,stat_code,collapse="\n") %>%
#     rsub(yy=yr,ya=ya,yb=yb)
# }
#
#

# 