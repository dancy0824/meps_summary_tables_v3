
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