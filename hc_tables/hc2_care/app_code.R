
###############################################################
###                      FUNCTIONS                          ###
###############################################################

source("r/stats.R")

r_svy <- function(grps,stat,yr,display=F){
  
  # Remove filler subgroups if using display
  if(display) gp <- grps[!grps %in% c("ind")] else gp <- grps
  
  by <- paste0(gp,collapse="+")
  
  # Select svy or svyby depending on subgroups
  if(length(gp)==0) meps_code = meps_svy else meps_code = meps_svyby
  
  svy = stat
  if(!svy %in% names(meps_svyby)) svy = strsplit(stat,"_")[[1]][1]
  
  meps_code[[svy]] %>% rsub(by=sprintf("~%s",by),formula=stat)
}



get_r_code <- function(grps,stat,yr){
  r_svy(grps,stat,yr,display=T)
}