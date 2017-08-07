
###############################################################
###                      FUNCTIONS                          ###
###############################################################

source("r/stats.R")

get_r_code <- function(rows,cols,stat,year){
  
  yr <- substring(year,3,4)
  gp <- rows[!rows %in% c("ind")] 
  by <- paste0(gp,collapse="+")
  
  prefix = strsplit(cols,"_")[[1]][1]
  if(!cols %in% names(meps_svyby)) svy = prefix else svy = cols
  
  dsgn = switch(prefix,"adult"="design_saq","diab"="design_diab","design_fyc")

  code <- readSource('../shared/r/load/load_pkg.R')
  code <- code %>% add(readSource("../shared/r/load/load_fyc.R"))
  code <- code %>% add(subgrp_code(grps=rows,lang='r')) 
  code <- code %>% add(readSource(sprintf("r/grps/%s.R",cols))) 
  
  ## add relevant care codes
  
  code <- code %>% add(readSource(sprintf("../shared/r/svydesign/%s.R",dsgn)))
  
  # Select svy or svyby depending on subgroups
  if(length(gp)==0) meps_code = meps_svy[[svy]] else meps_code = meps_svyby[[svy]]
  
  out <- code %>% add(meps_code) %>% 
    rsub(PUFdir="C:/MEPS", get_file_names(year),year=year,yy=yr,
         FUN = 'svymean', by=by, formula=cols)
  out %>% writeLines
  
  out
}

#cols = 'adult_respect'
