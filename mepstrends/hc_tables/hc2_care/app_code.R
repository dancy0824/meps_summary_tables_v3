
###############################################################
###                      FUNCTIONS                          ###
###############################################################

source("r/stats.R")

freq <- '
    "4" = "Always",
    "3" = "Usually",
    "2" = "Sometimes/Never",
    "1" = "Sometimes/Never",
    "-7" = "Don\'t know/Non-response",
    "-8" = "Don\'t know/Non-response",
    "-9" = "Don\'t know/Non-response",
    "-1" = "Inapplicable"'

freq_fmt <- '
proc format;
  value freq
   4 = "Always"
   3 = "Usually"
   1 - 2 = "Sometimes/Never"
  -9 - -7 = "Don\'t know/Non-response"
  -1 = "Inapplicable";
run;
'


load_data <- function(rows,cols,year,lang="r"){
  
  if(lang=="r") LANG = "R" else LANG = lang
  
  year <- as.numeric(year)
  yr <- substring(year,3,4)
  yb <- substring(year-1,3,4)
  ya <- substring(year+1,3,4)
  
  prefix = strsplit(cols,"_")[[1]][1]
  
  load_sg = rows
  load_agevar = (prefix %in% c("adult","child") & !(load_sg %in% age_subgrps))
  if(load_agevar) load_sg <- c("agevar",load_sg)
  
  code <- readSource(sprintf("../shared/%s/load/load_fyc.%s",lang,LANG))
  code <- code %>% add(subgrp_code(grps=load_sg,lang=lang)) 
  code <- code %>% add(readSource(sprintf("%s/grps/%s.%s",lang,cols,LANG))) 
  
  code %>% rsub(type=lang,year=year,yy=yr,ya=ya,yb=yb)
}

##############################################

get_r_code <- function(rows,cols,stat="",year=2014){

  yr <- substring(year,3,4)
  gp <- rows[rows != "ind"] 
  by <- paste0(gp,collapse="+")
  
  prefix = strsplit(cols,"_")[[1]][1]
  if(!cols %in% names(meps_svyby)) svy = prefix else svy = cols

  dsgn = switch(prefix,"adult"="design_saq","diab"="design_diab","design_fyc")

  code <- readSource('../shared/r/load/load_pkg.R')
  code <- code %>% add(load_data(rows,cols,year,lang="r"))
  code <- code %>% add(readSource(sprintf("../shared/r/svydesign/%s.R",dsgn)))
  
  # Select svy or svyby depending on subgroups
  if(length(gp)==0) meps_code = meps_svy[[svy]] else meps_code = meps_svyby[[svy]]
  
  code %>% add(meps_code) %>% 
    rsub(PUFdir="C:/MEPS", get_file_names(year),yy=yr,FUN='svymean', by=by, formula=cols,freq=freq)
}


get_sas_code <- function(rows,cols,stat="",year=2014){
  yr <- substring(year,3,4)
  
  code <- load_data(rows,cols,year,lang="sas")
  
  rs <- rows[rows != 'ind']
  if(length(rs) > 0){
    fmt <- paste(rs,paste0(rs,"."),collapse=" ")
    grp <- paste0(rs,"*")
    gp <- rs
    where <- sprintf("and %s ne .",gp)
  }else{
    fmt <- grp <- gp <- where <- ""
  } 
  
  code %>%
    rsub(type='sas',PUFdir="C:\\\\MEPS", get_file_names(year),
         yy=yr,fmt=fmt,where=where,grp=grp,gp=gp,freq_fmt=freq_fmt)
}
