
###################################################
###  Use and Expenditures -- estimate functions ###
###################################################

use_fun <- function(code_fun,stats,...,grp1="ind",grp2="ind"){
  output <- lapply(stats,function(x){
    code_s <- code_fun(x,subgrps=c(grp1,grp2),...,force_svyby = T); 
    writeLines(code_s);est <- eval(parse(text=code_s));
    est %>% std(key=x)
  }) %>% join_all
  
  args = list(...)
  if('sop' %in% names(args)) output$sop = args$sop
  if('event' %in% names(args)) output$event = args$event
  output %>% mutate(grp1=grp1,grp2=grp2) %>%
    mutate_(levels1=grp1,levels2=grp2) %>%
    select(grp1,grp2,levels1,levels2,one_of(stats),one_of(paste0(stats,"_se")))
}

#####################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../../run_preamble.R",chdir=T) # shared

source("use_dictionaries.R")
source("use_snippets.R")



redo_subgrps = NULL

# redo employed for FYC & EVNT -- 1996 only
# redo_subgrps = c("employed")
# 
# redo_fyc  = TRUE
# redo_evnt = TRUE
# 
# yearlist = 1996


# redo OBD and OBV for all years (start with 2014)
redo_subgrps = c("event","event_v2")
redo_fyc  = TRUE
redo_evnt = TRUE
event_list = c("OBV","OBD","OBO")

yearlist = 2006:2007

# redo OBD and OBV for all years (start with 2014)
# redo_subgrps = c("employed","event","event_v2")


for(year in yearlist){  print(year)
  
  # year = 2014
  
  yr <- substring(year,3,4); 
  
  this_year = meps_names %>% filter(Year==year) 
  file_names = as.list(this_year$url) %>% setNames(this_year$file_type)
  
  fyc_csv <- sprintf("tables/FYC%s.csv",year)
  evt_csv <- sprintf("tables/EVT%s.csv",year)
  
  # Load data
    load_fyc <- snp_load$load_fyc %>% rsub(file_names,yy=yr)
    writeLines(load_fyc); eval(parse(text = load_fyc));

  # Add subgroup variables to FYC data
    for(snp in snp_subgrps) eval(parse(text = snp %>% rsub(yy=yr)))
    for(snp in snp_use) eval(parse(text = snp %>% rsub(yy=yr)))

  # Load EVENTS and merge with FYC 
    sg_comma = paste0(paste(all_subgrps,collapse=","),",")
    
    load_evt <- snp_load$load_events %>% rsub(file_names,yy=yr,subgrps=sg_comma)
    writeLines(load_evt); 
    eval(parse(text = load_evt));
    
  # Define design
    eval(parse(text=snp_dsgn$fyc %>% rsub(yy=yr)))
    eval(parse(text=snp_dsgn$evnt %>% rsub(yy=yr)))

###  Run estimates ### 

  # Subgroups x Subgroup
    
    # Dear future Emily: don't try to change i and j back to all_subgrps.
    #  You will end up with many redundancies. Bad juju.
    
    for(i in 1:sl){   
     for(j in i:sl){ 
       grp1 = all_subgrps[i]; strp1 <- gsub("_v2","",grp1); 
       grp2 = all_subgrps[j]; strp2 <- gsub("_v2","",grp2);

       force = (any(c(grp1,grp2) %in% redo_subgrps)) 
      
       if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
         run(use_fun, fyc_csv, code_fun=fyc_code,  stats=fyc_stats,  grp1=grp1, grp2=grp2, yr=yr, force=(force&redo_fyc))
         run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1=grp1, grp2=grp2, yr=yr, force=(force&redo_evnt))
     }
    }
 
  # Source of Payment x Subgroup
    for(grp1 in all_subgrps){
      
      force = (any(c(grp1,'sop') %in% redo_subgrps))
      
      for(sop in sop_list){ 
        run(use_fun, fyc_csv, code_fun=fyc_code,  stats=fyc_stats,  grp1=grp1, grp2='sop', sop=sop, yr=yr, force=(force&redo_fyc))
        run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1=grp1, grp2='sop', sop=sp_list[[sop]],  yr=yr, force=(force&redo_evnt))
      }
    }

  # Event Type x Subgroup
    for(grp1 in all_subgrps){
      
      force <- (any(c(grp1,'event') %in% redo_subgrps))
      
      for(event in event_list){ 
        run(use_fun, fyc_csv, code_fun=fyc_code, stats=fyc_stats,  grp1=grp1, grp2='event', event=event, yr=yr, force=(force&redo_fyc))
      }
      
      run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1=grp1, grp2='event_v2', yr=yr, force=(force&redo_evnt))
      run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1=grp1, grp2='event',    yr=yr, force=(force&redo_evnt))
    }

  # Event type x SOP
    for(sop in sop_list){
      
      force <- (any(c('sop','event') %in% redo_subgrps))
      
      for(event in event_list){ 
        run(use_fun, fyc_csv, code_fun=fyc_code, stats=fyc_stats,  grp1='sop', grp2='event',  sop=sop, event=event, yr=yr, force=(force&redo_fyc))
      }
      run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1='sop', grp2='event_v2',sop=sp_list[[sop]], yr=yr, force=(force&redo_evnt))
      run(use_fun, evt_csv, code_fun=evnt_code, stats=evnt_stats, grp1='sop', grp2='event',   sop=sp_list[[sop]], yr=yr, force=(force&redo_evnt))
    }
 
} # end of yearlist loop

