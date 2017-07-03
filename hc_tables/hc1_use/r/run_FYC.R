
survey <- function(grp1,grp2,stat,yr,event="TOT",sop="EXP",use="PERWT.yy.F"){
  runSource("stats.R",dir=path,verbose=F)
  svyString <- use_svyby[[stat]] %>% 
    rsub(
      by=sprintf("~%s+%s",grp1,grp2),
      event=event, sop=sop, use=use, yy=yr)
  
  cat(stat,":",svyString %>% writeLines)
  
  results <- run(svyString)
  results %>% standardize(grp1=grp1,grp2=grp2,stat=stat)
}

##################################################
###                  LISTS                     ###
##################################################

dir = "C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables"
shared = paste0(dir,"/shared/r")
path = paste0(dir,"/hc1_use/r")
tables = paste0(path,"/tables")
PUFdir = paste0(dir,"/shared/PUFS")

setwd(dir)

source(paste0(shared,"/run_global.R"),chdir=T) # shared

# Load packages
  runSource('load_pkg.R',dir=shared)

# Define lists (subgrp_list defined in shared/r/run_functions.R)
  usegrps = c("sop", "event", "event_sop")

  sop_list = c("EXP", "SLF", "PTR", "MCR", "MCD", "OTZ")

  use_list = list(
    'TOT'='any_use','DVT'='DVTOT', 'RX' ='RXTOT',
    'OBV'='OBTOTV', 'OBD'='OBDRV', 'OBO'='OBOTHV',
    'OPT'='OPTOTV', 'OPY'='OPDRV', 'OPZ'='OPOTHV',
    'ERT'='ERTOT',  'IPT'='IPDIS',
    'HHT'='HHTOTD', 'HHA'='HHAGD', 'HHN'='HHINDD',
    'OMA'='OMAEXP')
 
  event_list = names(use_list)
  
  stat_list = c("totPOP","pctEXP","totEXP","meanEXP0","meanEXP","medEXP","n","n_exp")
  
  year_list = 2014:1996
 
  
  year_list = 1999:1996

##################################################
###                   RUN                      ###
##################################################

for(year in year_list){  
  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  
  yr <- substring(year,3,4); 
  this_year <- meps_names %>% filter(Year==year) 
  file_names <- as.list(this_year$url) %>% setNames(this_year$file_type)

# Load data
  runSource('load_fyc.R',dir=shared,
            "year"=year,"yy"=yr,"PUFdir"=PUFdir,
            get_file_names(year))

# Add subgroups  
  for(grp in subgrps) runSource(sprintf("grps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in usegrps) runSource(sprintf("grps/%s.R",grp),dir=path,"yy"=yr)
  
# Define survey design  
  runSource("fyc_dsgn.R",dir=path,"yy"=yr)
  
# Run for each statistic  
  for(stat in stat_list){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
    
    # Demographic subgroups, crossed 
    #  Dear future Emily: don't try to change i and j back to all_subgrps.
    #  You will end up with many redundancies. Bad juju.
    
    for(i in 1:sl){   
      for(j in i:sl){ 
        grp1 = subgrp_list[i]; strp1 <- gsub("_v2X","",grp1); 
        grp2 = subgrp_list[j]; strp2 <- gsub("_v2X","",grp2);
        if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
        if(done(outfile,dir=tables,grp1=grp1,grp2=grp2)) next
        
        results <- survey(grp1=grp1,grp2=grp2,stat=stat,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }
    
    # Demographic subgroups x source of payment 
    for(grp1 in subgrp_list){   
      for(SOP in sop_list){ 
        if(done(outfile,dir=tables,grp1=grp1,levels2=SOP)) next
        
        FYCdsgn <- update(FYCdsgn,sop=SOP)
        use <- paste0("TOT",SOP,yr)
        results <- survey(grp1=grp1,grp2='sop',sop=SOP,stat=stat,use=use,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }
    
    # Demographic subgroups x event type
    for(grp1 in subgrp_list){   
      for(EVNT in event_list){ 
        if(done(outfile,dir=tables,grp1=grp1,levels2=EVNT)) next
        
        FYCdsgn <- update(FYCdsgn,event=EVNT)
        use <- paste0(use_list[[EVNT]],yr)
        results <- survey(grp1=grp1,grp2='event',event=EVNT,stat=stat,use=use,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }  
    
    # Source of payment x event type
    for(SOP in sop_list){   
      for(EVNT in event_list){ 
        if(done(outfile,dir=tables,levels1=SOP,levels2=EVNT)) next
        
        FYCdsgn <- update(FYCdsgn,event=EVNT,sop=SOP)
        use <- paste0(EVNT,SOP,yr)
        results <- survey(grp1='sop',grp2='event',sop=SOP,event=EVNT,stat=stat,use=use,yr=yr)
        update.csv(results,file=outfile,dir=tables)
      }
    }  
    
  }# end of stat_list loop
}# end of year_list loop
  
  
  