#############################################
###     UTILIZATION AND EXPENDITURES      ###
#############################################

print("initiating run_USE.R")
Sys.sleep(1)

get_names <- function(vec){
  out <- vec %>% grep(" = ",.,value=T) %>% sub(' = .*$','',.)
  
  # Need to make sure we have the right key variables, esp. for avgEVT
  event_check <- substring(out,1,2) %in% substring(event_list,1,2)
  sop_check <- substring(out,nchar(out)-2,nchar(out)) %in% sop_list
  
  out[event_check&sop_check]
}

gather_sop <- function(df,grps){
  df %>% 
    gather_("group","coef",setdiff(names(.),c(grps,"ind"))) %>%
    separate(group,c("stat","sop"),sep="\\.",fill="left") %>%
    mutate(stat = replace(stat,is.na(stat),"stat")) %>%
    spread(stat,coef) %>%
    select(one_of(grps),sop,stat,se)
}

standardize <- function(results,grp1,grp2,stat){
  out <- results %>% select(-contains("FALSE")) 
  key = c(stat,paste0(stat,"_se"))
  names(out)[!names(out) %in% c("ind","sop","event",grp1,grp2)] = key
  out %>%
    mutate(grp1=grp1,grp2=grp2) %>%
    mutate_(levels1=grp1,levels2=grp2) %>%
    select(grp1,grp2,levels1,levels2,one_of(key))
}

##################################################
###                  LISTS                     ###
##################################################
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc1_use"

source("../run_preamble.R",chdir=T)

##################################################
###                   RUN                      ###
##################################################
# From initial run, .bat file:
#
# args = commandArgs(trailingOnly = TRUE)
# if(length(args) > 0){
#   year_start = as.numeric(args[1])
#   year_end = as.numeric(args[2])
# }else{
#   year_start = min(meps_names$Year)
#   year_end = max(meps_names$Year)
# }
# year_list = year_start:year_end


# Load packages
  runSource('load/load_pkg.R',dir=shared)

for(year in year_list){   print(year)
  
  done_file = sprintf("tables/%s/_DONE.Rdata",year)
  if(file.exists(done_file)) next
  
  dir.create(sprintf('tables/%s',year), showWarnings = FALSE)
  yr <- substring(year,3,4)
  sg <- paste0(subgrp_list,",",collapse="")

# Load data
  runSource('load/load_fyc.R',dir=shared,year=year,yy=yr,PUFdir="C:/MEPS",get_file_names(year))

# Add subgroups  
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,"yy"=yr)
  for(grp in c("sop","event","event_sop")) runSource(sprintf("grps/%s.R",grp),dir=path,"yy"=yr)

# Load EVENTS and merge with FYC 
  runSource('load_events.R',dir=path,yy=yr,PUFdir="C:/MEPS",subgrps=sg,get_file_names(year))
  runSource('merge_events.R',dir=path,yy=yr,subgrps=sg)

# Define survey design  
  runSource("svydesign/design_fyc.R",dir=shared,"yy"=yr)
  runSource("svydesign/design_evnt.R",dir=shared,"yy"=yr)
  runSource("stats/avgEVT.R",dir=path,"yy"=yr)
  
# Run for each statistic  
  for(stat in c(fyc_stats,evnt_stats)){ # fyc_stats includes n, n_exp
    
    outfile <- sprintf("%s/%s.csv",year,stat)

    grp_list = c(subgrp_list,"sop","event")
    if(stat %in% c("totEVT","meanEVT")) grp_list = c(grp_list,"event_v2X")
    
    sl = length(grp_list)
    
    #  Dear future Emily: don't try to change i and j back to grp_list
    #  You will end up with many redundancies. Bad juju.
    for(i in 1:sl){   
      for(j in i:sl){ 
        
        grp1 = grp_list[i]; strp1 <- gsub("_v2X","",grp1); 
        grp2 = grp_list[j]; strp2 <- gsub("_v2X","",grp2);
        if(strp1==strp2 & grp1!="ind") next   # skip if grp1 = grp2 (but run for ind, ind)
        if(done(outfile,dir="tables",grp1=grp1,grp2=grp2)) next
 
        grps = c(grp1,grp2)

        out_code <- capture.output(get_r_code(rows=grp1,cols=grp2,stat=stat,year=year,display=F) %>% run)
    
        for(nm in get_names(out_code)){

          ev = substr(nm,1,2);
          if(ev == "RX"){evt = ev; sp = substr(nm,3,5);
          }else{evt = substr(nm,1,3); sp = substr(nm,4,6);}
          
          out <- get(nm) 
          
          if(stat == "avgEVT" & 'sop' %in% grps) out <- out %>% gather_sop(grps) else out <- out %>% mutate(sop=sp)
          if(!stat %in% c("totEVT","meanEVT")) out <- out %>% mutate(event = evt)
          out <- out %>% mutate(ind="Total")

          results <- out %>% standardize(grp1,grp2,stat)
          print(results)
          update.csv(results,file=outfile,dir="tables")
        }
        
      }
    }
    
  } # end of stat_list loop

  all_done = TRUE; save(all_done,file=done_file);
  
}# end of year_list loop
  

print("...use estimates completed")
Sys.sleep(1)