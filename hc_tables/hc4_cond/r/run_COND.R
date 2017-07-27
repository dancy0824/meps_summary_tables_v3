##################################################
###            MEDICAL CONDITIONS              ###
##################################################

gather_sop <- function(df){
  df %>% 
    gather(group,coef,-Condition) %>%
    mutate(group = gsub(" > 0TRUE","",group)) %>%
    separate(group,c("stat","sop"),sep="\\.",fill="left") %>%
    mutate(stat = replace(stat,is.na(stat),"stat")) %>%
    spread(stat,coef) %>%
    mutate(sop = substr(sop,1,2))
}

standardize <- function(df,grp1,grp2,stat,type=""){
  out <- df %>% select(-contains("FALSE")) 
  
  if(type=="sop") out <- gather_sop(out)
  
  key = c(stat,paste0(stat,"_se"))
  names(out)[!names(out) %in% c(grp1,grp2)] = key
  
  out %>%
    mutate(grp1=grp1,grp2=grp2) %>%
    mutate_(levels1=grp1,levels2=grp2) %>%
    select(grp1,grp2,levels1,levels2,one_of(key))
}

##################################################
###                  LISTS                     ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

app <- "hc4_cond"

# Shared
source("../../shared/global.R")
source("../../shared/r/run_preamble.R",chdir=T)

# Local
# source("../global.R",chdir=T) -- doesn't exist yet

source("stats.R")

##################################################
###                   RUN                      ###
##################################################

# Load packages
runSource('load/load_pkg.R',dir=shared)

args = commandArgs(trailingOnly = TRUE)

if(length(args) > 0){
  year_start = as.numeric(args[1])
  year_end = as.numeric(args[2])
}else{
  year_start = min(meps_names$Year)
  year_end = max(meps_names$Year)
}

year_list = year_start:year_end

# year = 2013
# year_list = 2014:1996

for(year in year_list){ 

  dir.create(sprintf('%s/%s',tables,year), showWarnings = FALSE)
  yr <- substring(year,3,4); 
  sg = paste0(subgrp_list,",",collapse="")
  
  # Load data
  runSource('load/load_fyc.R', dir=shared,year=year,yy=yr,PUFdir=PUFdir,get_file_names(year))
  
  # Add subgroups 
  for(grp in subgrp_load) runSource(sprintf("subgrps/%s.R",grp),dir=shared,"yy"=yr)
  
  # Load EVENTS and merge with FYC 
  runSource('load_cond.R',yy=yr,PUFdir=PUFdir,subgrps=sg, get_file_names(year))
  
  # Define survey designs
  runSource("design/EVNTdsgn.R",yy=yr) # event-level
  runSource("design/PERSdsgn.R",yy=yr,subgrps=sg) # person-level
  runSource("design/PERSevnt.R",yy=yr,subgrps=sg) # person-level, by event type
  
  ####
  cond_stats = list(
      "Number of people with care" = "totPOP",
      "Number of events"           = "totEVT",
      "Total expenditures ($)"     = "totEXP",
      "Mean expenditure per person with care ($)"= "meanEXP"
  )
  ###

  for(stat in cond_stats){
    
    outfile <- sprintf("%s/%s.csv",year,stat)
  
    # by demographic subgroups
      for(grp1 in subgrp_list){
        if(done(outfile,dir=tables,grp1=grp1,grp2="Condition")) next
        by = sprintf("~Condition+%s",grp1)
        out <- meps_demo[[stat]] %>% rsub(by=by,subgrps=sg,yy=yr) %>% run
        results <- out %>% standardize(grp1,"Condition",stat)
        update.csv(results,file=outfile,dir=tables)
      }

    # by event type
      if(!done(outfile,dir=tables,grp1="event",grp2="Condition")){
        out <- meps_event[[stat]] %>% rsub(by=by,yy=yr) %>% run
        results <- out %>% standardize("event","Condition",stat)
        update.csv(results,file=outfile,dir=tables)
      }
      
    # by SOP
      if(!done(outfile,dir=tables,grp1="sop",grp2="Condition")){
        out <- meps_sop[[stat]] %>% rsub(by=by,yy=yr)  %>% run
        results <- out %>% standardize("sop","Condition",stat,type="sop")
        update.csv(results,file=outfile,dir=tables)
      }
  }
  
}   

      # by = "~Condition+.grp." # (or "~Condition" if grp=='ind' )
      # svyby(~ind, by = .by., FUN=svytotal,design=PERSdsgn) # totPOP
      # svyby(~ind, by = .by., FUN=svytotal,design=EVNTdsgn) # totEVT
      # svyby(~XP13X, by = .by., FUN=svytotal,design=PERSdsgn) # totEXP
      # svyby(~XP13X, by = .by., FUN=svymean,design=PERSdsgn)  # meanEXP
      
    
      # svyby(~ind, by = ~Condition+event,FUN=svytotal,design=PERSevnt) # totPOP
      # svyby(~ind, by = ~Condition+event,FUN=svytotal,design=EVNTdsgn) # totEVT
      # svyby(~XP13X, by = ~Condition+event,FUN=svytotal,design=PERSevnt) # totEXP
      # svyby(~XP13X, by = ~Condition+event,FUN=svymean, design=PERSevnt) # meanEXP
      
  
      
      # svyby(~(XP13X>0)+(SF13X>0)+(MR13X>0)+(MD13X>0)+(PR13X>0)+(OZ13X>0),
      #       by = ~Condition,FUN=svytotal,design=PERSdsgn) # totPOP
      # 
      # svyby(~(XP13X>0)+(SF13X>0)+(MR13X>0)+(MD13X>0)+(PR13X>0)+(OZ13X>0), 
      #       by = ~Condition,FUN=svytotal,design=EVNTdsgn) # totEVT
      # 
      # svyby(~XP13X+SF13X+MR13X+MD13X+PR13X+OZ13X,
      #       by = ~Condition,FUN=svytotal,design=PERSdsgn) # totEXP
      # 
      # svyby(~XP13X+SF13X+MR13X+MD13X+PR13X+OZ13X,
      #       by = ~Condition,FUN=svymean,design=PERSdsgn) # meanEXP
      
      
  # # Table 1: Number of persons with care
  #   out1a <- svyby(~ind,by = ~Condition,FUN=svytotal,design=PERSdsgn) # all events
  #   out1b <- svyby(~ind,by = ~Condition+event,FUN=svytotal,design=PERSevnt)   # by event type
  
  # # Table 2: Number of events (by event type)
  #   out2a <- svyby(~ind,by = ~Condition,FUN=svytotal,design=EVNTdsgn) # all events
  #   out2b <- svyby(~ind,by = ~Condition+event,FUN=svytotal,design=EVNTdsgn)     # by event type
 
  # # Table 3: Total expenses
  #   out3.1 <- svyby(~XP13X,by = ~Condition,FUN=svytotal,design=EVNTdsgn) # all events
  #   out3.2 <- svyby(~XP13X,by = ~Condition+event,FUN=svytotal,design=EVNTdsgn)  # by event type
    
  # # Table 3a: Mean expenses per person with care
  #   out3a.1 <- svyby(~XP13X,by = ~Condition,FUN=svymean,design=PERSdsgn) # all events
  #   out3a.2 <- svyby(~XP13X,by = ~Condition+event,FUN=svymean,design=PERSevnt)  # by event type
      
  # # Table 4: Total expenses
  #   # All sop (see above)
  #   out4 <- svyby(~XP13X+SF13X+MR13X+MD13X+PR13X+OZ13X,by = ~Condition,FUN=svytotal,design=EVNTdsgn)  # by SOP
        
     # round(out2[,c("SF13X","PR13X","MR13X","MD13X","OZ13X")]/out2$XP13X*100,1)
      
    # out1a %>% 
    #   full_join(out2a,by="Condition",suffix=c(".1",".2")) %>% 
    #   full_join(out3.1,by="Condition",suffix=c("",".3")) %>% 
    #   full_join(out3a.1,by="Condition",suffix=c("",".3a"))
    #   
    # out1b %>% 
    #   full_join(out2b,by=c("Condition","event"),suffix=c(".1",".2")) %>% 
    #   full_join(out3.2,by=c("Condition","event"),suffix=c("",".3")) %>% 
    #   full_join(out3a.2,by=c("Condition","event"),suffix=c("",".3a"))
    # 
    # out4
    # 
    
    
      
  
  