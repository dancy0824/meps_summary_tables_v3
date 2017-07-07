###############################################################
###                        INFO                             ###
###############################################################

info <- list(
  
  title = "Use and Expenditures",

  description = 
    "This interactive table has data on utilization and expenditures of care, including
      mean and median expenses per person, total number of healthcare events and average cost per event.
      Data can be grouped by event type (prescription medicines, dental services), source of payment (Medicare,
      Medicaid, etc.), or demographic characteristics (age, race, sex,...).",
  
  instructions = "Use the options below to select a
      variable of interest, as well as whether to view the data across time ('Trends over time') or in a single year ('Cross-sectional data').
      The 'Customize' dropdown allows additional stratification by grouping variables, and an
      option to view standard errors. Here are some extra words to make the introduction
      paragraph a bit longer..."
)


###############################################################
###                    DICTIONARIES                         ###
###############################################################

use_stats = list(
  "Population" = c(
    "Number of people" = "totPOP",
    "Percent of population with an expense (%)" = "pctEXP"
  ),
  "Expenditures" = c(
    "Total expenditures ($)"                        = "totEXP",
    "Mean expenditure per person ($)"               = "meanEXP0",
    "Mean expenditure per person with expense ($)"  = "meanEXP",
    "Median expenditure per person with expense ($)"= "medEXP"
  ),
  "Utilization" = c(
    "Number of events" = "totEVT",
    "Mean expenditure per event ($)" = "meanEVT"
  )
)

use_add <- list(
  "Event Type"  = "event",
  "Source of Payment"  = "sop")

use_subgrps <- append(subgrps,use_add,after=1)

shared_grps <- subgrps %>% unlist
local_grps <- use_add %>% unlist

stat_labels <- use_stats %>% invertList
grp_labels <- use_subgrps %>% invertList 
grp_labels$Year = "Year"


fyc_stats <- c(use_stats$Population,use_stats$Expenditures)
evnt_stats <- use_stats$Utilization

use_list = list(
  'TOT'='any_use','DVT'='DVTOT', 'RX' ='RXTOT',
  'OBV'='OBTOTV', 'OBD'='OBDRV', 'OBO'='OBOTHV',
  'OPT'='OPTOTV', 'OPY'='OPDRV', 'OPZ'='OPOTHV',
  'ERT'='ERTOT',  'IPT'='IPDIS',
  'HHT'='HHTOTD', 'HHA'='HHAGD', 'HHN'='HHINDD',
  'OMA'='OMAEXP')

sp_list = c("EXP"="XP","SLF"="SF","PTR"="PR",
            "MCR"="MR","MCD"="MD","OTZ"="OZ")

event_list = names(use_list)
sop_list = names(sp_list)

###############################################################
###                    CODE STRINGS                         ###
###############################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("r/stats.R")


r_code <- function(grps,stat,...){
  if(length(grps)==0) meps_code = meps_svy else meps_code = meps_svyby
  meps_code[[stat]] %>% 
    rsub(by = sprintf("~%s",paste0(grps,collapse="+")),...)
}

sas_code <- function(grps,stat,...){
  stat_code <- readCode(sprintf("sas/stats/%s.sas",stat))
# Adding 'ind' back in to make format and domain definitions easier
  if(length(grps)==0) grps <- 'ind'
  format <- paste(grps,paste0(grps,"."),collapse=" ")
  domain <- paste(grps,collapse="*")
  stat_code %>% rsub(type='sas',format = format,domain = domain,...)
}

svy_code <- function(grps,stat,yr,sop="EXP",event="TOT",display=F,lang="r"){
    lang <- tolower(lang)
    type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
    
  # Remove filler subgroups if using display
    gp <- grps
    if(display){
      if(type == "FYC") gp <- grps[!grps %in% c("ind","event","sop")]
      if(type == "EVNT") gp <- grps[!grps %in% c("ind","sop")]
    }
    
  # Set count and use variables if event or sop in grps
    count <- "PERWT.yy.F"
    if('event' %in% grps) count <- paste0(use_list[[event]],yr)
    if('sop' %in% grps) count <- paste0(event,sop,yr)
    
    sp <- sp_list[[sop]]
    use <- "(.sp..yy.X >= 0)"
    if('sop' %in% grps) use <- "(.sp..yy.X > 0)"

    if(lang=="r") code = r_code else 
      if(lang=='sas') code = sas_code else 
        stop("Language not available")
    
    code(grps=gp, stat=stat, count=count, use=use, event=event, sop=sop, sp=sp, yy=yr) 
}

#################################################################

get_r_code <- function(grps,stat,yr){

  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  
  code <- readCode(sprintf("../shared/%s/load_fyc.%s",lang,lang)) # Load FYC
  code <- paste0(code,"\n",subgrp_code(grps=grps,lang=lang)) # Define subgroups
  
  if('sop' %in% grps) sops = sop_list else sops = "EXP"
  
  # Run code
  if(type=="FYC"){
    
    if('event' %in% grps) events = event_list else events = "TOT"
    
    local_subgrps <- grps[grps %in% local_grps] %>% unique
    if(all(c("sop","event") %in% grps)) local_subgrps <- c("event_sop")
    
    code <- paste0(code,"\n",sapply(local_subgrps, function(x)
      readCode(sprintf("%s/grps/%s.%s",lang,x,lang))) %>%
      paste(collapse="\n"))
    
    # Loop over sops, events
    for(sop in sops){
      if('event'%in% grps) code <- paste0(code,"\n\n",sprintf("# Source of payment: %s",sop))
      for(event in events){
        code <- paste0(code,"\n",loop_code(grps,stat,yr,sop=sop,event=event,display=T))  
      }
    }
    
  }else if(type=="EVNT"){

    # Loop over sops
    for(sop in sops) code <- paste0(code,"\n",loop_code(grps,stat,yr,sop=sop,display=T))  
    if('event' %in% grps){
      grps_v2X <- grps %>% replace(grps=="event","event_v2X")
      for(sop in sops) code <- paste0(code,"\n",loop_code(grps_v2X,stat,yr,sop=sop,display=T))  
    }
    
  }
 
  return(code)
}



get_r_code(c("sop","sex"),"totPOP","05") %>% writeLines  




# SAS-specific
get_sas_code <- function(){
  
  macro_wrapper('totEVT',code=stat_code) %>% writeLines
  
}

macro_wrapper <- function(stat,code){
  params <- switch(stat,
                   'totPOP' = 'count',
                   'totEVT' = 'use,sp',
                   'meanEVT'= 'sp',
                   'event,sop')
  
  macro <- '%macro &stat.(&params.);\n&code.\n%mend;' 
  macro %>% rsub(type='sas',stat=stat,params=params,code=code)
}

macro_wrapper('totPOP',
              code=svy_code(grps,'totPOP',yr,sop="EXP",event="TOT",display=F,lang="sas")) %>%writeLines




get_sas_code <- function(grps,stat,yr){
  
  type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
  
  code <- readCode(sprintf("../shared/%s/load_fyc.%s",lang,lang)) # Load FYC
  code <- paste0(code,"\n",subgrp_code(grps=grps,lang=lang)) # Define subgroups
  
  if('sop' %in% grps) sops = sop_list else sops = "EXP"
  
  # Run code
  if(type=="FYC"){
    
    if('event' %in% grps) events = event_list else events = "TOT"
    
    local_subgrps <- grps[grps %in% local_grps] %>% unique
    if(all(c("sop","event") %in% grps)) local_subgrps <- c("event_sop")
    
    code <- paste0(code,"\n",sapply(local_subgrps, function(x)
      readCode(sprintf("%s/grps/%s.%s",lang,x,lang))) %>%
        paste(collapse="\n"))
    
    if(any(c('event','sop')%in%grps){
      # define macro
      inner <- svy_code(grps,stat,yr,sop=sop,event=event,display=T)
      macro <- macro_wrapper(stat,) ### ???
    }
    
    for(sop in sops){
      if('event'%in% grps) code <- paste0(code,"\n\n",sprintf("# Source of payment: %s",sop))
      for(event in events){
        code <- paste0(code,"\n",loop_code(grps,stat,yr,sop=sop,event=event,display=T))  
      }
    }
    
  }else if(type=="EVNT"){
    
    for(sop in sops) code <- paste0(code,"\n",loop_code(grps,stat,yr,sop=sop,display=T))  
    if('event' %in% grps){
      grps_v2X <- grps %>% replace(grps=="event","event_v2X")
      for(sop in sops) code <- paste0(code,"\n",loop_code(grps_v2X,stat,yr,sop=sop,display=T))  
    }
    
  }
  
  return(code)
}

