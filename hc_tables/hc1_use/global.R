
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
    "Total number of events" = "totEVT",
    "Average events per person" = "avgEVT",
    "Mean expenditure per event ($)" = "meanEVT"
  )
)

use_add <- list(
  "Event Characteristics" = c(
    "Event Type"  = "event",
    "Source of Payment"  = "sop"))

use_subgrps <- append(subgrps,use_add,after=1)

shared_grps <- subgrps %>% unlist
local_grps <- use_add %>% unlist

stat_labels <- use_stats %>% invertList
grp_labels <- use_subgrps %>% invertList %>% as.list
grp_labels$Year = "Year"


fyc_stats <- c(use_stats$Population,use_stats$Expenditures)
evnt_stats <- use_stats$Utilization

use_list = list(
  'TOT'='TOTUSE', 'DVT'='DVTOT', 'RX' ='RXTOT',
  'OBV'='OBTOTV', 'OBD'='OBDRV', 'OBO'='OBOTHV',
  'OPT'='OPTOTV', 'OPY'='OPDRV', 'OPZ'='OPOTHV',
  'ERT'='ERTOT',  'IPT'='IPDIS',
  'HHT'='HHTOTD', #'HHA'='HHAGD', 'HHN'='HHINDD',
  'OMA'='OMAEXP')

sp_list = c("EXP"="XP","SLF"="SF","PTR"="PR",
            "MCR"="MR","MCD"="MD","OTZ"="OZ")

event_list = names(use_list)
sop_list = names(sp_list)



###############################################################
###                      FUNCTIONS                          ###
###############################################################

source("r/stats/stats.R")

r_svy <- function(grps,stat,yr,sop="EXP",event="TOT",display=F,verbose=T){
  
  # Remove filler subgroups if using display
    gp <- grps  
    type <- ifelse(stat %in% evnt_stats,"EVNT","FYC")
    if(display){
      if(type == "FYC") gp <- grps[!grps %in% c("ind","event","sop")]
      if(type == "EVNT") gp <- grps[!grps %in% c("ind","sop")]
    }
  
  # Set pre-code if stat == 'avgEVT'
    precode <- ""
    if(stat == 'avgEVT'){
      extension = grps[grps %in% c("sop","event")] %>% sort
      codeName = paste(c("avgEVT",extension),collapse="_")
      if(display) precode <- readSource(sprintf("stats/%s.R",codeName),verbose=F)
      if('event' %in% grps){
        stat = "avgEVT_event"
        gp = gp[gp!="event"]
      }
    }
  
  # Set count and use variables if event or sop in grps
    count <- "PERWT.yy.F"
    if('event' %in% grps) count <- paste0(use_list[[event]],yr)
    if('sop' %in% grps) count <- paste0(event,sop,yr)
    
    sp <- sp_list[[sop]]
    use <- "(.sp..yy.X >= 0)"
    if('sop' %in% grps) use <- "(.sp..yy.X > 0)"
  
  if(length(gp)==0) meps_code = meps_svy else meps_code = meps_svyby
  
  code <- meps_code[[stat]] 
  
  out <- paste(precode,code) %>% 
    rsub(by = sprintf("~%s",paste0(gp,collapse="+")),
         subgrps = paste(paste0(gp,collapse=", "),","),
         grps=gp, stat=stat, count=count, use=use, 
         event=event, sop=sop, sp=sp, yy=yr) 
  
  if(verbose) out %>% writeLines
  return(out)
}


