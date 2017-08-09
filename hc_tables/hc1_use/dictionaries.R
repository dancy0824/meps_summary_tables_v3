
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

stat_labels <- use_stats %>% invertList %>% gsub(" \\(.*?\\)","",.)

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

get_caption <- function(stat_label,rows,cols,se_caption,year_caption){
  subgrp_caption <- get_subgrp_caption(rows,cols)
  add_caption <- ""
  if(grepl("number of people",tolower(stat_label))){
    if(grepl('event',tolower(subgrp_caption))) add_caption <- " with an event,"
    if(grepl('source of payment',tolower(subgrp_caption))) add_caption <- " with an expenditure"
  }
  sprintf("%s%s%s%s, United States, %s",stat_label,add_caption,se_caption,subgrp_caption,year_caption)
}

##################################################
###               FOR RUN_USE                  ###
##################################################

evnt_keys <-
  list("DV"="DVT","ER"="ERT","HH"="HHT","IP"="IPT",
       "OB"="OBV","OM"="OMA","OP"="OPT") %>% stack

event_dictionary <- 
  list("TOT"="All event types",
       "DVT"="Dental visits",
       "RX" ="Prescription medicines",
       "OBV"="Office-based events",
       "OBD"="Physician office visits",
       "OBO"="Non-physician office visits",
       "OPT"="Outpatient events",
       "OPY"="Physician hosp. visits",
       "OPZ"="Non-physician hosp. visits",
       "ERT"="Emergency room visits",
       "IPT"="Inpatient stays",
       "HHT"="Home health events",
       "OMA"="Other medical expenses") %>% stack

sop_dictionary <- 
  list("EXP"="All sources",
       "SLF"="Out of pocket",
       "PTR"="Private",
       "MCR"="Medicare",
       "MCD"="Medicaid",
       "OTZ"="Other") %>% stack
