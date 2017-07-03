
print("Loading dictionaries.R")


abbrev = function(s){
  s %>%
    gsub("Emergency room","ER",.) %>%
    gsub("Medicines","Med.",.) %>%
    gsub("American","Amer.",.) %>%
    gsub("Multiple","Mult.",.) %>%
    gsub("Physician","Phys.",.) %>%
    gsub("physician","phys.",.) %>%
    gsub("Provider","Prov.",.) %>%
    gsub("Independent","Ind.",.) %>%
    gsub("Prescription","Presc.",.) %>%
    gsub("medicines","meds",.) %>%
    gsub("events","",.,ignore.case=T)
}

CFACT <- "Center for Financing, Access and Cost Trends"
AHRQ  <- "Agency for Healthcare Research and Quality"
MEPS  <- "Medical Expenditure Panel Survey"

controlTotals_message <- "(Standard errors are approximately zero for control totals)"

suppressed_message <- " -- Estimates suppressed due to inadequate precision (see <a  target='_blank_'
  href = 'https://meps.ahrq.gov/mepsweb/about_meps/faq_results.jsp?ChooseTopic=All+Categories&keyword=&Submit2=Search'>
  FAQs</a> for details).<br>"

rse_message <- " * Relative standard error is greater than 30%.<br>"

#################################
##     LOAD MEPS FILE NAMES    ##
#################################

  # yearlist = meps_names %>% filter(file_type=="FYC") %>% 
  #   select(Year) %>% arrange(-Year) %>% unlist %>% unique
  # yearlist = 2014:1996

#################################
##     FORMATTING FUNCTIONS    ##
#################################

  switchNames <- function(named_vector){
    inverted = names(named_vector)
    names(inverted) = named_vector
    return(inverted)
  }

  invertList <- function(list){
    for(i in 1:length(list)){
      if(length(list[[i]])>1) names(list)[i] = ""
    }
    list %>% unlist %>% switchNames
  }
 
#################################
##          SUBGROUPS          ##
################################# 
  
subgrps <- list(
  "(none)"             = "ind",
  "Demographics" = c(
    "Age Groups"         = "agegrps",
    "Census Region"      = "region",
    "Marital Status"     = "married",
    "Race/Ethnicity"     = "race",
    "Sex"                = "sex"
  ),
  "Health Variables" = c(
    "Insurance Coverage" = "insurance",
    "Perceived Health Status" = "health",
    "Perceived Mental Health" = "mental_health"
  ), 
  "Socio-Economic Status" = c(
    "Education"         = "education",
    "Employment Status" = "employed",
    "Poverty Status"    = "poverty"
  )
)  

# subgrp_totals <- list(
#   "ind" = "Total",
#   "event"         = "All Event Types",
#   "sop"           = "All Sources",
#   "agegrps"       = "All Age Groups",
#   "region"        = "All Regions",
#   "married"       = "All Marital Statuses",
#   "race"          = "All Races/Ethnicities",
#   "sex"           = "All Persons",
#   "insurance"     = "All Coverage Statuses",
#   "health"        = "All Health Statuses",
#   "mental_health" = "All Mental Health Statuses",
#   "education"     = "All Education Levels",
#   "employed"      = "All Employment Statuses",
#   "poverty"       = "All Poverty Levels"
# )

