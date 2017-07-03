###################################################
##          Dictionaries for Use app             ##
###################################################

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



