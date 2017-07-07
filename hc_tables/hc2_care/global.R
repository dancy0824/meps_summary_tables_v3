
###############################################################
###                    DICTIONARIES                         ###
###############################################################

care_subgrps = list(
  
  "Access to Care" = c(
    "Persons with usual source of care"  = "usc",
    "Persons with difficulty receiving needed care" = "difficulty",
    "Reasons for difficulty receiving needed medical care" = "rsn_MD",
    "Reasons for difficulty receiving needed dental care" = "rsn_DN",
    "Reasons for difficulty receiving needed prescription medicines" = "rsn_PM"
  ),
  
  "Preventive Care" = c(
    "Adults advised to quit smoking" = "adult_nosmok",
    "Children receiving dental care" = "child_dental"
  ), 
  "Diabetes Care" = c(
    "Had H1b measurement" = "diab_a1c",
    "Had lipid profile" = "diab_chol",
    "Had eye exam"  = "diab_eye",
    "Had foot care" = "diab_foot",
    "Had flu shot" = "diab_flu"
  ),
  
  "Quality of Care: Adults" = c(
    "Ability to schedule a routine appointment" = "adult_routine",
    "Ability to schedule an appointment for illness or injury" = "adult_illness",
    "How often doctor listened carefully" = "adult_listen",
    "How often doctor explained things"   = "adult_explain",
    "How often doctor showed respect"     = "adult_respect",
    "How often doctor spent enough time"  = "adult_time",
    "Rating for care received" = "adult_rating"
  ),
  
  "Quality of Care: Children" = c(
    "Ability to schedule a routine appointment" = "child_routine",
    "Ability to schedule an appointment for illness or injury" = "child_illness",
    "How often doctor listened carefully" = "child_listen",
    "How often doctor explained things"   = "child_explain",
    "How often doctor showed respect"     = "child_respect",
    "How often doctor spent enough time"  = "child_time",
    "Rating for care received" = "child_rating"
  )
)

grp_labels <- subgrps %>% invertList %>% as.list  
care_labels <- c(care_subgrps %>% invertList) %>% as.list
care_labels$Year = "Year"

grp_labels <- c(grp_labels,care_labels)

stat_labels <- list("pctPOP"="Percent of population")


###############################################################
###                      FUNCTIONS                          ###
###############################################################

source("r/stats.R")

r_svy <- function(grps,stat,yr,display=F){
  
  # Remove filler subgroups if using display
  if(display) gp <- grps[!grps %in% c("ind")] else gp <- grps
  
  by <- paste0(gp,collapse="+")
  
  # Select svy or svyby depending on subgroups
  if(length(gp)==0) meps_code = meps_svy else meps_code = meps_svyby
  
  svy = stat
  if(!svy %in% names(meps_svyby)) svy = strsplit(stat,"_")[[1]][1]
  
  meps_code[[svy]] %>% rsub(by=sprintf("~%s",by),formula=stat)
}

