
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
    "H1b measurement" = "diab_a1c",
    "Lipid profile" = "diab_chol",
    "Eye exam"  = "diab_eye",
    "Foot care" = "diab_foot",
    "Flu shot" = "diab_flu"
  ),
  
  "Quality of Care: Adults" = c(
    "Ability to schedule a routine appointment" = "adult_routine",
    "Ability to schedule an appointment for illness or injury" = "adult_illness",
    "How often doctor listened carefully" = "adult_listen",
    "How often doctor explained things"   = "adult_explain",
    "How often doctor showed respect"     = "adult_respect",
    "How often doctor spent enough time"  = "adult_time",
    "Rating for care" = "adult_rating"
  ),
  
  "Quality of Care: Children" = c(
    "Ability to schedule a routine appointment" = "child_routine",
    "Ability to schedule an appointment for illness or injury" = "child_illness",
    "How often doctor listened carefully" = "child_listen",
    "How often doctor explained things"   = "child_explain",
    "How often doctor showed respect"     = "child_respect",
    "How often doctor spent enough time"  = "child_time",
    "Rating for care" = "child_rating"
  )
)

care_captions <- list(
  "usc" = "Percent of ",
  "difficulty" = "Percent of ",
  "rsn" = "",
  "adult_nosmok" = "Percent of ",
  "child_dental" = "Percent of ",
  
  "diab" = "Percent of adults with diabetes reporting ",
  "adult" = "Percent of adults reporting ",
  "child" = "Percent of children reporting "
)

grp_labels <- subgrps %>% invertList %>% as.list  
care_labels <- c(care_subgrps %>% invertList) %>% as.list
care_labels$Year = "Year"

grp_labels <- c(grp_labels,care_labels)

stat_labels <- list("pctPOP"="Percent of population")


get_caption <- function(stat_label,rows,cols,se_caption,year_caption){
  subgrp_caption <- get_subgrp_caption(rows)
  lookup = cols
  if(!cols %in% names(care_captions)) lookup = strsplit(lookup,"_")[[1]][1]
  care_caption <- care_captions[[lookup]]
  care_stat <- care_labels[[cols]] %>% tolower
  cap <- sprintf("%s%s%s%s, United States, %s",care_caption,care_stat,se_caption,subgrp_caption,year_caption)
  paste0(toupper(substr(cap,1,1)),substring(cap,2))
}

