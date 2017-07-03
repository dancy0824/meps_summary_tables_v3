
print("loading snippets4_care.R")

# QUALITY OF CARE
care_code <- function(care,grp='ind',yr,force_svyby=F,type='svymean'){
  care_list <- care_svy[[care]]
  
  if(!'formula' %in% names(care_list)) care_list$formula = care
  
  code_s = svyby_s %>% rsub(by=grp,FUN=type) %>% rsub(care_list) 
  code_s %>% writeLines;
  code_s
}

###############################################################

adult_had_appt = "subset(SAQdsgn, ADAPPT42 >= 1 & AGELAST >= 18)"
child_had_appt = "subset(FYCdsgn, CHAPPT42 >= 1 & AGELAST < 18)"

care_svy <- list(
  
 # Access to care
  
  'usc' = list(formula='usc',FUN='svymean',design='subset(FYCdsgn, ACCELI42==1 & HAVEUS42 >= 0 & LOCATN42 >= -1)'),
  'difficulty' = list(formula='delay_ANY + delay_MD + delay_DN + delay_PM',design='subset(FYCdsgn, ACCELI42==1)'),
  
  'rsn_MD' = list(formula = 'afford_MD + insure_MD + other_MD',design='subset(FYCdsgn, ACCELI42==1 & delay_MD==1)'),
  'rsn_DN' = list(formula = 'afford_DN + insure_DN + other_DN',design='subset(FYCdsgn, ACCELI42==1 & delay_DN==1)'),
  'rsn_PM' = list(formula = 'afford_PM + insure_PM + other_PM',design='subset(FYCdsgn, ACCELI42==1 & delay_PM==1)'),

 # Diabetes Care
  
  'diab_a1c'  = list(design='DIABdsgn'),
  'diab_chol' = list(design='DIABdsgn'),
  'diab_eye'  = list(design='DIABdsgn'),
  'diab_foot' = list(design='DIABdsgn'),
  'diab_flu'  = list(design='DIABdsgn'),
  
 # Quality of Care
 
  'adult_nosmok'  = list(design = 'subset(SAQdsgn, ADSMOK42==1 & CHECK53==1)'),
  'adult_routine' = list(design = 'subset(SAQdsgn, ADRTCR42==1 & AGELAST >= 18)'),
  'adult_illness' = list(design = 'subset(SAQdsgn, ADILCR42==1 & AGELAST >= 18)'),
  
  'child_dental'  = list(design = 'subset(FYCdsgn, child_2to17==1)'),
  'child_routine' = list(design = 'subset(FYCdsgn, CHRTCR42==1 & AGELAST < 18)'),
  'child_illness' = list(design = 'subset(FYCdsgn, CHILCR42==1 & AGELAST < 18)'),
  
  'adult_listen'  = list(design = adult_had_appt),
  'adult_explain' = list(design = adult_had_appt),
  'adult_respect' = list(design = adult_had_appt),
  'adult_rating'  = list(design = adult_had_appt),
  'adult_time'    = list(design = adult_had_appt),
  
  'child_listen'  = list(design = child_had_appt),
  'child_explain' = list(design = child_had_appt),
  'child_respect' = list(design = child_had_appt),
  'child_rating'  = list(design = child_had_appt),
  'child_time'    = list(design = child_had_appt)

)


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



#################################################
###       DIABETES CARE: QofC, 1.1-1.5        ###
#################################################

# Note: MUST have factor variables so svyby doesn't screw up

snp_care <- list()

snp_care$diab_a1c <-'
FYC <- FYC %>% 
  mutate(diab_a1c = ifelse(0 < DSA1C53 & DSA1C53 < 96, 1, DSA1C53)) %>%
  mutate(diab_a1c = replace(diab_a1c,DSA1C53==96,0)) %>%
  mutate(diab_a1c = recode_factor(diab_a1c,
           "1" = "Had measurement",
           "0" = "Did not have measurement",
          "-8" = "Don\'t know",
          "-9" = "Non-response",
          "-1" = "Inapplicable"))
'

snp_care$diab_chol <-'
if(year > 2007){
    FYC <- FYC %>% 
      mutate(
        past_year = (DSCH&yy.53==1 | DSCH&ya.53==1),
        more_year = (DSCH&yb.53==1 | DSCB&yb.53==1),
        never_chk = (DSCHNV53 == 1),
        dontknow  = (DSCH&yy.53 == -8),
        non_resp  = (DSCH&yy.53 %in% c(-7,-9))
      )
  }else{
    FYC <- FYC %>% 
      mutate(
        past_year = (CHOLCK53 == 1),
        more_year = (1 < CHOLCK53 & CHOLCK53 < 6),
        never_chk = (CHOLCK53 == 6),
        dontknow  = (CHOLCK53 == -8),
        non_resp  = (CHOLCK53 %in% c(-7,-9))
      )
  }

FYC <- FYC %>% 
  mutate(
    diab_chol = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$never_chk ~ "Never had cholesterol checked",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing")) 
   )

'

snp_care$diab_eye <-'
if(year > 2001){
  FYC <- FYC %>% 
    mutate(past_year = (DSEY&yy.53==1 | DSEY&ya.53==1),
           more_year = (DSEY&yb.53==1 | DSEB&yb.53==1),
           never_chk = (DSEYNV53 == 1),
           non_resp =( DSEY&yy.53== -9)
    )
}else{
  FYC <- FYC %>%
    mutate(
      past_year = (DSEYE53 %in% c(1,2)),
      more_year = (DSEYE53 %in% c(3,4)),
      never_chk = (DSEYE53 == 5),
      dontknow  = (DSCKFT53 == -8),
      non_resp  = (DSCKFT53 %in% c(-7,-9))
    )
}

FYC <- FYC %>% 
  mutate(
    diab_eye = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$never_chk ~ "Never had eye exam",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing"))
  )
'

snp_care$diab_foot <-'
if(year > 2007){
  FYC <- FYC %>% 
    mutate(
      past_year = (DSFT&yy.53==1 | DSFT&ya.53==1),
      more_year = (DSFT&yb.53==1 | DSFB&yb.53==1),
      never_chk = (DSFTNV53 == 1),
      dontknow  = (DSFT&yy.53 == -8),
      non_resp  = (DSFT&yy.53 %in% c(-7,-9)),
      not_past_year = FALSE
    )
}else{
  FYC <- FYC %>% 
    mutate(
      past_year = (DSCKFT53 == 1),
      more_year = FALSE,
      never_chk = FALSE,
      not_past_year = (DSCKFT53 == 0),
      dontknow  = (DSCKFT53 == -8),
      non_resp  = (DSCKFT53 %in% c(-7,-9))
    )
}

FYC <- FYC %>% 
  mutate(
    diab_foot = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$not_past_year ~ "Never had feet checked",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing"))
    )
'


snp_care$diab_flu <-'
if(year > 2007){
  FYC <- FYC %>% 
    mutate(
      past_year = (DSFL&yy.53==1 | DSFL&ya.53==1),
      more_year = (DSFL&yb.53==1 | DSVB&yb.53==1),
      never_chk = (DSFLNV53 == 1),
      dontknow  = (DSFL&yy.53 == -8),
      non_resp  = (DSFL&yy.53 %in% c(-7,-9))
   )
}else{
  FYC <- FYC %>% 
    mutate(
      past_year = (FLUSHT53 == 1),
      more_year = (1 < FLUSHT53 & FLUSHT53 < 6),
      never_chk = (FLUSHT53 == 6),
      dontknow  = (FLUSHT53 == -8),
      non_resp  = (FLUSHT53 %in% c(-7,-9))
  )
}

FYC <- FYC %>% 
  mutate(
    diab_flu = as.factor(case_when(
      .$past_year ~ "In the past year",
      .$more_year ~ "More than 1 year ago",
      .$never_chk ~ "Never had flu shot",
      .$dontknow ~ "Don\'t know",
      .$non_resp ~ "Non-response",
      TRUE ~ "Missing"))
  )
'
#############################################
###     PREVENTIVE CARE: QofC 2.1-2.2     ###
#############################################

snp_care$adult_nosmok <- '

if(year <= 2002){
  FYC <- FYC %>% rename(ADNSMK42 = ADDSMK42)
}

FYC <- FYC %>% 
  mutate(
    adult_nosmok = recode_factor(ADNSMK42,
       "1" = "Told to quit",
       "2" = "Not told to quit",
       "3" = "Had no visits in the last 12 months",
      "-9" = "Not ascertained",
      "-1" = "Inapplicable")
  )
'
snp_care$child_dental <-'
FYC <- FYC %>% 
  mutate(
    child_2to17 = (1 < AGELAST & AGELAST < 18),
    child_dental = ((DVTOT&yy. > 0) & (child_2to17==1))*1,
    child_dental = recode_factor(child_dental,
      "1" = "One or more dental visits",
      "0" = "No dental visits in past year"))
'


#######################################################
###  QUALITY OF CARE - ADULTS: QofC 3.1-4.10 (odd)  ###
#######################################################

how_often_codes <- '
    "4" = "Always",
    "3" = "Usually",
    "2" = "Sometimes/Never",
    "1" = "Sometimes/Never",
    "-8" = "Don\'t know",
    "-7" = "Non-response",
    "-9" = "Non-response",
    "-1" = "Inapplicable"'

snp_care$adult_routine <- sprintf('
FYC <- FYC %%>%% 
  mutate(adult_routine = recode_factor(ADRTWW42,%s))
',how_often_codes) 

snp_care$adult_illness <- sprintf('
FYC <- FYC %%>%%
  mutate(adult_illness = recode_factor(ADILWW42,%s))
',how_often_codes) 

snp_care$adult_listen <- sprintf('
FYC <- FYC %%>%%
  mutate(adult_listen = recode_factor(ADLIST42,%s))
',how_often_codes) 

snp_care$adult_explain <- sprintf('
FYC <- FYC %%>%%
  mutate(adult_explain = recode_factor(ADEXPL42,%s))
',how_often_codes) 

snp_care$adult_respect <- sprintf('
FYC <- FYC %%>%%
  mutate(adult_respect = recode_factor(ADRESP42,%s))
',how_often_codes) 

snp_care$adult_time <- sprintf('
FYC <- FYC %%>%%
  mutate(adult_time = recode_factor(ADPRTM42,%s))
',how_often_codes) 

snp_care$adult_rating <- '
FYC <- FYC %>% 
  mutate(
    adult_rating = as.factor(case_when(
      .$ADHECR42 >= 9 ~ "9-10 rating",      
      .$ADHECR42 >= 7 ~ "7-8 rating",
      .$ADHECR42 >= 0 ~ "0-6 rating",
      .$ADHECR42 == -8 ~ "Don\'t know",
      .$ADHECR42 == -9 ~ "Non-response",
      .$ADHECR42 == -1 ~ "Inapplicable",
      TRUE ~ "Missing"))
  )
'


##########################################################
###  QUALITY OF CARE - CHILDREN: QofC 3.1-4.10 (even)  ###
##########################################################

snp_care$child_routine <- sprintf('
FYC <- FYC %%>%% 
  mutate(child_routine = recode_factor(CHRTWW42,%s))
',how_often_codes) 

snp_care$child_illness <- sprintf('
FYC <- FYC %%>%%
  mutate(child_illness = recode_factor(CHILWW42,%s))
',how_often_codes) 

snp_care$child_listen <- sprintf('
FYC <- FYC %%>%%
  mutate(child_listen = recode_factor(CHLIST42,%s))
',how_often_codes) 

snp_care$child_explain <- sprintf('
FYC <- FYC %%>%%
  mutate(child_explain = recode_factor(CHEXPL42,%s))
',how_often_codes) 

snp_care$child_respect <- sprintf('
FYC <- FYC %%>%%
  mutate(child_respect = recode_factor(CHRESP42,%s))
',how_often_codes) 

snp_care$child_time <- sprintf('
FYC <- FYC %%>%%
  mutate(child_time = recode_factor(CHPRTM42,%s))
',how_often_codes) 

snp_care$child_rating <- '
FYC <- FYC %>% 
  mutate(
    child_rating = as.factor(case_when(
      .$CHHECR42 >= 9 ~ "9-10 rating",
      .$CHHECR42 >= 7 ~ "7-8 rating",
      .$CHHECR42 >= 0 ~ "0-6 rating",
      .$CHHECR42 == -8 ~ "Don\'t know",
      .$CHHECR42 == -9 ~ "Non-response",
      .$CHHECR42 == -1 ~ "Inapplicable",
      TRUE ~ "Missing"))
  )
'

###################################################
###  Access to care: AtoC 1 (skipping 2.1-2.3)  ###
###################################################

snp_care$usc <-'
if(year <= 2001) FYC <- FYC %>% rename(LOCATN42 = LOCATI42)

FYC <- FYC %>%
  mutate(usc = ifelse(HAVEUS42==2,0,LOCATN42)) %>%
  mutate(usc = recode_factor(usc, .default="Missing",
            "0" = "No usual source of health care",
            "1" = "Office-based",
            "2" = "Hospital",
            "3" = "Emergency room")
  )
'

snp_care$delay <- '
FYC <- FYC %>%
  mutate(delay_MD = (MDUNAB42 == 1|MDDLAY42==1)*1,
         delay_DN = (DNUNAB42 == 1|DNDLAY42==1)*1,  
         delay_PM = (PMUNAB42 == 1|PMDLAY42==1)*1,
         delay_ANY = (delay_MD|delay_DN|delay_PM)*1) 
'  

snp_care$rsn_DN <- '
FYC <- FYC %>%
  mutate(delay_DN  = (DNUNAB42 == 1|DNDLAY42==1)*1,
         afford_DN = (DNDLRS42==1|DNUNRS42==1)*1,
         insure_DN = (DNDLRS42 %in% c(2,3)|DNUNRS42 %in% c(2,3))*1,
         other_DN  = (DNDLRS42 > 3|DNUNRS42 > 3)*1)
'

snp_care$rsn_MD <- '
FYC <- FYC %>%
  mutate(delay_MD  = (MDUNAB42 == 1|MDDLAY42==1)*1,
         afford_MD = (MDDLRS42==1|MDUNRS42==1)*1,
         insure_MD = (MDDLRS42 %in% c(2,3)|MDUNRS42 %in% c(2,3))*1,
         other_MD  = (MDDLRS42 > 3|MDUNRS42 > 3)*1)
'

snp_care$rsn_PM <- '
FYC <- FYC %>%
  mutate(delay_PM  = (PMUNAB42 == 1|PMDLAY42==1)*1,
         afford_PM = (PMDLRS42==1|PMUNRS42==1)*1,
         insure_PM = (PMDLRS42 %in% c(2,3)|PMUNRS42 %in% c(2,3))*1,
         other_PM  = (PMDLRS42 > 3|PMUNRS42 > 3)*1)
'








