## SHARED/global -- stuff used by run + app codes -- across all apps


#####################################################
###                    SUBGROUPS                  ###
#####################################################

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

subgrp_code <- function(grps,lang="r"){
  lang <- tolower(lang)
  
  subgrps <- subgrps[subgrps != 'ind']
  subgrps <- grps[grps %in% subgrps] %>% unique
  
  # add agevar if needed
  if(any(c("agegrps","employed","education","insurance") %in% subgrps))
    subgrps <- c("agevar",subgrps)
  
  sapply(subgrps, function(x)
    readCode(sprintf("../shared/%s/grps/%s.%s",lang,x,lang))) %>%
    paste(collapse="\n")
} 


#####################################################
###              FORMATTING FUNCTIONS             ###
#####################################################

rsub <- function(string,...,type='r'){
  repl = switch(type,
                'r'='\\.%s\\.',
                'sas'='&%s\\.')
  
  sub_list = list(...) %>% unlist
  for(l in names(sub_list)){
    original <- sprintf(repl,l)
    replacement <- sub_list[l]
    string <- gsub(original,replacement,string)
  }
  return(string)
}

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
