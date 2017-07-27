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
  "Socio-Economic Status" = c(
    "Education"         = "education",
    "Employment Status" = "employed",
    "Insurance Coverage" = "insurance",
    "Poverty Status"    = "poverty"
  ),
  "Health Variables" = c(
    "Perceived Health Status" = "health",
    "Perceived Mental Health" = "mental_health"
  )
)  

subgrp_vec <- unlist(subgrps) 

subgrp_code <- function(grps,lang="r"){
  lang <- tolower(lang)
  
  subgrps <- subgrp_vec[subgrp_vec != 'ind']
  subgrps <- grps[grps %in% subgrps] %>% unique
  
  # add agevar if needed
  if(any(c("agegrps","employed","education","insurance") %in% subgrps))
    subgrps <- c("agevar",subgrps)
  
  sapply(subgrps, function(x)
    readSource(sprintf("../shared/%s/subgrps/%s.%s",lang,x,lang))) %>%
    paste(collapse="\n")
} 


#####################################################
###                   FUNCTIONS                   ###
#####################################################

get_file_names <- function(year){
  meps_names %>% 
    filter(Year==year) %>%
    select(-Year,-ends_with('Panel'))
}


readSource <- function(file,...,dir="."){
  fileName <- sprintf("%s/%s",dir,file) %>% gsub("//","/",.)
  codeString <- readChar(fileName,file.info(fileName)$size)
  codeString <- codeString %>% rsub(...) %>% gsub("\r","",.)
  codeString
}

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
