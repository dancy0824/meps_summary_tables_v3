################################
###      HELPER FUNCTIONS    ###
################################

rename_cols = function(df,lst){
  for(l in 1:length(lst)){
    old = names(lst[l])
    new = lst[[l]]
    names(df)[names(df) == old] <- new
  }
  return(df)
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

add.table <- function(x,file,...){
  suppressWarnings(write.table(x,file,sep=",",row.names=F,append=T,...))
}

formatNum <- function(x,d=1) ifelse(is.na(x),"--",formatC(x,big.mark=",",format="f",digits=d)) 
wrap_html <- function(...) str_wrap(...) %>% gsub("\n","<br>",.)
rm_html <- function(string) sapply(string,function(x) gsub("*<.*?> *"," ",x))
rm_brks <- function(string) sapply(string,function(x) gsub("\n","",x))
rm_xspc <- function(string) sapply(string,function(x) gsub("  "," ",x))

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
