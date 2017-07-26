################################
###      HELPER FUNCTIONS    ###
################################

add = function(main,extra,collapse="\n") paste0(main,collapse,extra)

rename_cols = function(df,lst){
  for(l in 1:length(lst)){
    old = names(lst[l])
    new = lst[[l]]
    names(df)[names(df) == old] <- new
  }
  return(df)
}     

add.table <- function(x,file,...){
  suppressWarnings(write.table(x,file,sep=",",row.names=F,append=T,...))
}

formatNum <- function(x,d=1) ifelse(is.na(x),"--",formatC(x,big.mark=",",format="f",digits=d)) 
wrap_html <- function(...) str_wrap(...) %>% gsub("\n","<br>",.)
rm_html <- function(string) sapply(string,function(x) gsub("*<.*?> *"," ",x))
rm_brks <- function(string) sapply(string,function(x) gsub("\n","",x))
rm_xspc <- function(string) sapply(string,function(x) gsub("  "," ",x))

# 
# build_header <- function(dir){
#   tags$ul(class="usa-nav-primary usa-accordion",
#     tags$li(tags$a(class = 'usa-nav-link',href=sprintf('%s/hc1_use/',dir),
#                    tags$span("Use, Expenditures, and Population"))),
#     
#     tags$li(tags$a(class = 'usa-nav-link',href=sprintf('%s/hc2_care/',dir),
#                    tags$span("Accessibility and Quality of Care"))),
#     
#     tags$li(tags$a(class = 'usa-nav-link',href=sprintf('%s/hc3_pmed/',dir),
#                    tags$span("Top Prescribed Drugs"))),
#     
#     tags$li(tags$a(class = 'usa-nav-link',href=sprintf('%s/hc4_cond/',dir),
#                    tags$span("Medical Conditions")))
#     )
# }


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


meps_wrap = function(s,br = "\n"){
  s %>% 
    gsub("Indian, Alaska Native, or",sprintf("Indian,%sAlaska Native,%sor",br,br),.) %>%
    gsub("Hawaiian, or",sprintf("Hawaiian,%sor",br),.) %>%
    gsub("Inapplicable \\(age",sprintf("Inapplicable%s\\(age",br),.)
}

