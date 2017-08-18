#####################################################
###               PAGE BUILDER FUNCTION           ###
#####################################################


mepsPage <- function(id,info,form_elements,tab_panel){
  
  meps_body <- bootstrapPage(
    
    div(class = 'info-box ',
        div(class='full-screen',
            h2(info$title), p(info$description), p(info$instructions))),
    
    fluidRow(class = "full-screen",
             column(width=12,class="col-md-3",
                    tags$form(class = "usa-form-large", form_elements)),
             
             column(width=12,class="col-md-9",tab_panel,hc_info(id))
    )
  )
  
  # external_link <- 'You are leaving a U.S. Department of Health and Human Services (HHS) Web site and entering a nongovernment Web site. \n\nHHS cannot attest to the accuracy of information provided by linked sites. \n\nLinking to an external Web site does not constitute an endorsement by HHS, or any of its employees, of the sponsors of the site or the products presented on the site. \n\nYou will be subject to the destination site\'s privacy policy when you leave the HHS site.\n\nPress \'OK\' to accept or \'Cancel\' to stay on this page. '
  
  htmlTemplate("../../template.html", body = meps_body, dir="../..")
}

#####################################################
###                    SUBGROUPS                  ###
#####################################################

subgrps <- list(
  "(none)"             = "ind",
  "Demographics" = c(
    "Age groups"         = "agegrps",
    "Census region"      = "region",
    "Marital status"     = "married",
    "Race/ethnicity"     = "race",
    "Sex"                = "sex"
  ),
  "Socio-economic status" = c(
    "Education"         = "education",
    "Employment status" = "employed",
    "Insurance coverage" = "insurance",
    "Poverty status"    = "poverty"
  ),
  "Health variables" = c(
    "Perceived health status" = "health",
    "Perceived mental health" = "mental_health"
  )
)  

subgrp_vec <- unlist(subgrps) 

subgrp_code <- function(grps,lang="r"){
  lang <- tolower(lang)
  if(lang=="r") LANG = "R" else LANG = lang
  
  subgrps <- c("agevar",subgrp_vec[subgrp_vec != 'ind'])
  subgrps <- grps[grps %in% subgrps] %>% unique
  
  # add agevar if needed
  if(any(c("agegrps","employed","education","insurance") %in% subgrps))
    subgrps <- c("agevar",subgrps)
  
  sapply(subgrps, function(x)
    readSource(sprintf("../shared/%s/subgrps/%s.%s",lang,x,LANG))) %>%
    paste(collapse="\n")
} 

#####################################################
###              CAPTION FUNCTIONS                ###
#####################################################

get_subgrp_caption <- function(rows="",cols=""){
  glabels <- c(grp_labels[[rows]], grp_labels[[cols]])
  glabels <- glabels[!glabels%in%c("","(none)","Year")] %>% tolower
  if(length(glabels)==0) return("")
  return(sprintf(" by %s",paste0(glabels,collapse=" and ")))
}

get_caption <- function(stat_label,rows,cols,se_caption,year_caption){
  subgrp_caption <- get_subgrp_caption(rows,cols)
  sprintf("%s%s%s, United States, %s",stat_label,se_caption,subgrp_caption,year_caption)
}

#####################################################
###              CODE TAB FUNCTIONS               ###
#####################################################

subgrp_comma <- function(grps){
  sg <- paste(grps,collapse=",")
  if(sg != "") sg = paste0(sg,",")
  sg
}

subgrp_formula <- function(grps){
  sprintf("~%s",paste(grps,collapse="+"))
}

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

exclude_levels <- function(all_levels){
  c(grep("physician",all_levels,value=T,ignore.case=T),
    grep("agency|independent",all_levels,value=T,ignore.case=T),
    grep("<65,|65\\+,",all_levels,value=T,ignore.case=T),
    grep("Under 5|5-17|18-44|45-64",all_levels,value=T,ignore.case=T),
    grep("All ",all_levels,value=T,ignore.case=T)
  )
}


###################################
###     SHINY 508 FUNCTIONS     ###
###################################

## The following functions are created to be used specifically
## with government standards for websites

###################################
###          TABLE              ###
###################################

HTML508table <- function(body,caption="",cnames=colnames(body) ){
  
  colHEADERS = sprintf(" <th scope='col' >%s</th> ",cnames)
  columnROW  = sprintf("<tr>%s</tr>",paste(colHEADERS,collapse=""))
  
  body[,1]  <- sprintf(" <th scope='row'>%s</th> ",body[,1])
  body[,-1] <- sprintf(" <td >%s</td> ", as.matrix(body[,-1]))
  
  body_string <- cbind(" <tr> ",body," </tr> ")  %>% t %>% paste(collapse="")
  
  myHtable <- sprintf("<div class = 'meps-table'>
                      <table border=0>
                      <thead> %s </thead>
                      <tbody> %s </tbody>
                      </table>
                      </div>",
                      columnROW,
                      body_string)
  
  return(HTML(myHtable))
}


###################################
###           LAYOUT            ###
###################################

#btn btn-default action-button


fluidRow508 <- function(...,full=F){
  add = ifelse(full,"-full","")
  div(class=paste0("usa-grid",add),...)
}

col508 <- function(...,width="one-half"){
  div(class=paste0("usa-width-",width),...)
}

textOutput508 <- function (outputId, container = if(inline) span else div, inline = FALSE,class=""){
  container(id = outputId, class = paste("shiny-text-output",class))
}

###################################
###  ACCORDIONS AND DROPDOWNS   ###
###################################

addLinks <- function(tabsetPanels,...,class=""){
  extras <- list(...)
  addItems <- lapply(extras, function(x) tags$li(x,class=class))
  
  tabs <- tabsetPanels$children[[1]]
  tabs$children <- append(tabs$children,addItems)
  tabsetPanels$children[[1]] <- tabs
  tabsetPanels
}

accordion508 <- function(...,bordered=TRUE){
  bordered = ifelse(bordered,"-bordered","")
  tags$ul(class=paste0("usa-accordion",bordered),...)
}

accordionPanel508 <- function(title,...,inputId=title,expanded=FALSE){
  expanded = tolower(expanded)
  inputId = gsub(" ","_",inputId)
  tags$li(
    tags$button(class = "usa-accordion-button",
                "aria-expanded" = expanded,
                "aria-controls" = inputId,title),
    tags$div(id=inputId,class="usa-accordion-content",...)
  )
}

dropdown508 <- function(inputId,label="",...){
  labelTag <- tags$span(class="usa-sr-only",label)
  
  div(class="dropdown black-text", id = inputId,
      tags$button(type="button",  title = "Select Levels", #id = inputId,
                  class="usa-accordion-button dropdown-toggle shiny-bound-input arrow-button em-tooltip",
                  'data-toggle'="dropdown",
                  'aria-haspopup'="true", 'aria-expanded'="false",labelTag),
      tags$ul(class="dropdown-menu dropdown-menu-right dropdown-menu-form", 'aria-labelledby'=inputId,...)
  )
}

###################################
###           NAVBAR            ###
###################################

link508 <- function(title,href="#",class=""){
  atag <- tags$a(href=href, 'data-value'=href, class=class, title)
  if(!startsWith(href,"http")) atag$attribs['data-toggle'] = 'tab'
  atag
}

item508 <- function(...){
  tags$li(link508(...))
}

navbarLink508 <- function(...){
  tags$li(link508(...,class="usa-nav-link"))
}

navbarMenu508 <- function(title,inputId,...){
  tags$li(
    tags$button(class="usa-accordion-button usa-nav-link",
                'aria-expanded'="false",
                'aria-controls'=inputId,
                tags$span(title)),
    tags$ul(id=inputId, class="usa-nav-submenu",...)
  )
}

navbar508 <- function(...) {
  tabs <- list(...)
  navlist <- tagList(tabs)
  
  containerDiv <-
    div(class = "usa-nav-inner",
        tags$button(class = "usa-nav-close",tags$img(src="www/uswds-1.1.0/img/close.svg", alt="close")),
        tags$ul(class="usa-nav-primary usa-accordion",navlist)
    )
  
  tags$nav(role = "navigation", class = "usa-nav", containerDiv)
}

tabPanel508 <- function(inputId, ..., value = title, icon = NULL) {
  div(id=inputId, class = "tab-pane", 'data-value'=inputId,
      `data-icon-class` = shiny:::iconClass(icon),...)
}

navbarPanels508 <- function(...,selected=NULL){
  tabs = list(...)
  tabs[[1]]$attribs$class = "tab-pane active"
  
  div(class="tab-content",tagList(tabs))
}

#################################################
###               FORM INPUTS                 ###
#################################################


## Buttons

actionButton508 <- function (inputId, label, usaStyle = NULL, class="", icon = NULL, width = NULL, ...){
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, style = if (!is.null(width))
    paste0("width: ", validateCssUnit(width), ";"), type = "button",
    class = sprintf("action-button %s",class),
    class = paste(c("usa-button",usaStyle),collapse="-"), 
    `data-val` = value,
    list(shiny:::validateIcon(icon), label), ...)
}

downloadButton508 <- function (outputId, label = "Download", class="",hideLabel=T){
  #if(missing(icon)) icon <- icon("download")
  
  title <- ifelse(hideLabel,label,'')
  labelClass <- ifelse(hideLabel,'usa-sr-only','')
  aTag <- tags$a(id = outputId,
                 title = title,
                 class = paste("em-tooltip shiny-download-link usa-button",class),
                 href = "",
                 target = "_blank",
                 #tags$span(icon,`aria-hidden`='true'),
                 tags$span(class=labelClass,label))
}


checkboxInput508 <- function(inputId, label, value = FALSE, inline=FALSE, class=""){
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox", name=inputId, value=inputId,class=class)
  if (!is.null(value) && value) inputTag$attribs$checked <- "checked"
  labelTag <- tags$label('for'=inputId,label)
  if(inline){
    inputTag$attribs$style = 'display: inline;'
    labelTag$attribs$style = 'display: inline;'
  }
  tagList(inputTag,labelTag)
}

checkboxGroupInput508 <- function (inputId, choices, label=NULL, selected = NULL, inline=FALSE) {
  
  selected <- restoreInput(id = inputId, default = selected)
  choices <- shiny:::choicesWithNames(choices)
  
  if(!is.null(selected)) selected <- as.character(selected)
  
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  
  options <- generateOptions508(inputId, choices, selected, inline)
  
  labelTag <- ""
  if(!is.null(label)) labelTag <- tags$label(label)
  legendTag <- tags$legend(label,class="usa-sr-only")
  
  
  tags$fieldset(id=inputId,
                class="usa-fieldset-inputs usa-sans shiny-input-checkboxgroup", ## !important shiny class
                labelTag,
                legendTag,
                tags$ul(class="usa-unstyled-list",options)
  )
}


radioButtons508 <- function(inputId, label, choices, selected = NULL, inline = FALSE, width = NULL,class="") {
  choices <- shiny:::choicesWithNames(choices)
  selected <- shiny:::restoreInput(id = inputId, default = selected)
  selected <- if(is.null(selected)){
    choices[[1]]
  }else {
    as.character(selected)
  }
  if(length(selected) > 1) stop("The 'selected' argument must be of length 1")
  
  options <- generateOptions508(inputId, choices, selected, inline, type = "radio")
  
  #labelTag <- tags$label(label)
  legendTag <- tags$legend(label,class="em-legend")
  
  tags$fieldset(id=inputId,
                class= paste("usa-fieldset-inputs usa-sans shiny-input-radiogroup",class), ## !important shiny class
                #  labelTag,
                legendTag,
                tags$ul(class="usa-unstyled-list",options)
                
  )
  
}



## Select Input

selectInput508 <- function (inputId, choices, selected = NULL, label=NULL, width = NULL, size = NULL){
  selected <- shiny:::restoreInput(id = inputId, default = selected)
  choices <- shiny:::choicesWithNames(choices)
  if(is.null(selected)) {
    selected <- shiny:::firstChoice(choices)
  }else{
    selected <- as.character(selected)
  }
  
  selectTag <- tags$select(id = inputId, size = size, shiny:::selectOptions(choices, selected))
  #styleTag <- if (!is.null(width)) paste0("max-width: ", validateCssUnit(width), ";")
  labelTag <- if(!is.null(label)) tags$label(label, 'for'=inputId)
  
  #tags$form(class=paste("usa-form",class), style = styleTag, labelTag, selectTag)
  tagList(labelTag, selectTag)
  
}


#################################################
###            INNER FUNCTIONS                ###
#################################################

generateOptions508 <- function (inputId, choices, selected, inline=FALSE, type = "checkbox"){
  options <- mapply(choices, names(choices),
                    FUN = function(value,name) {
                      unique_id = paste(inputId,value,sep="-") ## need this in case using same choices across namespaces
                      inputTag <- tags$input(id = unique_id, type = type, name = inputId, value = value)
                      if(value %in% selected) inputTag$attribs$checked <- "checked"
                      labelTag <- tags$label('for'=unique_id, name)
                      listTag <- tags$li(inputTag,labelTag)
                      
                      if(inline) listTag$attribs$style="display: inline-block; padding-right: 30px;"
                      listTag
                    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  div(class="shiny-options-group",options) ## need shiny-options-group class to replace, not append, new choices
}


updateInput508 <- function (session, inputId, label = NULL, choices = NULL, selected = NULL, type = NULL) {
  if (!is.null(choices))  choices <- shiny:::choicesWithNames(choices)
  if (!is.null(selected)) selected <- as.character(selected)
  options <- if (!is.null(choices)) {
    format(tagList(generateOptions508(session$ns(inputId), choices, selected, type = type)))
  }
  message <- shiny:::dropNulls(list(label = label, options = options, value = selected))
  session$sendInputMessage(inputId, message)
}


updateCheckboxGroupInput508 <- function(session, inputId, label = NULL, choices = NULL, selected = NULL){
  updateInput508(session, inputId, label, choices, selected, type="checkbox")
}





