################################
###  SHINY HELPER FUNCTIONS  ###
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

  myHtable <- sprintf("<table border=0>
                      <caption> %s </caption>
                      <thead> %s </thead>
                      <tbody> %s </tbody>
                      </table>",
                      caption,
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

downloadButton508 <- function (outputId, label = "Download", icon, class=""){
  if(missing(icon)) icon <- icon("download")
  aTag <- tags$a(id = outputId,
                 class = paste("shiny-download-link ",class),
                 href = "",
                 target = "_blank",
                 tags$span(icon,`aria-hidden`='true'),label)
}


checkboxInput508 <- function(inputId, label, value = FALSE, inline=FALSE){
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox", name=inputId, value=inputId)
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
