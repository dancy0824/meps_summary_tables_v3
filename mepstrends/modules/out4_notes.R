#######################################################
###                      UI                         ###
#######################################################

hc_info <- function(id){
  ns <- NS(id)
  
  div(id = ns("hc-info"),
    uiOutput(ns("source")),
    
    h3("Notes"),
    p(uiOutput(ns("notes"),role="region","aria-live"="polite")),
    p("This tool is provided as a convenience. It is the responsibility of the user to review
      results for statistical significance and overall reasonableness."),
    
    h3("About the data"),
    p("The MEPS Household Component fields questionnaires to individual household members to collect nationally representative data on
      demographic characteristics, health conditions, health status, use of medical care services, charges and payments,
      access to care, satisfaction with care, health insurance coverage, income, and employment. The population represented in the tables and
      figures includes all members of the U.S. civilian non-institutionalized population."),
    
    h3("Suggested Citation"),
    p(textOutput(ns("citation")))
  )
}

#######################################################
###                     SERVER                      ###
#######################################################

notesModule <- function(input, output, session, meps_inputs){ 
  tbl <- reactive(meps_inputs()$tbl)
  inputs <- reactive(meps_inputs()$inputs)
  labels <- reactive(meps_inputs()$labels)

  years <- reactive(inputs()$years)
  cols  <- reactive(inputs()$cols)
  rows  <- reactive(inputs()$rows)
  rowsX <- reactive(inputs()$rowsX) #ind -> year
  stat  <- reactive(inputs()$stat)
  controlTotals <- reactive(inputs()$controlTotals)

  output$source <- renderText(labels()$source_text)
  
  output$notes <- renderUI({
    stat_notes <- notes[[stat()]] 
    subgrp_notes <- notes[names(notes) %in% c(rows(),cols())] %>% unlist 
    all_notes <- c(stat_notes,subgrp_notes)
    all_notes = all_notes[all_notes!=""]
    
    if(length(all_notes)==0) return(HTML(""))
    HTML(sprintf("<p>%s</p>",paste(all_notes,collapse="<br>")))
  })
  
  output$citation <- renderText({
    today <- Sys.Date() %>% format("%B %d, %Y")
    sprintf("%s. %s. %s. Generated interactively: %s.",AHRQ,labels()$caption,MEPS,today) %>% 
      gsub("<SE>","(standard errors)",.)
  })

}
