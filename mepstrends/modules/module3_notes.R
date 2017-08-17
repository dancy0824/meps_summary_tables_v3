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

notesModule <- function(input, output, session, tbl, inputs, adj){ 
  
  years <- reactive(inputs()$years)
  cols  <- reactive(inputs()$cols)
  rows  <- reactive(inputs()$rows)
  rowsX <- reactive(inputs()$rowsX) #ind -> year
  stat  <- reactive(inputs()$stat)
  controlTotals <- reactive(inputs()$controlTotals)
  
  #################
  
  stat_label <- reactive({
    paste0(stat_labels[[stat()]], adj()$label)
  })
  
  labels <- reactive({
    list(rows = grp_labels[[rowsX()]], Total=stat_label())
  })
  
  year_caption <- reactive({                                             
    if(input$tabs=="cross") return(input$year)
    c(min(years()),max(years())) %>% unique %>% paste0(collapse="-")
  })
  
  se_caption <- reactive({
    if(!inputs()$showSEs) return("")
    return(" <SE>")
  })
  
  caption <- reactive({
    get_caption(stat_label(),rows(),cols(),se_caption(),year_caption())
  })

  #################
  
  source_text <- reactive( # also used in download table
    paste("<b>Source:</b>",sprintf("%s, %s, %s, %s.",CFACT,AHRQ,MEPS,year_caption()))
  ) 
  
  output$source <- renderText(source_text())
  
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
    sprintf("%s. %s. %s. Generated interactively: %s.",AHRQ,caption(),MEPS,today) %>% 
      gsub("<SE>","(standard errors)",.)
  })
  
  ################
  
  footnotes <- reactive({  
    tab <- tbl()
    footnotes = list()
    footnotes$suppress <- ifelse(any(tab$suppress,na.rm=T),suppressed_message,"")
    footnotes$star     <- ifelse(any(tab$star,na.rm=T),rse_message,"")
    return(footnotes)
  })
  
  
  #################
  
  return(reactive(list(labels = labels(), 
                       source = source_text(),
                       caption = caption(), 
                       footnotes = footnotes(),
                       controlTotals = controlTotals())))
  
}
