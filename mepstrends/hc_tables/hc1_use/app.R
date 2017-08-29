##################################################
###     APP: UTILIZATION AND EXPENDITURES      ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_preamble.R", chdir=F, local=T)

###########################################################

form_elements <- tagList(
    selectInput508("stat",label="Select statistic:",choices=use_stats),
    
    standardErrorInput("use"),
    dataViewInput("use"),
    yearInput("use",min=min(use_tables$Year),max=max(use_tables$Year)),
    
    tags$fieldset(
      rcInput("use",type="cols",choices=use_subgrps),
      div(class = 'hide-if-trend slide',
          rcInput("use",type="rows",choices=use_subgrps, hide_label=T),
          switchUI("use")
      )
    )
)

tab_panel <- tabsetPanel(type="pills",
  tabPanel(title=tags$span(class='tab-title table-tab',"Table"),tableUI('use')),
  tabPanel(title=tags$span(class='tab-title plot-tab',"Plot"),plotUI('use')),
  tabPanel(title=tags$span(class='tab-title code-tab',"Code"),codeUI('use'))
)


ui <- mepsPage("use",info=info,form_elements=form_elements,tab_panel=tab_panel)

##############################################################

# Exclude levels from initial select
  all_levels <- c(use_tables$levels1, use_tables$levels2) %>% unique
  exclude_initial <- exclude_levels(all_levels)
  exclude_choices <- grep("missing",all_levels,value=T,ignore.case=T)

##############################################################

server <- function(input,output,session) {

  stat <- reactive(input$stat)
  
  adj <- reactive({
    D <- switch(stat(),"totPOP"=1E3, "totEXP"=1E9,"totEVT"=1E6,1)
    if(stat() %>% startsWith("pct")) D = 1E-2
    
    lab <- ifelse(D==1E3, " in thousands",
           ifelse(D==1E6, " in millions",
           ifelse(D==1E9, " in billions","")))
    
    d <- ifelse(D==1,0,1)
    if(stat() == "avgEVT") d = 1
    
    return(list(D=D,d=d,label=lab))
  })


### Modules ###

# Edit inputs and filter data -- debounce inside dataModule
    meps_data <- 
      callModule(dataModule,"use", 
                 df = use_tables,
                 stat = stat,
                 adj = adj,
                 exclude_choices = exclude_choices,
                 exclude_initial = exclude_initial) # Output: adj, inputs, labels, tbl

    meps_inputs <- reactive(meps_data()) 
    
# tabPanels
    callModule(notesModule,"use", meps_inputs=meps_inputs)
    callModule(tableModule,"use", meps_inputs=meps_inputs)
    callModule(plotModule, "use", meps_inputs=meps_inputs)
    callModule(codeModule, "use", meps_inputs=meps_inputs)
}

  
# Run the application 
shinyApp(ui = ui, server = server)
