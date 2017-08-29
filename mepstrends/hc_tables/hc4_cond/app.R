##################################################
###          APP: MEDICAL CONDITIONS           ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_preamble.R", chdir=F, local=T)

###########################################################


form_elements <- tagList(
  
  selectInput508("stat",label="Select statistic:",choices=cond_stats),
  standardErrorInput("cond"),
 
  dataViewInput("cond"),
  yearInput("cond",min=min(cond_tables$Year),max=max(cond_tables$Year)),
  
  rcInput("cond",type="rows",choices=subgrps,class="hide-if-trend slide"),
  rcInput("cond",type="cols",choices=c("Condition"),class="hidden")
)

tab_panel <- tabsetPanel(type="pills",
  tabPanel(title=tags$span(class='tab-title table-tab',"Table"),
           plotUI('cond'),
           tableUI('cond',DT=T)),
  #tabPanel(title=tags$span(class='tab-title plot-tab',"Plot"),plotUI('cond')),
  tabPanel(title=tags$span(class='tab-title code-tab',"Code"),codeUI('cond'))
)

ui <- mepsPage("cond",info=info,form_elements=form_elements,tab_panel=tab_panel)



##############################################################


#################################

server <- function(input,output,session) {
  
  stat <- reactive(input$stat)

  adj <- reactive({
    D <- switch(stat(),"totPOP"=1E3, "totEXP"=1E9,"totEVT"=1E6,1)
    if(stat() %>% startsWith("pct")) D = 1E-2

    lab <- ifelse(D==1E3, " in thousands",
           ifelse(D==1E6, " in millions",
           ifelse(D==1E9, " in billions","")))

    d <- ifelse(D==1,0,1)

    return(list(D=D,d=d,label=lab))
  })


  ### Modules ###

  # Edit inputs and filter data
  meps_data <-
    callModule(dataModule,"cond",
               df = cond_tables,
               stat = stat,
               adj=adj,
               exclude_choices = c(""),
               exclude_initial = c(""))

  meps_inputs <- reactive(meps_data()) 
  
  # tabPanels
  callModule(notesModule,"cond", meps_inputs=meps_inputs)
  tab_rows <- callModule(tableModule,"cond", meps_inputs=meps_inputs,pivot=T,DT=T)
  callModule(plotModule, "cond", meps_inputs=meps_inputs,plot_rows=tab_rows,DT=T)
  callModule(codeModule, "cond", meps_inputs=meps_inputs)
}

  
# Run the application 
shinyApp(ui = ui, server = server)
