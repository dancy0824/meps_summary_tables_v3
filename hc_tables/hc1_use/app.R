
# Use and expenditures app

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_global.R", chdir=T, local=T)

info <- source("app_info.R")$value
load("USE_TABLES.Rdata")

###########################################################

use_stats = list(
  "Population" = c(
    "Number of people" = "totPOP",
    "Percent of population with an expense (%)" = "pctEXP"
  ),
  "Expenditures" = c(
    "Total expenditures ($)"                        = "totEXP",
    "Mean expenditure per person ($)"               = "meanEXP0",
    "Mean expenditure per person with expense ($)"  = "meanEXP",
    "Median expenditure per person with expense ($)"= "medEXP"
  ),
  "Utilization" = c(
    "Number of events" = "totEVT",
    "Mean expenditure per event ($)" = "meanEVT"
  )
)

use_add <- list(
  "Event Type"  = "event",
  "Source of Payment"  = "sop")

use_subgrps <- append(subgrps,use_add,after=1)

stat_labels <- use_stats %>% invertList
grp_labels <- use_subgrps %>% invertList 
grp_labels$Year = "Year"

############################################################

form_elements <- tagList(
    selectInput508("stat",label="Select statistic:",choices=use_stats),
    
    standardErrorInput("use"),
    dataViewInput("use"),
    yearInput("use",min=min(use_tables$Year),max=max(use_tables$Year)),
    
    tags$fieldset(
      div(class = "flex-parent",
          
          div(class = "col1",
              rcInput("use",type="cols",choices=use_subgrps),
              rcInput("use",type="rows",choices=use_subgrps, 
                      class = "hide-if-trend",selected = "event",hide_label=T)
          ),
          
          div(class = "col2 hide-if-trend", switchUI("use"))
      )
    )
)


body_ui <- mepsPage("use",info=info,form_elements=form_elements)
  

##################################

ui <- htmlTemplate("../../template.html", body = body_ui)

# Exclude levels from initial select
  all_levels <- c(use_tables$levels1, use_tables$levels2) %>% unique
  
  
  ## this should be a function in shared/function.R instead
  exclude_initial <- c(
    grep("physician",all_levels,value=T,ignore.case=T),
    grep("agency|independent",all_levels,value=T,ignore.case=T),
    grep("<65,|65\\+,",all_levels,value=T,ignore.case=T),
    grep("Under 5|5-17|18-44|45-64",all_levels,value=T,ignore.case=T),
    grep("All",all_levels,value=T,ignore.case=T)
  )

  
# Exclude 'Missing' entirely (can comment out for debugging)
  exclude_choices <- c(
    grep("missing",all_levels,value=T,ignore.case=T),
    grep("All",all_levels,value=T)
  )
  
#################################

server <- function(input, output,session) {

  stat <- reactive(input$stat)
  
  adj <- reactive({
    D <- switch(stat(),"totPOP"=1E3, "totEXP"=1E9,"totEVT"=1E6,1)
    if(stat() %>% startsWith("pct")) D = 1E-2
    
    lab <- ifelse(D==1E3, " (in thousands)",
           ifelse(D==1E6, " (in millions)",
           ifelse(D==1E9, " (in billions)","")))
    
    d <- ifelse(D==1,0,1)
    return(list(D=D,d=d,label=lab))
  })


### Modules ###

# Edit inputs and filter data
    meps_data <- 
      callModule(dataModule,"use", 
                 df = use_tables,
                 stat = stat,
                 exclude_choices = exclude_choices,
                 exclude_initial = exclude_initial)
    
    meps_tbl <- reactive(meps_data()$tbl)
    meps_inputs <- reactive(meps_data()$inputs) # years, rows, cols, stat


# Labels, caption, footnotes, notes
    meps_labels <- callModule(notesModule,"use", tbl=meps_tbl, inputs=meps_inputs, adj=adj)

# tabPanels
    callModule(tableModule,"use", tbl=meps_tbl, inputs=meps_inputs, labels=meps_labels, adj=adj)
    callModule(plotModule, "use", tbl=meps_tbl, inputs=meps_inputs, labels=meps_labels, adj=adj)
    callModule(codeModule, "use", inputs=meps_inputs)
}

  
# Run the application 
shinyApp(ui = ui, server = server)
