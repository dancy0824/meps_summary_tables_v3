##################################################
###   APP: Accessibility and Quality of Care   ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_preamble.R", chdir=F, local=T)

###########################################################

form_elements <- tagList(
  rcInput("care",type="cols",choices=care_subgrps,label="Select variable:"),
  standardErrorInput("care"),
  
  dataViewInput("care"),
  yearInput("care",min=min(care_tables$Year),max=max(care_tables$Year)),
  
  rcInput("care",type="rows",choices=subgrps,class="hide-if-trend slide")
)

tab_panel <- tabsetPanel(type="pills",
  tableUI('care'),
  plotUI('care'),
  codeUI('care')
)

ui <- mepsPage("care",info=info,form_elements=form_elements,tab_panel=tab_panel)


##############################################################

# Exclude levels from initial select
all_levels <- c(care_tables$levels1,care_tables$levels2) %>% unique
exclude_initial <- exclude_levels(all_levels)
exclude_choices <- c(
  grep("missing",all_levels,value=T,ignore.case=T),
  grep("inapplicable",all_levels,value=T,ignore.case=T)
)
##############################################################

server <- function(input, output,session) {
  
  stat <- reactive('pctPOP')

  adj <- reactive({
    D = 1E-2; d = 1; lab = "";
    return(list(D=D,d=d,label=lab))
  })

  ### Modules ###
  
  # Edit inputs and filter data
  meps_data <- 
    callModule(dataModule,"care", 
               df = care_tables,
               stat = stat,
               exclude_choices = exclude_choices,
               exclude_initial = exclude_initial)
  
  # Intentional bottleneck here to slow update
  meps_inputs <- reactive(list(adj=adj(),tbl=meps_data()$tbl,inputs=meps_data()$inputs)) %>% debounce(1500)
  
  # Labels, caption, footnotes, notes
  meps_labels <- callModule(notesModule,"care", meps_inputs=meps_inputs)

  # tabPanels
  callModule(tableModule,"care", meps_inputs=meps_inputs, labels=meps_labels)
  callModule(plotModule, "care", meps_inputs=meps_inputs, labels=meps_labels)
  callModule(codeModule, "care", meps_inputs=meps_inputs)
}


# Run the application 
shinyApp(ui = ui, server = server)
