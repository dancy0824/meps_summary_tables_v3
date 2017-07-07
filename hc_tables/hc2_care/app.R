##################################################
###   APP: Accessibility and Quality of Care   ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_preamble.R", chdir=T, local=T)
source("app_info.R",chdir=T,local=T)
source("global.R",chdir=T,local=T)
load("CARE_TABLES.Rdata")

###########################################################

form_elements <- tagList(
  rcInput("care",type="cols",choices=care_subgrps,label="Select variable:"),
  standardErrorInput("care"),
  
  dataViewInput("care"),
  yearInput("care",min=min(care_tables$Year),max=max(care_tables$Year)),
  
  rcInput("care",type="rows",choices=subgrps,class="hide-if-trend")
)

body_ui <- mepsPage("care",info=info,form_elements=form_elements)

##################################

ui <- htmlTemplate("../../template.html", body = body_ui)

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
               exclude_choices = c(""),
               exclude_initial = c(""))
  
  meps_tbl <- reactive(meps_data()$tbl)
  meps_inputs <- reactive(meps_data()$inputs) # years, rows, cols, stat
  
  # Labels, caption, footnotes, notes
  meps_labels <- callModule(notesModule,"care", tbl=meps_tbl, inputs=meps_inputs, adj=adj)
  
  # tabPanels
  callModule(tableModule,"care", tbl=meps_tbl, inputs=meps_inputs, labels=meps_labels, adj=adj)
  callModule(plotModule, "care", tbl=meps_tbl, inputs=meps_inputs, labels=meps_labels, adj=adj)
  callModule(codeModule, "care", inputs=meps_inputs)
}


# Run the application 
shinyApp(ui = ui, server = server)
