##################################################
###     APP: UTILIZATION AND EXPENDITURES      ###
##################################################

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)

source("../shared/app_preamble.R", chdir=T, local=T)
source("app_info.R",chdir=T,local=T)
#source("global.R",chdir=T,local=T)
#source("app_code.R",chdir=T,local=T)


###########################################################

form_elements <- tagList(
   h1("This app is under construction")
)

ui <- mepsPage("cond",info=info,form_elements=form_elements)

##############################################################


#################################

server <- function(input,output,session) {

}

  
# Run the application 
shinyApp(ui = ui, server = server)
