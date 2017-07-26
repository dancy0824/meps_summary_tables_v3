
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(stringr)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(scales)

addResourcePath('www','../../www')


meps_names <- read.csv("puf_expanded.csv", stringsAsFactors=F)

source("global.R",local=TRUE)
source("app_notes.R",local=TRUE)
source("app_functions.R",local=TRUE)
source("app_functions508.R",local=TRUE)
# 
# source("code/r_code/snippets_load.R",local=TRUE)
# source("code/r_code/snippets_subgrps.R",local=TRUE)
# source("code/r_code/snippets_design.R",local=TRUE)

source("../../modules/module1_data.R",local=TRUE)
source("../../modules/module2_levels.R",local=TRUE)
source("../../modules/module3_notes.R",local=TRUE)

source("../../modules/tab1_table.R",local=TRUE)
source("../../modules/tab2_plot.R",local=TRUE)
source("../../modules/tab3_code.R",local=TRUE)


########################################################
## UI builder

# 
# flex <- function(...,width=1){
#   tags$div(style=paste0('flex: ',width),...)
# }

mepsPage <- function(id,info,form_elements,tab_elements){

  meps_body <- bootstrapPage(
  
    div(class = 'info-box ',
      div(class='full-screen',
          h1(info$title), p(info$description), p(info$instructions))),

   fluidRow(class = "full-screen",
     column(width=12,class="col-md-3",
            tags$form(class = "usa-form-large", form_elements)),
     column(width=12,class="col-md-9",
            tabsetPanel(type="pills",tableUI(id),plotUI(id),codeUI(id)),
            hc_info(id))
   )
  )
  
# meps_header <- build_header(dir="..")

  #htmlTemplate("../../template.html", body = meps_body, footer = meps_footer)
  htmlTemplate("../../template.html", body = meps_body, dir="..")
}

