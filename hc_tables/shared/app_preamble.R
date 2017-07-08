
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

mepsPage <- function(id,info,form_elements,tab_elements){
 
  bootstrapPage(
    
    div(class='select-box',
        
        fluidRow508(
          col508(width="one-half", h1(info$title), p(info$description), p(info$instructions) ),
          col508(width="one-half", tags$form(class = "usa-form-large", form_elements))
        )
    ),
    
    div(class='usa-grid',
        addLinks(
          tabsetPanel(type="pills",tableUI(id),plotUI(id),codeUI(id)),
          downloadUI(id), 
          downloadPlotUI(id),
          class="right"),
        
        hc_info(id)
    )

  )

}

