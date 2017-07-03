
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(stringr)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(scales)

addResourcePath('www','../../www')

#meps_names = read.csv("meps_names.csv",stringsAsFactors = F)

meps_names = read.csv("https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv",
                      stringsAsFactors = F)

source("notes.R",local=TRUE)
source("functions.R",local=TRUE)
source("functions508.R",local=TRUE)
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

subgrps <- list(
  "(none)"             = "ind",
  "Demographics" = c(
    "Age Groups"         = "agegrps",
    "Census Region"      = "region",
    "Marital Status"     = "married",
    "Race/Ethnicity"     = "race",
    "Sex"                = "sex"
  ),
  "Health Variables" = c(
    "Insurance Coverage" = "insurance",
    "Perceived Health Status" = "health",
    "Perceived Mental Health" = "mental_health"
  ), 
  "Socio-Economic Status" = c(
    "Education"         = "education",
    "Employment Status" = "employed",
    "Poverty Status"    = "poverty"
  )
)  

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

