
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
  
    div(class = 'select-box ',
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
  # 
  # meps_footer <- tagList(
  #   div(class="usa-footer-primary-section",
  #       div(class = "usa-grid-full",
  #           actionButton508(ns("feedback"),label="Send Feedback"))),
  #   
  #   div(class="usa-footer-secondary_section",
  #       div(class = 'usa-grid',
  #           div(class = 'usa-footer-log usa-width-one-half',
  #               tags$a(href = "https://www.ahrq.gov/",
  #                      tags$img(class = "usa-footer-logo-img",
  #                               src = "www/img/ahrq_logo.png",
  #                               alt = "AHRQ logo")),
  #               tags$h3(class = 'usa-footer-logo-heading',AHRQ)),
  #           
  #           div(class = 'usa-footer-contact-links usa-width-one-half',
  #               tags$a(href = "https://meps.ahrq.gov/mepsweb/",
  #                      tags$img(class = "usa-footer-logo-img",
  #                               src = "www/img/oc_meps_logo_blue.png",
  #                               alt = "MEPS logo")),
  #               tags$address(
  #                 tags$p(MEPS,br(),AHRQ,br(),
  #                   "5600 Fishers Lane",br(),
  #                   "Rockville, MD 20857", br(),
  #                   tags$a(href = "mailto:mepsprojectdirector@ahrq.hhs.gov",
  #                          "mepsprojectdirector@ahrq.hhs.gov"),br(),
  #                   "(301) 427-1406")))
  # )))

  #htmlTemplate("../../template.html", body = meps_body, footer = meps_footer)
  htmlTemplate("../../template.html", body = meps_body)
}

