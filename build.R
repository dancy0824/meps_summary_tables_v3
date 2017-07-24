## The following package versions are required for ggplotly responsive plot rendering 

# ggplot2: 2.2.0
# plotly: 4.5.6
# R: 3.3.3

# require(devtools)
# install_version("plotly", version = "4.5.6", repos = "http://cran.us.r-project.org")


## Run these commands to organize packrat

# packrat::init()   # initialize packrat
# packrat::clean()    # remove unused packages
# packrat::snapshot() # save the current state of your library
# packrat::bundle()   # Bundle a packrat project, for easy sharing.

#############################################################################
##   Build index.html

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("hc_tables/shared/app_functions.R")
source("hc_tables/shared/app_functions508.R")

library(htmltools)
library(shiny)

build_preview <- function(info,folder,path,col_width="one-half"){
  col508(width = col_width,
         tags$a(href = paste0(path,folder,"/"),class = "preview-box",
                h1(info$title),
                p(info$description))
  )
}

#############################################################################

hc_apps = "hc_tables/"

applist = c("hc1_use","hc2_care")

apps = list()
for(dir in applist){
  apps[[dir]] = source(paste0(hc_apps,dir,"/app_info.R"))$value
}

app_list <- mapply(build_preview, apps, names(apps),
                   path = hc_apps, col_width = "one-half", SIMPLIFY=F)

index_body <- bootstrapPage(
  div(class = "container",
    fluidRow508(
      tagList(app_list)
    )
  )
)

index_header <- build_header(dir="hc_tables")

ui <- htmltools::htmlTemplate("template.html", body = index_body, header = index_header)

write(as.character(ui),file = 'index.html')

