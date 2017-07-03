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

library(htmltools)
library(shiny)

build_preview <- function(info,folder,path,col_width=12){
  column(width = col_width,
         tags$a(href = paste0(path,folder,"/"),class = "preview-box",
                h1(info$title),
                p(info$description))
  )
}

#############################################################################

hc_apps = "hc_tables/apps/"

apps = list()
for(dir in list.files(hc_apps)){
  apps[[dir]] = source(paste0(hc_apps,dir,"/info.R"))$value
}

hide = c("hc2_care","hc3_pmed")
apps = apps[!names(apps) %in% hide]

col_width = 12 / length(apps)

app_list <- mapply(build_preview, apps, names(apps),
                   path = hc_apps, col_width = col_width, SIMPLIFY=F)

body_ui <- bootstrapPage(
  
  div(class = "container",
    fluidRow(
      tagList(app_list)
    )
  )
)

ui <- htmltools::htmlTemplate("template.html", body = body_ui)

write(as.character(ui),file = 'index.html')

