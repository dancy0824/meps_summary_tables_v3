
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

suppressMessages(library(shiny))
suppressMessages(library(DT))
suppressMessages(library(stringr))
suppressMessages(library(plotly))
suppressMessages(library(RColorBrewer))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(scales))

meps_names <- read.csv("../shared/puf_expanded.csv", stringsAsFactors=F)

source("../../modules/module1_data.R",local=TRUE)
source("../../modules/module2_levels.R",local=TRUE)
source("../../modules/module3_notes.R",local=TRUE)

source("../../modules/tab1_table.R",local=TRUE)
source("../../modules/tab2_plot.R",local=TRUE)
source("../../modules/tab3_code.R",local=TRUE)

source("../shared/app_notes.R",local=TRUE)
source("../shared/app_global.R",local=TRUE)

source("app_info.R",local=T)
source("app_code.R",local=T)
source("dictionaries.R",local=T) # run dictionaries last to overwrite default functions

try(load("TABLES.Rdata"),silent=T)

addResourcePath('www','../../www')