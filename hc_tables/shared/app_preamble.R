
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(DT)
library(stringr)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(scales)

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

load("TABLES.Rdata")

addResourcePath('www','../../www')