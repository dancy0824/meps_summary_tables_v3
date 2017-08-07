#rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("TABLES.Rdata")

source("../shared/app_preamble.R", chdir=F, local=T)
source("app_code.R")

run <- function(codeString,verbose=T){
  if(verbose) writeLines(codeString)
  eval(parse(text=codeString),envir=.GlobalEnv)
}

##################

# PARAMS <- TEST <- COMP <- list()

care_grps = unlist(care_subgrps) %>% setNames(NULL)

#load("test_code.Rdata")

i = 1
for(year in c(2013,2007,2002)){
  for(stat in care_grps){
    for(g1 in c("ind","race")){
          
        if(i <= length(PARAMS)){
          print(i)
          i = i+1
          next
        }

        test <- get_r_code(rows=g1,cols=stat,stat="pctPOP",year=year) %>% run
        
        PARAMS[[i]] = list(year=year,stat=stat,g1=g1)
        
        TEST[[i]] = test
        
        COMP[[i]] = care_tables %>%
          filter(Year==year,grp1==g1,grp2==stat) %>%
          select(grp1,grp2,levels1,levels2,"pctPOP","pctPOP_se",Year)
        
        i = i+1
        
        #save(PARAMS,TEST,COMP,file="test_code.Rdata")
        
        rm(FYC)
    }
  }
}


year = 2007
stat = "diab_eye"
g1 = 'ind'




#load("test_code.Rdata")

i = 1


i = i+round(runif(1,5,10))

PARAMS[[i]]
TEST[[i]]
COMP[[i]] 







