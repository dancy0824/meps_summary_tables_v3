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

## Write sas code to .sas programs

for(year in c(2013,2007,2001,1996)){
  for(stat in c(fyc_stats,evnt_stats,"avgEVT")){
    for(g1 in c("ind","race","event")){
      for(g2 in c("insurance","sop")){
        
        if(g1==g2) next
        
        Str = get_sas_code(g1,g2,stat=stat,year=year)
        
        write(Str,file=sprintf("TEST/%s_%s_%s_%s.sas",stat,g1,g2,year))
        
      }}}}


# 2013 totevt event insurance (need ~event)
# 2013 totevt event sop (need ~event)
# 2013 meanevt event insurance (need ~event)
# 2013 meanevt event sop (need ~event)
# all avgEVT

# get_r_code('ind','insurance','avgEVT',2013) %>% writeLines
# out <- get_r_code('ind','insurance','avgEVT',2013) %>% run

for(year in c(2013,2007,2001,1996)){
  #for(stat in c(fyc_stats,evnt_stats)){
  for(stat in "avgEVT"){
    for(g1 in c("ind","race","event")){
      for(g2 in c("insurance","sop")){
      
      print(c(year,stat,g1,g2))
      if(g1==g2) next
  
      stat_se = paste0(stat,"_se")
      
      t1 <- use_tables %>%
        filter(Year==year,grp1==g1,grp2==g2) %>%
        select(grp1,grp2,levels1,levels2,stat,stat_se,"totEVT","totEVT_se",Year)
      
      t2 <- use_tables %>%
        filter(Year==year,grp1==g2,grp2==g1) %>%
        select(grp1,grp2,levels1,levels2,stat,stat_se,Year)
      
      if(nrow(t1) > 0) print(t1)
      if(nrow(t2) > 0) print(t2)
      
      urlname = sprintf(
        "file:///C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables/hc1_use/TEST_HTML/%s_%s_%s_%s.html",
        stat,g1,g2,year
      )
      
      browseURL(urlname)
      
      readline(prompt="Press [enter] to continue")
      
    }
  }

}}

###############

#PARAMS <- TEST <- COMP <- list()

i = 1
for(year in c(2013,2007,2001,1996)){
  for(stat in c(fyc_stats,evnt_stats)){
    for(g1 in c("ind","insurance","event","sop")){
      for(g2 in c("race","sop")){
        
        if(g1==g2) next

        if(i <= length(PARAMS)){
          print(i)
          i = i+1
          next
        }

        stat_se = paste0(stat,"_se")

        grps = c(g1,g2)

        test <- get_r_code(rows=g1,cols=g2,stat=stat,year=year) %>% run

        PARAMS[[i]] = list(year=year,stat=stat,g1=g1,g2=g2)
        TEST[[i]] = test
        COMP[[i]] = use_tables %>%
          filter(Year==year,grp1==g1,grp2==g2) %>%
          select(grp1,grp2,levels1,levels2,stat,stat_se,Year)

        i = i+1
        
        rm(list=c("FYC","stacked_events","EVENTS","pers_events","n_events"))
        
      }
    }
  }
}

getwd()

#save(PARAMS,TEST,COMP,file="test_code.Rdata")


load("test_code.Rdata")

i = 1


i = i+round(runif(1,10,20))
PARAMS[[i]]
TEST[[i]]
COMP[[i]] 

use_tables %>%
  filter(Year == PARAMS[[i]]$year,
         grp1 == PARAMS[[i]]$g2,
         grp2 == PARAMS[[i]]$g1) %>%
  select_("grp1","grp2","levels1","levels2",PARAMS[[i]]$stat)
print(i)







