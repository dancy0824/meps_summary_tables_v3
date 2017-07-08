
## This can eventually be combined into merge.R

##################################################
## Compare SAS and R estimates

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

##################################################

compare <- function(df1,df2,stat){
  both <- full_join(df1,df2,by=c("grp1","grp2","levels1","levels2"))
  
  diff = both %>% 
    mutate_(stat1=paste0(stat,".x"),
            stat2=paste0(stat,".y"),
            se1 = paste0(stat,"_se.x"),
            se2 = paste0(stat,"_se.y"))
  
  if(stat %in% c("n","n_exp")){
    diff <- diff %>% mutate(se1 = 0, se2 = 0)
  }
  
  diff <- diff %>%
    filter(stat1 != stat2 | se1 != se2 | is.na(stat1) | is.na(stat2)) %>%
    select(grp1,grp2,levels1,levels2,stat1,stat2,se1,se2) %>%
    tbl_df

  if(nrow(diff)==0) return(NULL)
  return(diff)
}

# Loop over stats, years
compare_all <- function(statlist,yearlist,FUN=compareR){
  diff <- 
    lapply(statlist,function(x)
      lapply(yearlist,FUN,stat=x) %>% setNames(paste0("Y",years))
    ) %>% setNames(statlist)
  diff
}

################################################

compareR <- function(year,stat="totPOP"){  print(paste0(year,":",stat));
  r <- read.csv(sprintf("r/tables/%s/%s.csv",year,stat),stringsAsFactors = F)
  old_r <-  read.csv(sprintf("r/tables_baseline/%s/%s.csv",year,stat),stringsAsFactors = F)
  
  old_r <- old_r %>% mutate(
    levels1=replace(levels1,levels1=="XP","EXP"),
    levels1=replace(levels1,levels1=="SF","SLF"),
    levels1=replace(levels1,levels1=="PR","PTR"),
    levels1=replace(levels1,levels1=="MR","MCR"),
    levels1=replace(levels1,levels1=="MD","MCD"),
    levels1=replace(levels1,levels1=="OZ","OTZ"),
    
    levels2=replace(levels2,levels2=="XP","EXP"),
    levels2=replace(levels2,levels2=="SF","SLF"),
    levels2=replace(levels2,levels2=="PR","PTR"),
    levels2=replace(levels2,levels2=="MR","MCR"),
    levels2=replace(levels2,levels2=="MD","MCD"),
    levels2=replace(levels2,levels2=="OZ","OTZ"))
  
  compare(r,old_r,stat=stat)
}

years <- 2014:1996
stats <- c("totPOP","totEXP","pctEXP","meanEXP0","meanEXP","medEXP",
           "n","n_exp",
           "totEVT","meanEVT")


R_diffs <- compare_all(stats,years,compareR)
R_diffs





#############################################

compareSAS = function(year,stat="totPOP"){  print(paste0(year,":",stat));
  
  stat_se = paste0(stat,"_se")
  
  sas_stat = switch(stat,
    "totPOP" = "sum", 
    "totEXP" = "sum",
    "medEXP" = "estimate", 
    "pctEXP" = "mean",
    "meanEXP" = "mean",
    "meanEXP0" = "mean",
    "meanEVT" = "mean",
    "totEVT" = "sum")

  sas_se = switch(sas_stat,
    "sum" = "stddev",
    "estimate" = "stderr",
    "mean" = "stderr")

  load_stat = stat
  # if(stat == "n") load_stat = "totPOP"
  # if(stat == "n_exp") load_stat = "meanEXP"
  
  r = read.csv(sprintf("r/tables/%s/%s.csv",year,stat),stringsAsFactors = F)
  sas = read.csv(sprintf("sas/tables/%s/%s.csv",year,load_stat),stringsAsFactors = F)  
  names(sas) = tolower(names(sas))
  sas[,c(stat,stat_se)] = sas[,c(sas_stat,sas_se)]
  
  d = ifelse(stat == "pctEXP",6,0)
  D = ifelse(stat == "totEXP",1E6,1)
  
  r <- r %>% mutate_if(is.numeric, function(x) round(x/D,digits=d))
  sas <- sas %>% mutate_if(is.numeric, function(x) round(x/D,digits=d)) %>%
    filter( gsub("_v2X","",grp1)!=gsub("_v2X","",grp2) | grp1 == 'ind')
    #mutate_at(vars(levels1,levels2),trimws)
  
  sas <- sas %>% mutate(
    levels2=replace(levels2,levels2=="OBT","OBV"),
    levels2=replace(levels2,levels2=="OPD","OPY"),
    levels2=replace(levels2,levels2=="OPO","OPZ"),
    levels2=replace(levels2,levels2=="IPD","IPT"),
    levels2=replace(levels2,levels2=="HHI","HHN"))
  
  compare(r,sas,stat=stat)
}

years <- 2014
stats <- c("totPOP","totEXP","pctEXP","meanEXP0","meanEXP","medEXP",
           "meanEVT","totEVT")

stats = c("totPOP")

SAS_diffs <- compare_all(stats,years,compareSAS)

SAS_diffs$totPOP
sas %>% filter(grp1=="ind",grp2=="event") #%>% head

SAS_diffs$totPOP$Y2014 %>% print(n=200)


## !! SOP and EVENT stats are very wrong
# 
# diffs <- compareSAS(2014,"totPOP") ; diffs;
# 
# diffs %>% filter(grp2 != 'event')
# 
# diffs %>% filter(is.na(stat1))
# 
# diffs %>% filter(grp2 == "sop",grp1=="ind")
#   filter(!grp2 %in%  c('event','sop')) %>%
#   print(n=100)
# 



SAS_diffs$n$Y2014 %>% print(n=100)


for(stat in stats){
  for(year in years){
    print(stat)
    print(year)
    Y = paste0("Y",year)
    out <- SAS_diffs[[stat]][[Y]]
    # out <- out %>% 
    #   filter(grp1 != 'race') %>%
    #   filter(grp2 != 'race') 
    # 
    if(nrow(out) > 0 ) print(out)
    
  }
}


# Errors: 
#  OTZ x OMA (sop, event) -- fixed
#  PTR X OMA (2006,2005) -- fixed
# n: sop -- fixed maybe?
# race: < 2011 -- fixed probably


SAS_diffs$n$Y2014 %>%
  filter(grp2 != 'sop')

SAS_diffs$totPOP$Y2010

SAS_diffs$totPOP$Y2011 %>% filter(grp2 != 'race') %>%
  filter(grp1 != 'race')

################################



year = 2014
stat = "totPOP"

compare(year=2014,stat="totPOP")
compare(year=2014,stat="pctEXP")

compare(year=2014,stat="totEXP")
compare(year=2014,stat="meanEXP0")
compare(year=2014,stat="meanEXP")
compare(year=2014,stat="medEXP")




