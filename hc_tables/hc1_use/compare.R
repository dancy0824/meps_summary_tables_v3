
library(dplyr)


  ## can delete this later: 
  framify <- function(vec,ncol=1,names=NULL){
    vec %>% matrix(ncol=ncol,byrow=T) %>%
      as.data.frame(stringsAsFactors=F) %>%
      setNames(names)
  }
  
  event_dictionary = c(
    "TOT",   "ind", "All event types",
    "DVT",   "DV",  "Dental visits",
    "RX",    "RX",  "Prescription medicines",
    "OBV",   "OB",  "Office-based events",
    "OBD",   "OBD", "Physician office visits",
    "OBO",   "OBO", "Non-physician office visits",
    "OPT",   "OP",  "Outpatient events",
    "OPY",   "OPY", "Physician hosp. visits",
    "OPZ",   "OPZ", "Non-physician hosp. visits",
    "ERT",   "ER",  "Emergency room visits",
    "IPT",   "IP",  "Inpatient stays",
    "HHT",   "HH",  "Home health events",
    "HHA",   "HHA", "Home health agency",
    "HHN",   "HHN", "Home health independent",
    "OMA",   "OM",  "Other medical expenses",
    "Missing","Missing","Missing"
  ) %>%
    framify(ncol=3,names=c("event","use","label")) 

#######################################
# Compare new R estimates to old ones

compareR <- function(year,stat){
  
  is.event = (stat %in% c("totEVT","meanEVT"))
  
  ftype = ifelse(is.event,"EVT","FYC")
  
  stat_se = paste0(stat,"_se")
  
  setwd("C:/Users/emily.mitchell/Desktop/Programming/GitHub/meps_summary_tables/hc_tables")
  
  old_r = read.csv(sprintf("hc1_use/r/tables_old/%s%s.csv",ftype,year),stringsAsFactors = F)
  new_r = read.csv(sprintf("hc1_use/r/tables/%s/%s.csv",year,stat),stringsAsFactors = F)
  
  old_r = old_r %>% 
    select_("levels1","levels2","grp1","grp2",stat,stat_se) %>%
    mutate_at(vars(levels1,levels2),trimws) %>%
    mutate_at(vars(grp1,grp2),funs(gsub("_v2","",.))) %>%
    rename_(old_stat = stat, old_se = stat_se) %>% 
    tbl_df
  
  new_r = new_r %>% 
    rename_(new_stat = stat, new_se = stat_se) %>%
    mutate_at(vars(grp1,grp2),funs(gsub("_v2X","",.))) %>%
    tbl_df
  
  #############
  # old_r %>% filter(grp1=="sop"|grp2=="sop")
  # new_r %>% filter(grp1=="sop"|grp2=="sop")
  # 
  # old_r %>% filter(grp1=="event"|grp2=="event") %>% print(n=30)
  # new_r %>% filter(grp1=="event"|grp2=="event") %>% print(n=30)
  # 
  if(is.event){
    new_r <- left_join(new_r,event_dictionary,by=c("levels2"="use")) 
    new_2 <- new_r %>%
      mutate(levels2 = ifelse(grp2=="event",event,levels2))
    new_r=new_2
  }

  ############
  
  both <- full_join(old_r,new_r,by=c("grp1","grp2","levels1","levels2"))
  
  diff = both %>% filter(round(old_stat,2) != round(new_stat,2)) 
  diff_se = both %>% filter(round(old_se,2) != round(new_se,2)) 
  
  miss_old = both %>% filter(is.na(old_stat)&!is.na(new_stat)) 
  miss_new = both %>% filter(is.na(new_stat)&!is.na(old_stat))

  #print(list(diff=diff,diff_se=diff_se,miss_old=miss_old,miss_new=miss_new))
  
  return(list(diff=diff,diff_se=diff_se,miss_old=miss_old,miss_new=miss_new))
}

  #some residual 1996 errors in old version for employed (go with new version)
  
yearlist = 1997:2014  
  
printdiffs <- function(difflist,rm_bad=F){
  for(i in 1:length(difflist)){
    cat(i,":",yearlist[[i]],"\n")
    test = difflist[[i]]
  
    if(rm_bad){
      test$diff <- test$diff %>% 
        filter(!levels2 %in% c("OBV","OBD")) %>%
        filter(!levels2 %in% c("EXP","TOT")) %>%
        filter(!levels1 %in% c("EXP","TOT")); 
      
      # old did not count only number of people with event/expense, for all event types/sops
      test$diff_se <- test$diff_se %>% 
        filter(!levels2 %in% c("OBV","OBD")) %>%
        filter(!levels2 %in% c("EXP","TOT")) %>%
        filter(!levels1 %in% c("EXP","TOT")); 
    }
    
    
    
    for(d in test){
      if(nrow(d) > 0) print(d,n=60)
    }
  }
}

yearlist = 1996

R1 = lapply(yearlist,compareR,stat="totPOP") 
R2 = lapply(yearlist,compareR,"pctEXP"); printdiffs(R2);
R3 = lapply(yearlist,compareR,"totEXP"); printdiffs(R3);
R4 = lapply(yearlist,compareR,"meanEXP0"); printdiffs(R4);
R5 = lapply(yearlist,compareR,"meanEXP"); printdiffs(R5);
R6 = lapply(yearlist,compareR,"medEXP"); printdiffs(R6);
R7 = lapply(yearlist,compareR,"n"); printdiffs(R7);
R8 = lapply(yearlist,compareR,"n_exp"); printdiffs(R8);

R9 = lapply(yearlist,compareR,"totEVT"); printdiffs(R9);
R10 = lapply(yearlist,compareR,"meanEVT"); printdiffs(R10);

R1 %>% printdiffs(rm_bad=T)
R7 %>% printdiffs(rm_bad=T)


##################################################
## Compare SAS and R estimates

compare = function(year,stat="n"){ 
  stat_se = paste0(stat,"_se")
  
  sas_stat = switch(stat,
                    "totPOP" = "sum",
                    "totEXP" = "sum",
                    "medEXP" = "estimate",
                    "pctEXP" = "mean",
                    "meanEXP" = "mean",
                    "meanEXP0" = "mean",
                    "n" = "n",
                    "n_exp"="n")

  sas_se = switch(sas_stat,
                  "sum" = "stddev",
                  "estimate" = "stderr",
                  "mean" = "stderr",
                  "n" = "n",
                  "n_exp"="n")

  r = read.csv(sprintf("hc1_use/code/r/tablesX/%s/%s.csv",year,stat),stringsAsFactors = F)
  
  load_stat = stat
  if(stat == "n") load_stat = "totPOP"
  if(stat == "n_exp") load_stat = "meanEXP"

  sas = read.csv(sprintf("hc1_use/code/sas/tables/%s/%s.csv",year,load_stat),stringsAsFactors = F)  
  names(sas) = tolower(names(sas))
  
  r = r %>% 
    rename_(r_stat = stat, r_se = stat_se) %>%
    mutate_at(vars(grp1,grp2),funs(gsub("_v2X","",.))) %>%
    tbl_df
  
  sas = sas %>% select_("levels1","levels2","grp1","grp2",sas_stat,sas_se) %>%
    mutate_at(vars(levels1,levels2),trimws) %>%
    mutate_at(vars(grp1,grp2),funs(gsub("_v2X","",.))) %>%
    rename_(sas_se=sas_se,sas_stat = sas_stat)
  
  both = full_join(sas,r,by = c("levels1","levels2","grp1","grp2"),suffix = c(".sas",".r"))
  
  d = ifelse(stat == "pctEXP",2,0)
  D = ifelse(stat == "totEXP",1E6,1)
  both <- both %>% mutate_if(is.numeric, function(x) round(x/D,d)) %>% tbl_df
  
  
  diff = both %>% filter(r_stat != sas_stat) 
  diff_se = both %>% filter(r_se != sas_se) 
  
  miss_r   = both %>% filter(is.na(r_stat)) %>% filter(grp1!=grp2) # Remove agegrps x agegprs (from v2)
  miss_sas = both %>% filter(is.na(sas_stat))
  
  return(list(diff=diff,diff_se=diff_se,miss_r=miss_r,miss_sas=miss_sas))
}


################################



year = 2014
stat = "totPOP"

compare(year=2014,stat="totPOP")
compare(year=2014,stat="pctEXP")

compare(year=2014,stat="totEXP")
compare(year=2014,stat="meanEXP0")
compare(year=2014,stat="meanEXP")
compare(year=2014,stat="medEXP")




