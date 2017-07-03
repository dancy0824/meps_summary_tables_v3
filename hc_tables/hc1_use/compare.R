
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
  
  diff = both %>% filter(round(old_stat,3) != round(new_stat,3)) 
  diff_se = both %>% filter(round(old_se,3) != round(new_se,3)) 
  
  miss_old = both %>% filter(is.na(old_stat)&!is.na(new_stat)) 
  miss_new = both %>% filter(is.na(new_stat)&!is.na(old_stat))

  print(list(diff=diff,diff_se=diff_se,miss_old=miss_old,miss_new=miss_new))
  
  return(list(both=both,diff=diff,diff_se=diff_se,miss_old=miss_old,miss_new=miss_new))
}

year=2014

r1 = compareR(year,"totPOP");  

# for totpop, old has wrong OBD/OBV utilization var
r1$diff <- r1$diff %>% filter(!levels2 %in% c("OBV","OBD")); r1$diff;

# old did not count only number of people with event/expense, for all event types/sops
r1$diff <- r1$diff %>% 
  filter(!levels2 %in% c("EXP","TOT")) %>%
  filter(!levels1 %in% c("EXP","TOT")); r1$diff;


r2 = compareR(year,"pctEXP"); 
r3 = compareR(year,"totEXP"); 
r4 = compareR(year,"meanEXP0");
r5 = compareR(year,"meanEXP");  #r5$miss_new %>% print(n=100);
r6 = compareR(year,"medEXP");  #r6$miss_new %>% print(n=100);

r7 = compareR(year,"n"); # this should have the same issues as r1
r8 = compareR(year,"n_exp"); 

r9 = compareR(year,"totEVT"); 
r10 = compareR(year,"meanEVT"); 

r9$diff %>% filter(grp1=="sop")

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




