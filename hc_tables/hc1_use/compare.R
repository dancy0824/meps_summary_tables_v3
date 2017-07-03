
## This can eventually be combined into merge.R

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




