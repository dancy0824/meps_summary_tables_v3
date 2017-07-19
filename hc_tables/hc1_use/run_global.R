
library(dplyr)

evnt_keys <-
  list("DV"="DVT","ER"="ERT","HH"="HHT","IP"="IPT",
       "OB"="OBV","OM"="OMA","OP"="OPT") %>% stack

event_dictionary <- 
  list("TOT"="All event types",
       "DVT"="Dental visits",
       "RX" ="Prescription medicines",
       "OBV"="Office-based events",
       "OBD"="Physician Office visits",
       "OBO"="Non-physician office visits",
       "OPT"="Outpatient events",
       "OPY"="Physician hosp. visits",
       "OPZ"="Non-physician hosp. visits",
       "ERT"="Emergency room visits",
       "IPT"="Inpatient stays",
       "HHT"="Home health events",
       "OMA"="Other medical expenses") %>% stack

sop_dictionary <- 
  list("EXP"="All sources",
       "SLF"="Out of pocket",
       "PTR"="Private",
       "MCR"="Medicare",
       "MCD"="Medicaid",
       "OTZ"="Other") %>% stack

#####################################################################################

join_all <- function(df_list,...){
  out <- df_list[[1]]
  for(df in df_list[-1]) out <- suppressWarnings(full_join(out,df,...))
  out
}

reverse <- function(df) df[nrow(df):1,]

dedup <- function(df){
  df %>% 
    reverse %>% 
    distinct(Year,grp1,grp2,levels1,levels2,.keep_all=TRUE) %>% 
    reverse
}

rm_v2 <- function(df){
  df%>% mutate(grp1 = grp1 %>% gsub("_v2X","",.),
               grp2 = grp2 %>% gsub("_v2X","",.))
}


# Add event and SOP labels
add_labels <- function(df,dictionary, key="ind",vars=c("levels1","levels2")){
  for(var in vars){
    df <- df %>%
      mutate_(temp=var) %>%
      left_join(dictionary,by=c("temp"=key)) %>%
      mutate(temp = coalesce(values,temp))
    df[,var] = df$temp
    df <- df %>% select(-temp,-values)
  }
  return(df)
}

switch_labels <- function(df){
  df %>% 
    mutate(g1=grp1,g2=grp2,l1=levels1,l2=levels2) %>%
    mutate(grp1=g2,grp2=g1,levels1=l2,levels2=l1) %>%
    select(-g1,-g2,-l1,-l2)
}

get_totals <- function(grp,df,label="All persons"){
  totals <- df %>% filter(grp1=="ind",grp2!=grp)
  totals %>%
    mutate(grp1=grp,levels1=label) %>%
    switch_labels
}

reorder_cols <- function(df){
  df2 <- df %>% rowwise %>%
    mutate(ordered = sort(c(grp1,grp2))[1] == grp1,
           ordered = replace(ordered,grp1=='ind',TRUE),
           # ordered = replace(ordered,grp1%in%c("sop","event"),FALSE),
           # ordered = replace(ordered,grp2 == 'sop',TRUE)) %>%
           ordered = replace(ordered,grp1%in%c("sop","event"),FALSE),
           ordered = replace(ordered,grp2%in%c("sop","event"),TRUE)) %>%
    ungroup
  
  Ordered <- df2 %>% filter(ordered)
  Unordered <- df2 %>% filter(!ordered) %>% switch_labels
  
  bind_rows(Ordered,Unordered) %>% select(-ordered)
}
