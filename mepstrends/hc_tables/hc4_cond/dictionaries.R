cond_stats = list(
  "Number of people with care" = "totPOP",
  "Number of events"           = "totEVT",
  "Total expenditures ($)"     = "totEXP",
  "Mean expenditure per person with care ($)"= "meanEXP"
)

grp_labels <- subgrps %>% invertList %>% as.list  

grp_labels$Year = "Year"

stat_labels <- cond_stats %>% invertList %>% gsub(" \\(.*?\\)","",.)
