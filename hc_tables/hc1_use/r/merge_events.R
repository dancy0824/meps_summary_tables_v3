stacked_events <- bind_rows(RX,DVT,OMA,IPT,ERT,OPT,OBV,HHT)
EVENTS <- stacked_events %>% full_join(FYCsub, by='DUPERSID')
