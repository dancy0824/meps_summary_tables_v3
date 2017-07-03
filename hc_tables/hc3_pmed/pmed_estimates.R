#####################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../run_preamble.R",chdir=T) # shared


#source("pmed_snippets.R")

# For 2013-2014, RX

year = 2012

yr = substring(year,3,4)
this_year = meps_names %>% filter(Year==year) 
file_names = as.list(this_year$url) %>% setNames(this_year$file_type)

rxName = file_names$RX

RX <- downloadMEPS(rxName)

names(RX) <- gsub(yr,"",names(RX))

RXdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWTF,           
  data = RX,
  nest = TRUE)

# drgnames = unique(RX$RXDRGNAM) %>% sort
# dd <- data.frame(drgnames)
#########################################
## before 2013: RXNAME??

ndc <- read.csv("NDC/product.csv")
ndc <- ndc %>% select(PRODUCTNDC,PROPRIETARYNAME,NONPROPRIETARYNAME,SUBSTANCENAME)


dd <- dd %>% mutate(drgnames = as.character(drgnames),dupilcate=drgnames)

head(dd)

RX <- RX %>% mutate(drgnames = RXNAME)

neat <- difference_inner_join(RX,dd,by=c("drgnames"))



dist.name <- adist(RX$drgnames,dd$drgnames,ignore.case = T)

min.name <- apply(dist.name,1,min)

RX %>% select(RXNAME,RXNDC) %>% 
  filter(grepl("WARFARIN",RXNAME)) %>%
  mutate(RXNDC2 = substring(RXNDC,1,9)) %>%
  select(RXNAME,RXNDC2) %>%
  unique 

head(ndc)

ndc %>%
  filter(grepl("lipitor",tolower(PROPRIETARYNAME))) 

ndc %>%
  filter(grepl("0113",PRODUCTNDC)) %>%
  head(30)



# 
# drgs <- RX %>% group_by(RXNAME) %>%
#   summarise(nexp = sum(RXXPX*PERWTF)) %>%
#   arrange(-nexp)
# 
# tc1s = drgs$RXNAME[1:50]
# tc1s = tc1s[tc1s != '-9']
# 
# print(length(tc1s))
# 
# out <- list(); i = 1;
# for(rx in tc1s){
#   print(i); print(rx);
#   out[[rx]] <- svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, RXNAME == rx ))
#   i = i+1;
# }
# 
# new <- data.frame(); i = 1;
# for(rx in tc1s){
#   temp = out[[rx]]  
#   new[i,1] = coef(temp) %>% c
#   new[i,2] = SE(temp) %>% c
#   new[i,3] = rx
#   i = i+1;
# }
# colnames(new) = c("RXXP","SE","DRG")
# new %>% head
# 
# # Read in TC1 names
# tc1_names = read.csv("tc1_names.csv")
# 
# tcn <- tc1_names %>% separate(TCNAME,into=c("space","number","DRG"),sep=' ',fill="right",extra="merge")
# tcn <- tcn %>% mutate(DRG = gsub("'",'',DRG)) %>%
#   mutate(one=1) %>% select(DRG,one)
# 
# new2 <- left_join(new,tcn,by="DRG")
# new2 %>% arrange(-RXXP) %>% filter(is.na(one)) %>% head(30)


#################################################








#########################################
## this works with RXDRGNAM: 2013-2014

drgs <- RX %>% group_by(RXDRGNAM) %>%
  summarise(nexp = sum(RXXPX*PERWTF)) %>%
  arrange(-nexp)

tc1s = drgs$RXDRGNAM[1:50]
tc1s = tc1s[tc1s != '-9']

print(length(tc1s))

out <- list(); i = 1;
for(rx in tc1s){
  print(i); print(rx);
  out[[rx]] <- svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, RXDRGNAM == rx ))
  i = i+1;
}

new <- data.frame(); i = 1;
for(rx in tc1s){
  temp = out[[rx]]  
  new[i,1] = coef(temp) %>% c
  new[i,2] = SE(temp) %>% c
  new[i,3] = rx
  i = i+1;
}
colnames(new) = c("RXXP","SE","DRG")
new %>% head

# Read in TC1 names
tc1_names = read.csv("tc1_names.csv")

tcn <- tc1_names %>% separate(TCNAME,into=c("space","number","DRG"),sep=' ',fill="right",extra="merge")
tcn <- tcn %>% mutate(DRG = gsub("'",'',DRG)) %>%
  mutate(one=1) %>% select(DRG,one)

new2 <- left_join(new,tcn,by="DRG")
new2 %>% arrange(-RXXP) %>% filter(is.na(one)) %>% head(30)


#################################################


#########################################
## This works for TC1: 2005-2014
tc1s = unique(RX$TC1)
tc1s = tc1s[tc1s >= 0]

out <- list(); i = 1;
for(rx in tc1s){
  print(i); print(rx);
  out[[rx]] <- svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, TC1 == rx ))
  i = i+1;
}

new <- data.frame(); i = 1;
for(rx in tc1s){
  temp = out[[rx]]  
  new[i,1] = coef(temp) %>% c
  new[i,2] = SE(temp) %>% c
  new[i,3] = rx
  i = i+1;
}
colnames(new) = c("RXXP","SE","TC1")
new %>% head

# Read in TC1 names
tc1_names = read.csv("tc1_names.csv")

new2 <- left_join(new,tc1_names,by="TC1")
new2 %>% arrange(-RXXP) %>% head(30)
#################################################







RX$lipitor = RX$TC1 %>% startsWith("LIP")

svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, startsWith(as.character(RXNAME),"LIPITOR") ))
svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, startsWith(as.character(RXNAME),"SIMVASTATIN") ))
svytotal(~RXXPX, FUN = svytotal, design=subset(RXdsgn, startsWith(as.character(RXNAME),"NEXIUM") ))

lipiRX <- RX %>% mutate(RXC = as.character(RXNAME)) %>% 
  filter(grepl("LIPITOR",RXC)) %>% 
  select(RXNDC,RXNAME) %>% unique %>% select(RXNDC) %>% unlist

RX %>% filter(RXNDC %in% lipiRX) %>%
  select(RXNDC,RXNAME) %>% unique %>% arrange(RXNDC)

svytotal(~RXXP08X, FUN = svytotal, design=subset(RXdsgn, RXNDC %in% lipiRX ))


simvaRX <- RX %>% mutate(RXC = as.character(RXNAME)) %>% 
  filter(grepl("SIMVASTATIN",RXC)) %>% 
  select(RXNDC,RXNAME) %>% unique %>% select(RXNDC) %>% unlist

RX %>% filter(RXNDC %in% simvaRX) %>%
  select(RXNDC,RXNAME) %>% unique %>% arrange(RXNDC)

svytotal(~RXXP08X, FUN = svytotal, design=subset(RXdsgn, RXNDC %in% simvaRX ))


nexiRX <- RX %>% mutate(RXC = as.character(RXNAME)) %>% 
  filter(grepl("NEXIUM",RXC)) %>% 
  select(RXNDC,RXNAME) %>% unique %>% select(RXNDC) %>% unlist

RX %>% filter(RXNDC %in% nexiRX) %>%
  select(RXNDC,RXNAME) %>% unique %>% arrange(RXNDC)

svytotal(~RXXP08X, FUN = svytotal, design=subset(RXdsgn, RXNDC %in% nexiRX ))


