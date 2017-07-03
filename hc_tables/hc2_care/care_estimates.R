#############################################
###  Access to care and quality of care   ###
#############################################

care_fun <- function(grp1,grp2,yr){
  code_s = care_code(grp=grp1,care=grp2,yr=yr)
  samp_s = care_code(grp=grp1,care=grp2,yr=yr,type="unwtd.count")
  
  est <- eval(parse(text=code_s))
  smp <- eval(parse(text=samp_s))
  
  est2 <- est %>% 
    rename_(levels1 = grp1) %>%
    gather(group,percent,-levels1) %>%
    separate(group,c("coef","levels2"),sep="\\.",fill="left") %>%
    mutate(levels2 = gsub(grp2,"",levels2)) %>%
    mutate(levels2 = factor(levels2,levels = unique(levels2))) %>%
    mutate(coef = replace(coef,is.na(coef),"coef")) %>%
    spread(coef,percent) %>%
    std(key="pctPOP")
  
  smp2 <- smp %>% 
    rename_(levels1 = grp1, n = 'counts') %>%
    select(-se)
  
  full_join(est2,smp2) %>%
    mutate(grp1=grp1,grp2=grp2) %>%
    select(grp1,grp2,levels1,levels2,pctPOP,pctPOP_se,n) 
}

#####################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../run_preamble.R",chdir=T) # shared
source("care_snippets.R")

# year = 2014

for(year in yearlist){   print(year)
  
  this_year = meps_names %>% filter(Year==year) 
  file_names = as.list(this_year$url) %>% setNames(this_year$file_type)
  
  yr <- substring(year,3,4); 
  yb <- substring(year-1,3,4)
  ya <- substring(year+1,3,4)

  care_csv <- sprintf("tables/CARE%s.csv",year)
  smp_csv  <- sprintf("tables/SMP%s.csv",year)
  
  # Load data
    load_fyc <- snp_load$load_fyc %>% rsub(file_names,yy=yr)
    writeLines(load_fyc); eval(parse(text = load_fyc));
    
  # Add subgroup variables to FYC data
    for(snp in snp_subgrps) eval(parse(text = snp %>% rsub(yy=yr)))
    for(snp in snp_care) eval(parse(text = snp %>% rsub(yy=yr,ya=ya,yb=yb)))

  # Define design
    eval(parse(text=snp_dsgn$fyc %>% rsub(yy=yr)))
    eval(parse(text=snp_dsgn$diab %>% rsub(yy=yr)))  
    eval(parse(text=snp_dsgn$saq %>% rsub(yy=yr)))  
    
  # Run estimates 
    care_vec = care_subgrps %>% unlist %>% setNames(NULL)

    redo_subgrps = NULL
    #redo_subgrps = all_subgrps
    
    for(grp1 in all_subgrps){
      for(care_stat in care_vec){
        force = (any(c(grp1,care_stat) %in% redo_subgrps))
        run(care_fun,file=care_csv,grp1=grp1,grp2=care_stat,yr=yr,force = force)
      }
    }

} # end of yearlist loop



 