print("loading snippetsZ1_design.R")


svyby_s <- 'svyby(~&formula., FUN = &FUN., by = ~&by., design = &design.)'
svy_s <- '&FUN.(~&formula., design = &design.)'


snp_dsgn <- list()

snp_dsgn$fyc <-  "  
FYCdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT&yy.F,           
  data = FYC,
  nest = TRUE)
"

snp_dsgn$evnt <- "  
EVNTdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT&yy.F,           
  data = EVENTS,
  nest = TRUE)
"

snp_dsgn$diab <- "
DIABdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~DIABW&yy.F,
  data = FYC,
  nest = TRUE)
"

snp_dsgn$saq <-"
SAQdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~SAQWT&yy.F,
  data = FYC,
  nest = TRUE)
"





