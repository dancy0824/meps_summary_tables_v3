# Download FYC file from MEPS website
  FYC <- read.xport('.PUFdir./.FYC..ssp'); 
  year <- .year.

  if(year <= 2001) FYC <- FYC %>% mutate(VARPSU = VARPSU.yy., VARSTR=VARSTR.yy.)
  if(year <= 1998) FYC <- FYC %>% rename(PERWT.yy.F = WTDPER.yy.) 
  
  FYC$ind = 1