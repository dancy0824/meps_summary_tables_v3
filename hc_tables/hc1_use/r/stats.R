meps_svyby <- list(
  'totPOP'   = 'svyby(~(.count. > 0), FUN=svytotal, by=.by., design=FYCdsgn)',
  'totEXP'   = 'svyby(~.event..sop..yy., FUN=svytotal, by=.by., design=FYCdsgn)',
  'pctEXP'   = 'svyby(~(.event..sop..yy. > 0), FUN=svymean, by=.by., design=FYCdsgn)',
  
  'meanEXP0' = 'svyby(~.event..sop..yy., FUN=svymean, by=.by., design=FYCdsgn)',
  'meanEXP'  = 'svyby(~.event..sop..yy., FUN=svymean, by=.by., design=subset(FYCdsgn,.event..sop..yy. > 0))',
  'medEXP'   = 'svyby(~.event..sop..yy., FUN=svyquantile, by=.by., design=subset(FYCdsgn,.event..sop..yy. > 0), quantiles=c(0.5), ci=T, method="constant")',
  
  'n' = 'svyby(~.count., FUN=unwtd.count, by=.by., design=subset(FYCdsgn, .count. > 0))',
  'n_exp' = 'svyby(~.count., FUN=unwtd.count, by=.by., design=subset(FYCdsgn, .event..sop..yy. > 0))',
  
  'totEVT' = 'svyby(~.use., FUN=svytotal, by=.by., design=subset(EVNTdsgn, .sp..yy.X >= 0))',
  'meanEVT' = 'svyby(~.sp..yy.X, FUN=svymean, by=.by., design=subset(EVNTdsgn, .sp..yy.X >= 0))'
)

meps_svy <- list(
  'totPOP'   = 'svytotal(~(.count. > 0), design=FYCdsgn)',
  'totEXP'   = 'svytotal(~.event..sop..yy., design=FYCdsgn)',
  'pctEXP'   = 'svymean(~(.event..sop..yy. > 0), design=FYCdsgn)',
  
  'meanEXP0' = 'svymean(~.event..sop..yy.,design=FYCdsgn)',
  'meanEXP'  = 'svymean(~.event..sop..yy., design=subset(FYCdsgn,.event..sop..yy. > 0))',
  'medEXP'   = 'svyquantile(~.event..sop..yy., design=subset(FYCdsgn,.event..sop..yy. > 0), quantiles=c(0.5), ci=T, method="constant")',

  'totEVT' = 'svytotal(~.use., design=subset(EVNTdsgn, .sp..yy.X >= 0))',
  'meanEVT' = 'svymean(~.sp..yy.X, design=subset(EVNTdsgn, .sp..yy.X >= 0))'
)
