use_svyby <- list(
  'totPOP'   = 'svyby(~(.use. > 0), FUN=svytotal, by=.by., design=FYCdsgn)',
  'totEXP'   = 'svyby(~.event..sop..yy., FUN=svytotal, by=.by., design=FYCdsgn)',
  'pctEXP'   = 'svyby(~(.event..sop..yy. > 0), FUN=svymean, by=.by., design=FYCdsgn)',
  
  'meanEXP0' = 'svyby(~.event..sop..yy., FUN=svymean, by=.by., design=FYCdsgn)',
  'meanEXP'  = 'svyby(~.event..sop..yy., FUN=svymean, by=.by., design=subset(FYCdsgn,.event..sop..yy. > 0))',
  'medEXP'   = 'svyby(~.event..sop..yy., FUN=svyquantile, by=.by., design=subset(FYCdsgn,.event..sop..yy. > 0), quantiles=c(0.5), ci=T, method="constant")',
  
  'n' = 'svyby(~.use., FUN=unwtd.count, by=.by., design=subset(FYCdsgn, .use. > 0))',
  'n_exp' = 'svyby(~.use., FUN=unwtd.count, by=.by., design=subset(FYCdsgn, .event..sop..yy. > 0))',
  
  'totEVT' = 'svyby(~.use., FUN=svytotal, by=.by., design=subset(EVNTdsgn, .sop..yy.X >= 0))',
  'meanEVT' = 'svyby(~.sop..yy.X, FUN=svymean, by=.by., design=subset(EVNTdsgn, .sop..yy.X >= 0))'
)

