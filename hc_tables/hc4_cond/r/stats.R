meps_demo <- list(
  'totPOP' = 'svyby(~count, by = .by., FUN=svytotal,design=PERSdsgn)',
  'totEVT' = 'svyby(~count, by = .by., FUN=svytotal,design=EVNTdsgn)',
  'totEXP' = 'svyby(~XP.yy.X, by = .by., FUN=svytotal,design=PERSdsgn)',
  'meanEXP'= 'svyby(~XP.yy.X, by = .by., FUN=svymean,design=PERSdsgn)'
)

meps_event <- list(
  'totPOP' = 'svyby(~count, by = ~Condition+event,FUN=svytotal,design=PERSevnt)',
  'totEVT' = 'svyby(~count, by = ~Condition+event,FUN=svytotal,design=EVNTdsgn)',
  'totEXP' = 'svyby(~XP.yy.X, by = ~Condition+event,FUN=svytotal,design=PERSevnt)',
  'meanEXP'= 'svyby(~XP.yy.X, by = ~Condition+event,FUN=svytotal,design=PERSevnt)'
)

meps_sop <- list(
  'totPOP' = 'svyby(~(XP.yy.X>0)+(SF.yy.X>0)+(MR.yy.X>0)+(MD.yy.X>0)+(PR.yy.X>0)+(OZ.yy.X>0),
    by = ~Condition,FUN=svytotal,design=PERSdsgn)',
  
  'totEVT' = 'svyby(~(XP.yy.X>0)+(SF.yy.X>0)+(MR.yy.X>0)+(MD.yy.X>0)+(PR.yy.X>0)+(OZ.yy.X>0), 
    by = ~Condition,FUN=svytotal,design=EVNTdsgn)',
  
  'totEXP' = 'svyby(~(XP.yy.X>0)+(SF.yy.X>0)+(MR.yy.X>0)+(MD.yy.X>0)+(PR.yy.X>0)+(OZ.yy.X>0), 
    by = ~Condition,FUN=svytotal,design=EVNTdsgn)',
  
  'meanEXP'= 'svyby(~XP.yy.X+SF.yy.X+MR.yy.X+MD.yy.X+PR.yy.X+OZ.yy.X,
    by = ~Condition,FUN=svymean,design=PERSdsgn)'
)

