
meps_svyby <- list(
  'usc' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(FYCdsgn, ACCELI42==1 & HAVEUS42 >= 0 & LOCATN42 >= -1))',

  'difficulty' ='svyby(~delay_ANY + delay_MD + delay_DN + delay_PM, FUN = svymean, by = ~.by., design=subset(FYCdsgn, ACCELI42==1))',
  
  'rsn_MD' = 'svyby(~afford_MD + insure_MD + other_MD, FUN = svymean, by = ~.by., design=subset(FYCdsgn, ACCELI42==1 & delay_MD==1))',
  'rsn_DN' = 'svyby(~afford_DN + insure_DN + other_DN, FUN = svymean, by = ~.by., design=subset(FYCdsgn, ACCELI42==1 & delay_DN==1))',
  'rsn_PM' = 'svyby(~afford_PM + insure_PM + other_PM, FUN = svymean, by = ~.by., design=subset(FYCdsgn, ACCELI42==1 & delay_PM==1))',
    
  'diab' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=DIABdsgn)',
  
  'adult_nosmok'  = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(SAQdsgn, ADSMOK42==1 & CHECK53==1))',
  'adult_routine' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(SAQdsgn, ADRTCR42==1 & AGELAST >= 18))',
  'adult_illness' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(SAQdsgn, ADILCR42==1 & AGELAST >= 18))',
  
  'child_dental' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(FYCdsgn, child_2to17==1))',
  'child_routine' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(FYCdsgn, CHRTCR42==1 & AGELAST < 18))',
  'child_illness' = 'svyby(~.formula., FUN = svymean, by = ~.by., design=subset(FYCdsgn, CHILCR42==1 & AGELAST < 18))',
 
  'adult' = 'svyby(~.formula., FUN=svymean, by = ~.by., design=subset(SAQdsgn, ADAPPT42 >= 1 & AGELAST >= 18))',
  'child' = 'svyby(~.formula., FUN=svymean, by = ~.by., design=subset(FYCdsgn, CHAPPT42 >= 1 & AGELAST < 18))'
)


meps_svy <- list(
  'usc' = 'svymean(~.formula., design=subset(FYCdsgn, ACCELI42==1 & HAVEUS42 >= 0 & LOCATN42 >= -1))',
  
  'difficulty' ='svymean(~delay_ANY + delay_MD + delay_DN + delay_PM, design=subset(FYCdsgn, ACCELI42==1))',
  
  'rsn_MD' = 'svymean(~afford_MD + insure_MD + other_MD, design=subset(FYCdsgn, ACCELI42==1 & delay_MD==1))',
  'rsn_DN' = 'svymean(~afford_DN + insure_DN + other_DN, design=subset(FYCdsgn, ACCELI42==1 & delay_DN==1))',
  'rsn_PM' = 'svymean(~afford_PM + insure_PM + other_PM, design=subset(FYCdsgn, ACCELI42==1 & delay_PM==1))',
  
  'diab' = 'svymean(~.formula., FUN = svymean, by = ~.by., design=DIABdsgn)',
  
  'adult_nosmok'  = 'svymean(~.formula., design=subset(SAQdsgn, ADSMOK42==1 & CHECK53==1))',
  'adult_routine' = 'svymean(~.formula., design=subset(SAQdsgn, ADRTCR42==1 & AGELAST >= 18))',
  'adult_illness' = 'svymean(~.formula., design=subset(SAQdsgn, ADILCR42==1 & AGELAST >= 18))',
  
  'child_dental' = 'svymean(~.formula., design=subset(FYCdsgn, child_2to17==1))',
  'child_routine' = 'svymean(~.formula., design=subset(FYCdsgn, CHRTCR42==1 & AGELAST < 18))',
  'child_illness' = 'svymean(~.formula., design=subset(FYCdsgn, CHILCR42==1 & AGELAST < 18))',
  
  'adult' = 'svymean(~.formula., design=subset(SAQdsgn, ADAPPT42 >= 1 & AGELAST >= 18))',
  'child' = 'svymean(~.formula., design=subset(FYCdsgn, CHAPPT42 >= 1 & AGELAST < 18))'
)
