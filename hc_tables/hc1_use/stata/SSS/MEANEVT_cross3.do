forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
encode evntyp,gen(evtype)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "agegrps region married race sex insurance health mental_health education employed poverty insurance_v2 agegrps_v2"
foreach j in `subgroup'{
local subgroupn "hh_prov ob_dr op_dr evtype"
foreach k in `subgroupn'{
svy:mean xp, over(`k' `j')
esttab using MEANEVT_cross3.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles(`j'#`k')
}
}
}
exit,clear
