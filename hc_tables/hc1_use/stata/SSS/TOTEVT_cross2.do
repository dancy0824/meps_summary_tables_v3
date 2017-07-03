forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "agegrps region married race sex insurance health mental_health education employed poverty insurance_v2 agegrps_v2"
foreach j in `subgroup'{
local subgroupn "sop1 sop2 sop3 sop4 sop5"
foreach k in `subgroupn'{
svy:total `k', over(`j')
esttab using TOTEVT_cross2.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles(`j'#`k')
}
}
}
exit,clear
