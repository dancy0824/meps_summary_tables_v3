forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "agegrps region married race sex insurance health mental_health education employed poverty insurance_v2 agegrps_v2"
foreach j in `subgroup'{
local subgroupn "sop1exp sop2exp sop3exp sop4exp sop5exp"
foreach k in `subgroupn'{
svy:mean `k', over(`j')
esttab using MEANEVT_cross2.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles(`j'#`k')
}
}
}
exit,clear
