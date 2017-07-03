forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
encode evntyp,gen(evtype)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "sop1 sop2 sop3 sop4 sop5"
foreach j in `subgroup'{
local subgroupn "hh_prov ob_dr op_dr evtype"
foreach k in `subgroupn'{
svy:total `j', over(`k')
esttab using TOTEVT_cross4.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles(`j'#`k')
}
}
}
exit,clear

