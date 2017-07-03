forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
encode evntyp,gen(evtype)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "sop1exp sop2exp sop3exp sop4exp sop5exp"
local subgroupn "hh_prov ob_dr op_dr evtype"
foreach j in `subgroup'{
foreach k in `subgroupn'{
svy:mean `j', over(`k')
esttab using MEANEVT_cross4.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') nomtitle
}
}
}
exit,clear

