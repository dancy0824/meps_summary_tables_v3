forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
local subgroup "agegrps agegrps_v2 region married race sex insurance insurance_v2 health mental_health education employed poverty"
foreach j in `subgroup'{
local exclude "`j'"
local subgroupn :list subgroup - exclude
foreach k in `subgroupn'{
svy:tab `k' `j', count se format(%15.1fc)
esttab using TOTEVT_cross1.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles(`j'#`k')
}
local subgroup "`subgroupn'"
}
}
exit,clear