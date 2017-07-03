forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
svy:total ind, over(sop1)
esttab using TOTEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles("Out of pocket")
svy:total ind, over(sop2)
esttab using TOTEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Private")
svy:total ind, over(sop3)
esttab using TOTEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Medicare")
svy:total ind, over(sop4)
esttab using TOTEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Medicaid")
svy:total ind, over(sop5)
esttab using TOTEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Other")
}
exit,clear
