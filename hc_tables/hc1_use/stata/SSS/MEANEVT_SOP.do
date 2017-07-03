forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
svy:mean sop1exp
esttab using MEANEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain title(`i') mtitles("Out of pocket")
svy:mean sop2exp
esttab using MEANEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Private")
svy:mean sop3exp
esttab using MEANEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Medicare")
svy:mean sop4exp
esttab using MEANEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Medicaid")
svy:mean sop5exp
esttab using MEANEVT_SOP.csv, b(a9) se(a9) append wide nostar nonumbers noobs plain mtitles("Other")
}
exit,clear
