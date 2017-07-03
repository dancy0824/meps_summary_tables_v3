forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
encode evntyp,gen(evtype)
svy:total ind, over(evtype)
esttab using TOTEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:total ind, over( ob_dr )
esttab using TOTEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:total ind, over( op_dr )
esttab using TOTEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:total ind, over( hh_prov )
esttab using TOTEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
}
exit,clear

