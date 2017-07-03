forval i=2014(-1)1996{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
encode evntyp,gen(evtype)
svy:mean xp, over(evtype)
esttab using MEANEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp, over( ob_dr )
esttab using MEANEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp, over( op_dr )
esttab using MEANEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp, over( hh_prov )
esttab using MEANEVT_EV.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
}
exit,clear