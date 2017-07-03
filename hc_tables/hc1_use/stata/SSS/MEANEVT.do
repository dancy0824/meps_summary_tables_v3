forval i=1996/2014{
use `i',clear
drop if mi(evntidx)
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)
svy:mean xp
esttab using meanevt`i'.csv, b(a9) se(a9) replace wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(agegrps)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(agegrps_v2)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(region)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(married)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(race)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(sex)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(insurance)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(insurance_v2)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(health)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(mental_health)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(education)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(employed)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
svy:mean xp,over(poverty)
esttab using meanevt`i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
}
exit,clear
