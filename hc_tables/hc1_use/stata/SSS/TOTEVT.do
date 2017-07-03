* Read in consolidated data file Merge-GL and create individual dataset for each year;
forval y=1996/2014{
use Merge-GL if year==`y',clear
save `y'
}

* Create loop over year;
forval i=1996/2014{
use `i',clear

* Declare the data to be complex survey data and identify survey design characteristics;
svyset varpsu [pweight=perwtf], strata(varstr) singleunit(cen)

* Drop missing values in event type;
drop if mi(evntidx)

* Use macro to create a variable list for demographics;
local subgroup "agegrps agegrps_v2 region married race sex insurance insurance_v2 health mental_health education employed poverty"

* Calculate total count and se;
svy:tab ind, count se format(%15.1fc)

* Write to Excel file named by year;
esttab using `i'.csv, b(a9) se(a9) replace wide nostar nonumbers noobs nomtitle plain

* Calculate count and se by demographics;
foreach j in `subgroup'{
svy:tab `j' ind, count se format(%15.1fc)
esttab using `i'.csv, b(a9) se(a9) append wide nostar nonumbers noobs nomtitle plain
}
}
exit,clear

