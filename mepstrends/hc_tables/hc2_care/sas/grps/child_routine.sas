data MEPS; set MEPS;
	child_routine = CHRTWW42;
	domain = (CHRTCR42 = 1 & AGELAST < 18);
run;

&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT child_routine freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_routine / row;
run;

proc print data = out;
	where domain = 1 and child_routine ne . &where.;
	var child_routine &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

