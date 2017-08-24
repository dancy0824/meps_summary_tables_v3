data MEPS; set MEPS;
	adult_routine = ADRTWW42;
	domain = (ADRTCR42 = 1 & AGELAST >= 18);
run;

&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_routine freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_routine / row;
run;

proc print data = out;
	where domain = 1 and adult_routine ne . &where.;
	var adult_routine &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

