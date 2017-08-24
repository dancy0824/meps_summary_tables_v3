data MEPS; set MEPS;
	adult_listen = ADLIST42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_listen freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_listen / row;
run;

proc print data = out;
	where domain = 1 and adult_listen ne . &where.;
	var adult_listen &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

