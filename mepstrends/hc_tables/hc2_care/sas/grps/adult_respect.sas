data MEPS; set MEPS;
	adult_respect = ADRESP42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_respect freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_respect / row;
run;

proc print data = out;
	where domain = 1 and adult_respect ne . &where.;
	var adult_respect &gp. WgtFreq Frequency RowPercent RowStdErr;
run;


