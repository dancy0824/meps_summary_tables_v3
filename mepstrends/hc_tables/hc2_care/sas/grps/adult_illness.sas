data MEPS; set MEPS;
	adult_illness = ADILWW42;
	domain = (ADILCR42=1 & AGELAST >= 18);
run;

&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_illness freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_illness / row;
run;

proc print data = out;
	where domain = 1 and adult_illness ne . &where.;
	var adult_illness &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

