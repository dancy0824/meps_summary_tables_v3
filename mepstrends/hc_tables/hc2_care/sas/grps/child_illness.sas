data MEPS; set MEPS;
	child_illness = CHILWW42;
	domain = (CHILCR42=1 & AGELAST < 18);
run;
	
&freq_fmt.;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT child_illness freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_illness / row;
run;

proc print data = out;
	where domain = 1 and child_illness ne . &where.;
	var child_illness &gp. WgtFreq Frequency RowPercent RowStdErr;
run;
