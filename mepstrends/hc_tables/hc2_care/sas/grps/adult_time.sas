data MEPS; set MEPS;
	adult_time = ADPRTM42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT adult_time freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_time / row;
run;

