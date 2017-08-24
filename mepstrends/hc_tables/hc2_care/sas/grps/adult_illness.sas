data MEPS; set MEPS;
	adult_illness = ADILWW42;
	domain = (ADILCR42=1 & AGELAST >= 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT adult_illness freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_illness / row;
run;

