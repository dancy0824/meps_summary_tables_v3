data MEPS; set MEPS;
	adult_respect = ADRESP42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT adult_respect freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_respect / row;
run;


