data MEPS; set MEPS;
	adult_routine = ADRTWW42;
	domain = (ADRTCR42 = 1 & AGELAST >= 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT adult_routine freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_routine / row;
run;

