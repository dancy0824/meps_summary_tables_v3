data MEPS; set MEPS;
	child_routine = CHRTWW42;
	domain = (CHRTCR42 = 1 & AGELAST < 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT child_routine freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_routine / row;
run;

