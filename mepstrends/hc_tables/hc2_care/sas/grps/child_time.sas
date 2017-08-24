data MEPS; set MEPS;
	child_time = CHPRTM42;
	domain = (CHAPPT42 >= 1 & AGELAST < 18);
run;

&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT child_time freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_time / row;
run;


