data MEPS; set MEPS;
	child_illness = CHILWW42;
	domain = (CHILCR42=1 & AGELAST < 18);
run;
	
&freq_fmt.;

proc surveyfreq data = MEPS missing; 
	FORMAT child_illness freq. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_illness / row;
run;

