data MEPS; set MEPS;
	child_routine = CHRTWW42;
	domain = (CHRTCR42 = 1 & AGELAST < 18);
run;

proc format;
	value child_routine
		 4 = "Always"
		 3 = "Usually"
		 2 = "Sometimes/Never"
		 1 = "Sometimes/Never"
		-8 = "Don't know"
		-7 = "Non-response"
		-9 = "Non-response"
		-1 = "Inapplicable";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT child_routine child_routine. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_routine / row;
run;

