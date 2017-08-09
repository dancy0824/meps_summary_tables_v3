data MEPS; set MEPS;
	adult_routine = ADRTWW42;
	domain = (ADRTCR42 = 1 & AGELAST >= 18);
run;

proc format;
	value adult_routine
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
	FORMAT adult_routine adult_routine. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_routine / row;
run;

