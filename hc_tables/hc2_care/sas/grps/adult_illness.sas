data MEPS; set MEPS;
	adult_illness = ADILWW42;
	domain = (ADILCR42=1 & AGELAST >= 18);
run;

proc format;
	value adult_illness
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
	FORMAT adult_illness adult_illness. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_illness / row;
run;

