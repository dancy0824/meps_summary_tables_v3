data MEPS; set MEPS;
	adult_explain = ADEXPL42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

proc format;
	value adult_explain
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
	FORMAT adult_explain adult_explain. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_explain / row;
run;


