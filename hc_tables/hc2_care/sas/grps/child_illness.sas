data MEPS; set MEPS;
	child_illness = CHILWW42;
	domain = (CHILCR42=1 & AGELAST < 18);
run;
	
proc format;
	value child_illness
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
	FORMAT child_illness child_illness. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_illness / row;
run;

