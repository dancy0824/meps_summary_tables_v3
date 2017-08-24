data MEPS; set MEPS;
	child_rating = CHHECR42;
	domain = (CHAPPT42 >= 1 & AGELAST < 18);
run;

proc format;
	value child_rating
	9-10 = "9-10 rating"
	7-8 = "7-8 rating"
	0-6 = "0-6 rating"
	 -8 = "Don't know"
	 -9 = "Non-response"
	 -1 = "Inapplicable";
run;


proc surveyfreq data = MEPS missing; 
	FORMAT child_rating child_rating. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.child_rating / row;
run;

