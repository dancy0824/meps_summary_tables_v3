data MEPS; set MEPS;
	adult_rating = ADHECR42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

proc format;
	value adult_rating
      9-10 = "9-10 rating"
      7-8 = "7-8 rating"
      0-6 = "0-6 rating"
      -8 = "Don't know"
      -9 = "Non-response"
      -1 = "Inapplicable";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT adult_rating adult_rating. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_rating / row;
run;

