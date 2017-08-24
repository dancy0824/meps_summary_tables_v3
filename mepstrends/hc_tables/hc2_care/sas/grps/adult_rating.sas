data MEPS; set MEPS;
	adult_rating = ADHECR42;
	domain = (ADAPPT42 >= 1 & AGELAST >= 18);
run;

proc format;
	value adult_rating
	9-10 = "9-10 rating"
	7-8 = "7-8 rating"
	0-6 = "0-6 rating"
	-9 - -7 = "Don't know/Non-response"
	-1 = "Inapplicable";
run;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_rating adult_rating. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_rating / row;
run;

proc print data = out;
	where domain = 1 and adult_rating ne . &where.;
	var adult_rating &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

