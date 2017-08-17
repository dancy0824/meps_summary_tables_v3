data MEPS; set MEPS;
	if year <= 2001 then usc = LOCATI42;
	else usc = LOCATN42;
	if HAVEUS42 = 2 then usc = 0;

	domain = (ACCELI42 = 1 & HAVEUS42 >= 0 & LOCATN42 >= -1);
run;

proc format;
	value usc
     0 = "No usual source of health care"
     1 = "Office-based"
     2 = "Hospital"
     3 = "Emergency room"
	-1 = "Inapplicable"
	-8 = "Don't know";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT usc usc. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.usc / row;
run;

