data MEPS; set MEPS;
    child_2to17 = (1 < AGELAST & AGELAST < 18);
    child_dental = ((DVTOT&yy. > 0) & (child_2to17=1));
run;

proc format;
	value child_dental
	1 = "One or more dental visits"
	0 = "No dental visits in past year";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT child_dental child_dental. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES child_2to17*&grp.child_dental / row;
run;

