data MEPS; set MEPS;
	if 0 < DSA1C53 & DSA1C53 < 96 then diab_a1c = 1; 
	else diab_a1c = DSA1C53;
	if diab_a1c = 96 then diab_a1c = 0;
run;

proc format;
	value diab_a1c
	 1 = "Had measurement"
	 0 = "Did not have measurement"
	-9 - -7 = "Don't know/Non-response"
	-1 = "Inapplicable";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT diab_a1c diab_a1c. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT DIABW&yy.F; 
	TABLES &grp.diab_a1c / row;
run;

