proc surveyfreq data = MEPS missing; 
	&format.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT DIABW&yy.F; 
	TABLES &tbl / row;
run;
