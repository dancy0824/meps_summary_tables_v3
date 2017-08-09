data MEPS; set MEPS;
	delay_MD = (MDUNAB42 = 1|MDDLAY42=1);
	delay_DN = (DNUNAB42 = 1|DNDLAY42=1);  
	delay_PM = (PMUNAB42 = 1|PMDLAY42=1);
	delay_ANY = (delay_MD|delay_DN|delay_PM);
	domain = (ACCELI42 = 1);
run;

proc format;
	value delay
	1 = "Difficulty accessing care"
	0 = "No difficulty";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT delay: delay. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	TABLES domain*&grp.(delay_ANY delay_MD delay_DN delay_PM) / row;
run;

