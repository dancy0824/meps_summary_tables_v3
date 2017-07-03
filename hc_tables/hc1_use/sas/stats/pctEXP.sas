* pctEXP;
data MEPS; set MEPS;
	any_exp = (&event.&sop.&yy. > 0);
run;

proc surveymeans data = MEPS mean missing nobs; 
	FORMAT &format.;
	VAR any_exp;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	DOMAIN &domain.;
run;


