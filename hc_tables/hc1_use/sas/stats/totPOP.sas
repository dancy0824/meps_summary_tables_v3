* totPOP ;

data MEPS; set MEPS;
	count = (&countvar. > 0);
run; 

proc surveymeans data = MEPS sum missing nobs;
	FORMAT &format.;	
	VAR count;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	DOMAIN &domain.;
run;
