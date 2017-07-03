proc surveymeans data = MEPS sum missing nobs; 
	FORMAT &format.;
	VAR &event.&sop.&yy.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	DOMAIN &domain.;
run;
