proc surveymeans data = MEPS mean missing nobs; 
	FORMAT &format.;
	VAR &event.&sop.&yy.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	DOMAIN &domain.;
run;
