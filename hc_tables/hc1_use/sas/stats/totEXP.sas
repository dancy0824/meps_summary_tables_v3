proc surveymeans data = MEPS sum missing nobs; 
	&format.;
	VAR &vars.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	&domain.;
run;
