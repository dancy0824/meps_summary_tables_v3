data MEPS; set MEPS;
	any_exp = (&event.&sop.&yy. > 0);
run;

proc surveymeans data = MEPS median missing nobs; 
	FORMAT &format.;
	VAR &event.&sop.&yy.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	DOMAIN any_exp*&domain.;
run;
