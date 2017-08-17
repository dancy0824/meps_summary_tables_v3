data MEPS_use; set MEPS;
	array vars &counts.;
	do over vars;
		vars = (vars > 0);
	end;
run; 

proc surveymeans data = MEPS_use sum missing nobs;
	&format.;	
	VAR &counts.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	&domain.;
run;
