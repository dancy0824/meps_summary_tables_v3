data MEPS_gt0; set MEPS;
	array vars &vars.;
	do over vars;
		vars = (vars > 0);
	end;
run;

proc surveymeans data = MEPS_gt0 mean missing nobs; 
	&format.;
	VAR &vars.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	&domain.;
run;


