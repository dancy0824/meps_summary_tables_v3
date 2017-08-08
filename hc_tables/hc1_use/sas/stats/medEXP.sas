data MEPS_gt0; set MEPS;
	array vars &vars.;
	do over vars;
		if vars <= 0 then vars = .;
	end;
run;

proc surveymeans data = MEPS_gt0 median nobs nomcar; 
	&format.;
	VAR &vars.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F; 
	&domain.;
run;
