data EVENTS_ge0; set EVENTS;
	array vars &vars.;
	do over vars;
		if vars < 0 then vars = .;
	end;
run; 

proc surveymeans data = EVENTS_ge0 mean missing nobs;
	&format.;	
	VAR &vars.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	&domain.;
run;
