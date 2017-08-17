data EVENTS_ge0; set EVENTS;
	array vars &uses.;
	do over vars;
		vars = (vars &gt. 0);
	end;
run; 

proc surveymeans data = EVENTS_ge0 sum missing nobs;
	&format.;	
	VAR &uses.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	&domain.;
run;
