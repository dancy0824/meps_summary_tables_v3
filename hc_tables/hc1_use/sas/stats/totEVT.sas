* totEVT ;

data EVENTS; set EVENTS;
	count = &countvar.;
	count_event = (&sop.&yy.X >= 0);
run; 

proc surveymeans data = EVENTS sum missing nobs;
	FORMAT &format.;	
	VAR count;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	DOMAIN &domain.*count_event;
run;
