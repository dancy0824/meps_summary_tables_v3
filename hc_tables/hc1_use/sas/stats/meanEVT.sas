* meanEVT ;

data EVENTS; set EVENTS;
	count_event = (&sop.&yy.X >= 0);
run; 

proc surveymeans data = EVENTS mean missing nobs;
	FORMAT &format.;	
	VAR &sop.&yy.X;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	DOMAIN &domain.*count_event;
run;
