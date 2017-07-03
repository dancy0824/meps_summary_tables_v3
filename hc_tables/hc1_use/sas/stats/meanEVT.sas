* meanEVT ;

proc surveymeans data = EVENTS mean missing nobs;
	FORMAT &format.;	
	VAR &sop.&yy.X;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	DOMAIN &domain.;
run;
