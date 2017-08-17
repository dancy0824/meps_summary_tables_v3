data stacked_events;
	set RX DV OM IP ER OP OB HH;
run;

proc sort data = stacked_events; by DUPERSID; run;
proc sort data = FYCsub; by DUPERSID; run;

data pers_events;
	merge stacked_events FYCsub;
	by DUPERSID;
	EXP = (XP&yy.X >= 0);
run;

proc means data = pers_events sum noprint;
	by DUPERSID VARSTR VARPSU PERWT&yy.F &subgrps. ;
	var EXP;
	output out = n_events sum = EXP;
run;	
&ods.;
proc surveymeans data = n_events mean missing nobs;
	&format.;	
	VAR EXP;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT PERWT&yy.F;
	&domain.;
run;
