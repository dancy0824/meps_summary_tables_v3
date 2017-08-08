%macro avgEVT(event);

	proc sort data = &event.; by DUPERSID; run;
	proc sort data = FYCsub; by DUPERSID; run;

	data pers_events;
		merge &event. FYCsub;
		by DUPERSID;
		EXP = (XP&yy.X > 0);
		SLF = (SF&yy.X > 0);
		MCR = (MR&yy.X > 0);
		MCD = (MD&yy.X > 0);
		PRV = (PR&yy.X > 0);
		OTZ = (OZ&yy.X > 0);
	run;

	proc means data = pers_events sum noprint;
		by DUPERSID VARSTR VARPSU PERWT&yy.F &subgrps. ;
		var EXP SLF MCR MCD PRV OTZ;
		output out = n_events sum = EXP SLF MCR MCD PRV OTZ;
	run;	

	title "Event = &event";
	proc surveymeans data = n_events mean missing nobs;
		&format.;	
		var EXP SLF MCR MCD PRV OTZ;
		STRATA VARSTR;
		CLUSTER VARPSU;
		WEIGHT PERWT&yy.F;
		&domain.;
	run;
%mend;

data OBD OBO; 
	set OB;
	if event_v2X = 'OBD' then output OBD;
	if event_v2X = 'OBO' then output OBO;
run;

data OPY OPZ; 
	set OP;
	if event_v2X = 'OPY' then output OPY;
	if event_v2X = 'OPZ' then output OPZ;
run;

%avgEVT(RX);
%avgEVT(DV);
%avgEVT(OM);
%avgEVT(IP);
%avgEVT(ER);
%avgEVT(HH);

%avgEVT(OP); 
  %avgEVT(OPY); 
  %avgEVT(OPZ); 

%avgEVT(OB);
  %avgEVT(OBD);
  %avgEVT(OBO);

