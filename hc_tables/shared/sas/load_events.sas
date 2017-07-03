
%let file = h160;
%let year = 2013;
%let yy = 13;


%macro load_events(evnt,file) / minoperator;

	FILENAME &file. "&PUFdir.\&file..ssp";
	proc xcopy in = &file. out = WORK IMPORT;
	run;

	data &evnt;
		SET &file.;
		event = "&evnt.";
		year = &year.;

		%if &evnt in (IP OP ER) %then %do;
			MD&yy.X=&evnt.DMD&yy.X+&evnt.FMD&yy.X;
			PV&yy.X=&evnt.DPV&yy.X+&evnt.FPV&yy.X;
			VA&yy.X=&evnt.DVA&yy.X+&evnt.FVA&yy.X;
			OF&yy.X=&evnt.DOF&yy.X+&evnt.FOF&yy.X;
			SL&yy.X=&evnt.DSL&yy.X+&evnt.FSL&yy.X;
			WC&yy.X=&evnt.DWC&yy.X+&evnt.FWC&yy.X;
			OR&yy.X=&evnt.DOR&yy.X+&evnt.FOR&yy.X;
			OU&yy.X=&evnt.DOU&yy.X+&evnt.FOU&yy.X;
			OT&yy.X=&evnt.DOT&yy.X+&evnt.FOT&yy.X;
			XP&yy.X=&evnt.DXP&yy.X+&evnt.FXP&yy.X;
			
			if year <= 1999 then TR&yy.X=&evnt.DTR&yy.X+&evnt.FTR&yy.X;
			else TR&yy.X=&evnt.DCH&yy.X+&evnt.FCH&yy.X;
		%end;

		%else %do;
			MD&yy.X=&evnt.MD&yy.X;
			PV&yy.X=&evnt.PV&yy.X;
			VA&yy.X=&evnt.VA&yy.X;			
			OF&yy.X=&evnt.OF&yy.X;
			SL&yy.X=&evnt.SL&yy.X;
			WC&yy.X=&evnt.WC&yy.X;
			OR&yy.X=&evnt.OR&yy.X;
			OU&yy.X=&evnt.OU&yy.X;
			OT&yy.X=&evnt.OT&yy.X;
			XP&yy.X=&evnt.XP&yy.X;

			if year <= 1999 then TR&yy.X=&evnt.TR&yy.X;
			else TR&yy.X=&evnt.CH&yy.X;
		%end;
	run;
%mend;

%load_events(RX,&RX.);
%load_events(DV,&DV.);
%load_events(OM,&OM.);
%load_events(IP,&IP.);
%load_events(ER,&ER.);
%load_events(OP,&OP.);
%load_events(OB,&OB.);
%load_events(HH,&HH.);

/* Define sub-levels for office-based, outpatient, and home health */
data HH; set HH;
	if year = 1996 then MPCELIG = SELFAGEN;
	if MPCELIG = 1 then event_v2X = 'HHA';
	else if MPCELIG = 2 then event_v2X = 'HHN';
	else event_v2X = 'Missing';
run;

data OB; set OB;
	if SEEDOC = 1 then event_v2X = 'OBD';
	else if SEEDOC = 2 then event_v2X = 'OBO';
	else event_v2X = 'Missing';
run;

data OP; set OP;
	if SEEDOC = 1 then event_v2X = 'OPY';
	else if SEEDOC = 2 then event_v2X = 'OPZ';
	else event_v2X = 'Missing';
run;

/* Stack events into single dataset */
data stacked_events;
	set RX DN OM IP ER OP OB HH;
	PR&yy.X = PV&yy.X+TR&yy.X;
    OZ&yy.X = OF&yy.X+SL&yy.X+OT&yy.X+OR&yy.X+OU&yy.X+WC&yy.X+VA&yy.X;
	keep DUPERSID event:  XP&yy.X SF&yy.X PR&yy.X MR&yy.X MD&yy.X OZ&yy.X;
run;

/* Merge EVENTS onto FYC file */

data FYCsub; set FYC;
	keep &subgrps. DUPERSID PERWT&yy.F VARSTR VARPSU;
run;

proc sort data = stacked_events; by DUPERSID; run;
proc sort data = FYCsub; by DUPERSID; run;

data EVENTS;
	merge stacked_events FYCsub;
	by DUPERSID;
run;
