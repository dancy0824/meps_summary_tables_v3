ods graphics off;
ods exclude all; *ods exclude none;
ods listing close;

/*********************  Define Lists  *************************/
%let dir = C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables;
%let path = &dir\hc1_use\sas;
%let PUFdir = &dir\shared\PUFS;
%let shared = &dir\shared\sas;

%include "&shared\run_global.sas";

%let sop_list = XP SF PR MR MD OZ;

 %let subgrp_list = ind region;

%let ngrps  = %sysfunc(countw(&subgrp_list));
%let nsop   = %sysfunc(countw(&sop_list));

proc format; /* Blank format for sop and event */
	value $ sop;
	value $ event;
	value $ event_v2X;
run;

/*********************  Define Macros  *************************/

/* Reminder: only inline comments are allowed in macros */

%macro survey(grp1,grp2,stat,char1,char2);
	%put &grp1, &grp2, &sop, &stat;

	%let format = &grp1. &char1.&grp1..  &grp2. &char2.&grp2.. ;
	%let domain = &grp1.*&grp2.;

	%if &stat = medEXP %then %let ods_table = DomainQuantiles;
	%else %let ods_table = Domain; 

	ods output &ods_table. = sas_results; 
	%include "&path\stats\&stat..sas" ;
	%standardize(grp1=&grp1, grp2=&grp2, char1=&char1,char2=&char2);
%mend;

%macro run_stat(stat,year);
	%local i j grp1 grp2; 

	%let yy = %substr(&year,3,2);
	%let sop = XP;
	%let countvar = (XP&yy.X >= 0);

/* Initialize results csv with ind*ind */
	%survey(grp1=ind,grp2=ind,stat=&stat,char1=,char2=);

	proc export data=sas_results 
		outfile="&path\tables\&year\&stat..csv" 
		dbms=csv replace;
	run;

/* Demographic subgroups, crossed */
	%do i=1 %to &ngrps;
		%do j = &i+1 %to &ngrps;
			%let grp1 = %scan(&subgrp_list, &i); 
			%let grp2 = %scan(&subgrp_list, &j);
			%survey(grp1=&grp1,grp2=&grp2,stat=&stat,char1=,char2=);
			%append(stat=&stat,year=&year);
		%end;
	%end;

/* Demographic subgroups x source of payment */

	%do i = 1 %to &ngrps;
		%do j = 1 %to &nsop;
			%let grp1 = %scan(&subgrp_list, &i); 
			%let sop = %scan(&sop_list, &j); 
			%let countvar = (&sop.&yy.X > 0);
			
			data EVENTS; set EVENTS; sop = "&sop"; run;
			%survey(grp1=&grp1,grp2=sop,stat=&stat,char1=,char2=$);

			%append(stat=&stat,year=&year);
		%end;
	%end;

/* Demographic subgroups x event type */
	%let sop = XP;
	%let countvar = (XP&yy.X >= 0);

	%do i = 1 %to &ngrps;
		%let grp1 = %scan(&subgrp_list, &i); 
	
		%survey(grp1=&grp1,grp2=event,stat=&stat,char1=,char2=$);
		%append(stat=&stat,year=&year);

		%survey(grp1=&grp1,grp2=event_v2X,stat=&stat,char1=,char2=$);
		%append(stat=&stat,year=&year);
	%end;

/* Source of payment x event type */
	%do i = 1 %to &nsop;
		%let sop = %scan(&sop_list, &i); 
		%let countvar = (&sop.&yy.X > 0);
			
		%survey(grp1=sop,grp2=event,stat=&stat,char1=$,char2=$);
		%append(stat=&stat,year=&year);

		%survey(grp1=sop,grp2=event_v2X,stat=&stat,char1=$,char2=$);
		%append(stat=&stat,year=&year);
	%end;

%mend;

%macro run_year(year,file);
	%set_file_names(&year);

	%let yy = %substr(&year,3,2);
	%include "&shared\load_fyc.sas" / source2;
	%create_subgrps;

	%include "&shared\load_events.sas" / source2;

	options dlcreatedir;
	libname newdir "&path\tables\&year\";

	/* Utilization */
	%run_stat(totEVT,year=&year);
	%run_stat(meanEVT,year=&year);
%mend;

%macro run_sas(year_start,year_end);
	%do year = &year_start %to &year_end;
		%run_year(year=&year);
	%end;
%mend;

/*********************************************************/

%run_sas(1996,2014);

