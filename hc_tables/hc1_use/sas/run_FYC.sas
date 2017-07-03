ods graphics off;
ods exclude all; *ods exclude none;
ods listing close;

/*********************  Define Lists  *************************/
%let dir = C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables;
%let shared = &dir\shared\sas;
%let PUFdir = &dir\shared\PUFS;
%let path = &dir\hc1_use\sas;

%include "&shared\run_global.sas";

%let usegrps = sop event event_sop;

%let sop_list   = EXP SLF PTR MCR MCD OTZ;
%let event_list = TOT DVT   RX    OBV    OBD   OBO    OPT    OPY   OPZ    ERT   IPT   HHT    HHA   HHN    OMA;
%let use_list   = ind DVTOT RXTOT OBTOTV OBDRV OBOTHV OPTOTV OPDRV OPOTHV ERTOT IPDIS HHTOTD HHAGD HHINDD OMAEXP; 

%let nsop   = %sysfunc(countw(&sop_list));
%let nevent = %sysfunc(countw(&event_list));
%let nuse   = %sysfunc(countw(&use_list));

proc format; /* Blank format for sop and event */
	value $ sop;
	value $ event;
run;

/*********************  Define Macros  *************************/

/* Reminder: only inline comments are allowed in macros */

%macro survey(grp1,grp2,stat,char1,char2);
	%put &grp1, &grp2, &event, &sop, &stat;

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
	%let sop = EXP;
	%let event = TOT;
	%let countvar = PERWT&yy.F;

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
	%let event = TOT;
	%let grp2 = sop;

	%do i = 1 %to &ngrps;
		%do j = 1 %to &nsop;
			%let grp1 = %scan(&subgrp_list, &i); 
			%let sop = %scan(&sop_list, &j); 
			%let countvar = &event.&sop.&yy.F;
			
			data MEPS; set MEPS; sop = "&sop"; run;
			%survey(grp1=&grp1,grp2=&grp2,stat=&stat,char1=,char2=$);
			%append(stat=&stat,year=&year);
		%end;
	%end;

/* Demographic subgroups x event type */
	%let sop = EXP;
	%let grp2 = event;

	%do i = 1 %to &ngrps;
		%do j = 1 %to &nevent;		
			%let grp1 = %scan(&subgrp_list, &i); 
			%let event = %scan(&event_list, &j);
			%let countvar = %scan(&use_list, &j)&yy.;
			
			data MEPS; set MEPS; event = "&event"; run;
			%survey(grp1=&grp1,grp2=&grp2,stat=&stat,char1=,char2=$);
			%append(stat=&stat,year=&year);
		%end;
	%end;

/* Source of payment x event type */
	%let grp1 = sop;
	%let grp2 = event;

	%do i = 1 %to &nsop;
		%do j = 1 %to &nevent;
			%let sop = %scan(&sop_list, &i); 
			%let event = %scan(&event_list, &j); 
			%let countvar = &event.&sop.&yy.F;
			
			data MEPS; set MEPS; sop = "&sop"; event = "&event"; run;
			%survey(grp1=&grp1,grp2=&grp2,stat=&stat,char1=$,char2=$);
			%append(stat=&stat,year=&year);
		%end;
	%end;

%mend;

%macro run_year(year);
	%set_file_names(&year);

	%let yy = %substr(&year,3,2);
	%include "&shared\load_FYC.sas" / source2;
	%create_subgrps;

	options dlcreatedir;
	libname newdir "&path\tables\&year\";

	/* Population */
	%run_stat(totPOP,year=&year);
	%run_stat(pctEXP,year=&year);

	/* Expenditures */
	%run_stat(totEXP,year=&year);
	%run_stat(meanEXP0,year=&year);
	%run_stat(meanEXP,year=&year);
	%run_stat(medEXP,year=&year);
%mend;

%macro run_sas(year_start,year_end);
	%do year = &year_start %to &year_end;
		%run_year(year=&year);
	%end;
%mend;

/*********************************************************/

%run_sas(1996,2014);

%run_year( (1996, 1997, 1998, 1999 ) );

%let year = 1996;
%run_year(&year);
