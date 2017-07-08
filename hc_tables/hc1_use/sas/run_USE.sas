/******   RUN: UTILIZATION AND EXPENDITURES  ******/

%let app = hc1_use;
%include "C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables\shared\sas\run_preamble.sas";
%let usegrps = sop event event_sop;

proc format; /* Blank format for sop and event */
	value $ sop;
	value $ event;
	value $ event_v2X;
run;

/********************  Define Macros  *******************/
/* Reminder: only inline comments are allowed in macros */

%macro survey(grp1,grp2,stat,char1=,char2=);
	%let format = FORMAT &grp1. &char1.&grp1.. &grp2. &char2.&grp2.. ;
	%let domain = DOMAIN &grp1.*&grp2.;

	%if &stat = medEXP %then %let ods_table = DomainQuantiles;
	%else %let ods_table = Domain; 

	ods output &ods_table. = sas_results; 
	%include "&path\stats\&stat..sas" ;
%mend;

%macro stdize(grp1,grp2,char1=,char2=,type=FYC);
	%put &grp1, &grp2, &stat;

	data sas_results;
		format levels1 &char1.&grp1.. levels2 &char2.&grp2..;
		set sas_results;

		%if &type = FYC %then %do;
			event = substr(VarName,1,3);
			sop = substr(VarName,4,3);
			evnt = substr(VarName,1,2);

			if evnt = "RX" then do; 
				event = evnt; sop = substr(VarName,3,3);
			end;
		%end;
		%else %if &type = EVNT %then %do;
			sop = substr(VarName,1,2);
		%end;

		grp1 = "&grp1";
		grp2 = "&grp2";
		levels1 = &grp1;
		levels2 = &grp2;
		
		keep grp1 grp2 levels1 levels2 Sum Mean Estimate StdDev StdErr n ;
	run;
%mend;

%macro run_stat(stat,year) / mindelimiter = ',';
	%local i j grp1 grp2; 
	%let yy = %substr(&year,3,2);
	
	%if &stat in totEVT,meanEVT %then %let type = EVNT;
	%else %let type = FYC;

/* Initialize results csv with ind*ind */

	%let counts = count;
	%let vars = TOTEXP&yy.;
	%let uses = XP&yy.X;
	%let gt = >= ;

	%survey(grp1=ind,grp2=ind,stat=&stat);
	%stdize(grp1=ind,grp2=ind,type=&type);

	proc export data=sas_results 
		outfile="&path\tables\&year\&stat..csv" 
		dbms=csv replace;
	run;

/* Demographic subgroups, crossed */

	%do i=1 %to &ngrps;
		%let grp1 = %scan(&subgrps, &i); 

		%do j = &i+1 %to &ngrps;
			%let grp2 = %scan(&subgrps, &j);
			%survey(grp1=&grp1,grp2=&grp2,stat=&stat);
			%stdize(grp1=&grp1,grp2=&grp2,type=&type);
			%append(stat=&stat,year=&year);
		%end;
	%end;

/* Demographic subgroups x source of payment */

	%do i=1 %to &ngrps;
		%let grp1 = %scan(&subgrps, &i); 

		%let gt = > ;
		%let uses   = XP&yy.X SF&yy.X PR&yy.X MR&yy.X MD&yy.X OZ&yy.X;
		
		%let counts = TOTEXP&yy. TOTSLF&yy. TOTPTR&yy. TOTMCR&yy. TOTMCD&yy. TOTOTZ&yy.;
		%let vars = &counts;

		%survey(grp1=&grp1, grp2=ind, stat=&stat);
		%stdize(grp1=&grp1, grp2=sop, char2=$,type=&type);
		%append(stat=&stat,year=&year);

	%end;



/* Demographic subgroups x event type */

	%do i=1 %to &ngrps;
		%let grp1 = %scan(&subgrps, &i); 

		%if &type=EVNT %then %do;
			%let gt = >= ;
			%let uses = XP&yy.X;

			%survey(grp1=&grp1,grp2=event,stat=&stat,char1=,char2=$);
			%stdize(grp1=&grp1,grp2=event,char2=$,type=&type);
			%append(stat=&stat,year=&year);

			%survey(grp1=&grp1,grp2=event_v2X,stat=&stat,char1=,char2=$);
			%stdize(grp1=&grp1,grp2=event,char2=$,type=&type);
			%append(stat=&stat,year=&year);
		%end;

		%else %do;	
			%let counts = TOTUSE&yy. DVTOT&yy. RXTOT&yy. OBTOTV&yy. OBDRV&yy. OBOTHV&yy. 
						  OPTOTV&yy. OPDRV&yy. OPOTHV&yy. ERTOT&yy.  IPDIS&yy. OMAEXP&yy.
						  HHTOTD&yy. HHAGD&yy. HHINDD&yy. ; 

			%let vars = TOTEXP&yy. DVTEXP&yy. RXEXP&yy.  OBVEXP&yy. OBDEXP&yy. OBOEXP&yy. 
						OPTEXP&yy. OPYEXP&yy. OPZEXP&yy. ERTEXP&yy. IPTEXP&yy. OMAEXP&yy.
					    HHTEXP&yy. HHAEXP&yy. HHNEXP&yy. ;

			%survey(grp1=&grp1,grp2=ind,stat=&stat);
			%stdize(grp1=&grp1,grp2=event,char2=$,type=&type);
			%append(stat=&stat,year=&year);
		%end;

	%end; 

/* Source of payment x event type */

	%if &type=EVNT %then %do;	
		%let gt = > ;
		%let uses = XP&yy.X SF&yy.X PR&yy.X MR&yy.X MD&yy.X OZ&yy.X;
	
		%survey(grp1=ind,grp2=event,stat=&stat,char2=$);
		%stdize(grp1=sop,grp2=event,char1=$,char2=$,type=&type);
		%append(stat=&stat,year=&year);

		%survey(grp1=ind,grp2=event_v2X,stat=&stat,char2=$);
		%stdize(grp1=sop,grp2=event_v2X,char1=$,char2=$,type=&type);
		%append(stat=&stat,year=&year);
	%end;

	%else %do;

		%let counts = 
				TOTEXP&yy. TOTSLF&yy. TOTPTR&yy. TOTMCR&yy. TOTMCD&yy. TOTOTZ&yy.
				DVTEXP&yy. DVTSLF&yy. DVTPTR&yy. DVTMCR&yy. DVTMCD&yy. DVTOTZ&yy.
				RXEXP&yy.  RXSLF&yy.  RXPTR&yy.  RXMCR&yy.  RXMCD&yy.  RXOTZ&yy.
				OBVEXP&yy. OBVSLF&yy. OBVPTR&yy. OBVMCR&yy. OBVMCD&yy. OBVOTZ&yy.
				OBDEXP&yy. OBDSLF&yy. OBDPTR&yy. OBDMCR&yy. OBDMCD&yy. OBDOTZ&yy.
				OBOEXP&yy. OBOSLF&yy. OBOPTR&yy. OBOMCR&yy. OBOMCD&yy. OBOOTZ&yy.
				OPTEXP&yy. OPTSLF&yy. OPTPTR&yy. OPTMCR&yy. OPTMCD&yy. OPTOTZ&yy.
				OPYEXP&yy. OPYSLF&yy. OPYPTR&yy. OPYMCR&yy. OPYMCD&yy. OPYOTZ&yy.
				OPZEXP&yy. OPZSLF&yy. OPZPTR&yy. OPZMCR&yy. OPZMCD&yy. OPZOTZ&yy.
				ERTEXP&yy. ERTSLF&yy. ERTPTR&yy. ERTMCR&yy. ERTMCD&yy. ERTOTZ&yy.
				IPTEXP&yy. IPTSLF&yy. IPTPTR&yy. IPTMCR&yy. IPTMCD&yy. IPTOTZ&yy.
				HHTEXP&yy. HHTSLF&yy. HHTPTR&yy. HHTMCR&yy. HHTMCD&yy. HHTOTZ&yy.
				HHAEXP&yy. HHASLF&yy. HHAPTR&yy. HHAMCR&yy. HHAMCD&yy. HHAOTZ&yy.
				HHNEXP&yy. HHNSLF&yy. HHNPTR&yy. HHNMCR&yy. HHNMCD&yy. HHNOTZ&yy.
				OMAEXP&yy. OMASLF&yy. OMAPTR&yy. OMAMCR&yy. OMAMCD&yy. OMAOTZ&yy.;

		%let vars = &counts;
		
		%survey(grp1=ind,grp2=ind,stat=&stat);
		%stdize(grp1=sop,grp2=event,char1=$,char2=$,type=&type);
		%append(stat=&stat,year=&year);

	%end; 

%mend;

proc print data = sas_results;
run;

%macro run_year(year);
	%set_file_names(&year);

	%let yy = %substr(&year,3,2);
	%include "&shared\load\load_FYC.sas" / source2;
	%create_subgrps(&subgrp_load,&shared\subgrps);
	%create_subgrps(&usegrps,&path\grps);

	%include "&shared\load\load_events.sas" / source2;

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

	/* Utilization */
	%run_stat(totEVT,year=&year); 
	%run_stat(meanEVT,year=&year);

%mend;

%macro run_sas(year_start,year_end);
	%do year = &year_end %to &year_start %by -1;
		%run_year(year=&year);
	%end;
%mend;

/*********************************************************/

%run_sas(1996,2014);



/*


%let year = 2014;

%let year = 2002;
%let stat = totPOP;

%let stat = meanEVT;

%let stat = totEVT;

%let i = 1;
%let j = 1;

%let char1 = ;
%let char2 = ;

%put &subgrps;

proc print data = sas_results;
run;

*/
