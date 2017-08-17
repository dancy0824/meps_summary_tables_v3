/**************  Set Options  *******************/
ods graphics off; 
ods listing close; 
ods exclude all; *ods exclude none;
options minoperator;

%let dir = C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables;
%let shared = &dir\shared\sas;
%let PUFdir = &dir\shared\PUFS;
%let path = &dir\&app\sas;


%let grps = 
		ind agegrps region married race sex /* Demographics */
		insurance health mental_health   	/* Health Variables */
		education employed poverty;  		/* Socio-economic Status */

%let subgrp_load = agevar &grps;
%let subgrps = &grps insurance_v2X agegrps_v2X; /* add alternative levels */

%let ngrps  = %sysfunc(countw(&subgrps));

/* Initialize macro variables so PROC SQL will work */
%let FYC = ; %let RX = ; %let DV = ;
%let OM = ;  %let IP = ; %let ER = ;
%let OB = ;  %let OP = ; %let HH = ;

/************* MEPS file names *******************/

proc import 
 datafile = "C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables\shared\puf_expanded.csv"
 out = meps_names dbms = CSV replace;
run;

%macro set_file_names(year);
	data pufs;
		set meps_names;
		where year = &year;
		drop Year New_Panel Old_Panel;
	run;

	proc sql noprint;
		select FYC,RX,DV,OM, IP, ER, OP, OB, HH into 
		      :FYC trimmed,:RX trimmed,:DV trimmed,
			  :OM trimmed,:IP trimmed,:ER trimmed,
			  :OP trimmed,:OB trimmed,:HH trimmed
		from pufs;
	quit;

%mend;

%macro create_subgrps(grp_list,fileloc);
	%local i grp;
	%do i=1 %to %sysfunc(countw(&grp_list));
	   %let grp = %scan(&grp_list, &i);
	   %include "&fileloc\&grp..sas" / source2;
	%end;
%mend;


%macro append(stat,year);
	data _null_;
		set sas_results;		
		file "&path\tables\&year\&stat..csv" dsd MOD ;
		put (_all_) (+0) ;
	run;
%mend;
