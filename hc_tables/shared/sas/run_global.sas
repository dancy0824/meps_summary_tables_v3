
%let subgrps = 
		ind agegrps region married race sex /* Demographics */
		insurance health mental_health   	/* Health Variables */
		education employed poverty;  		/* Socio-economic Status */

%let subgrp_list = &subgrps insurance_v2X agegrps_v2X; /* add alternative levels */
%let ngrps  = %sysfunc(countw(&subgrp_list));

/* Initialize macro variables so PROC SQL will work */
%let FYC = ; %let RX = ; %let DV = ;
%let OM = ;  %let IP = ; %let ER = ;
%let OB = ;  %let OP = ; %let HH = ;

/******************************************/

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

%macro create_subgrps;
	%local i grp;
	%include "&shared\grps\agevar.sas" / source2;

	%do i=1 %to %sysfunc(countw(&subgrps));
	   %let grp = %scan(&subgrps, &i);
	   %include "&shared\grps\&grp..sas" / source2;
	%end;

	%do i=1 %to %sysfunc(countw(&usegrps));
	   %let grp = %scan(&usegrps, &i);
	   %include "&path\grps\&grp..sas" / source2;
	%end;
%mend;


%macro standardize(grp1,grp2,char1,char2);
	data sas_results;
		format levels1 &char1.&grp1.. levels2 &char2.&grp2..;
		set sas_results;
		grp1 = "&grp1";
		grp2 = "&grp2";
		levels1 = &grp1;
		levels2 = &grp2;
		has_exp = any_exp;
		if has_exp = . then has_exp = 1;
		keep grp1 grp2 levels1 levels2 Sum Mean Estimate StdDev StdErr has_exp n ;
	run;

	data sas_results;
		set sas_results;
		where has_exp = 1;
	run;
%mend;

%macro append(stat,year);
	data _null_;
		set sas_results;		
		file "&path\tables\&year\&stat..csv" dsd MOD ;
		put (_all_) (+0) ;
	run;
%mend;
