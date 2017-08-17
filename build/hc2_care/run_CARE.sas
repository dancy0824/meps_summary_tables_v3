/******   RUN: ACCESSIBILITY AND QUALITY OF CARE  ******/

%let app = hc2_care;
%include "..\..\shared\sas\run_preamble.sas";

%let caregrps = usc 
	difficulty rsn_MD rsn_DN rsn_PM 
	adult_nosmok child_dental 
	diab_a1c diab_chol diab_eye diab_foot diab_flu
	adult_routine adult_illness 
	adult_listen adult_explain 
	adult_respect adult_time adult_rating 
	child_routine child_illness 
	child_listen child_explain 
	child_respect child_time child_rating;

%let caregrps = diab_chol;
%let nstat =  %sysfunc(countw(&caregrps));

/********************  Define Macros  *******************/
/* Reminder: only inline comments are allowed in macros */

%macro survey(grp1,stat,stat_grp);
	%let format = FORMAT &grp1. &grp1.. &stat. &stat.. ;
	%let tbl = &grp1.*&stat;

	ods output CrossTabs = sas_results; 
	%include "&path\stats\&stat_grp..sas" ;
%mend;

%macro stdize(grp1,grp2);
	%put &grp1, &grp2, &stat;
	data sas_results;
		format levels1 &grp1.. levels2 &stat..;
		set sas_results;

		grp1 = "&grp1";
		grp2 = "&grp2";
		levels1 = &grp1;
		levels2 = &grp2;
		
		keep grp1 grp2 levels1 levels2 RowPercent RowStdErr Frequency;
	run;
%mend;

%macro run_stat(stat,stat_grp,year) / mindelimiter = ',';
	%local i j grp1 stat; 
	%let yy = %substr(&year,3,2);

/*
	 %let i = 1; 
	%let stat = diab_chol;
	%let stat_grp = diab;

	Add initial here to overwrite table;
*/
	%do i = 1 %to &ngrps;
		%let grp1 = %scan(&subgrp_list, &i); 
		%survey(grp1=&grp1,stat=&stat,stat_grp=&stat_grp);
		%stdize(grp1=&grp1,grp2=&stat);
		%append(stat=&stat,year=&year);
	%end;

%mend;


%macro run_year(year);
	%set_file_names(&year);

	%let yy = %substr(&year,3,2);
	%let yb = %eval(&yy - 1);
	%let ya = %eval(&yy + 1);

	%include "&shared\load\load_FYC.sas" / source2;
	%create_subgrps(&subgrp_load,&shared\subgrps);
	%create_subgrps(&caregrps,&path\grps);

	options dlcreatedir;
	libname newdir "&path\tables\&year\";


	%run_stat(stat=diab_chol,stat_grp=diab,year=&year);

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
%let subgrps = ind agegrps;
*/
