data MEPS; set MEPS;
	if year > 2007 then do;
		past_year = (DSFL&yy.53=1 | DSFL&ya.53=1);
		more_year = (DSFL&yb.53=1 | DSVB&yb.53=1);
		never_chk = (DSFLNV53 = 1);
		non_resp  = (DSFL&yy.53 in (-7,-8,-9));
	end;
	
	else do;
		past_year = (FLUSHT53 = 1);
		more_year = (1 < FLUSHT53 & FLUSHT53 < 6);
		never_chk = (FLUSHT53 = 6);
		non_resp  = (FLUSHT53 in (-7,-8,-9));
	end;

	if past_year = 1 then diab_flu = 1;
	else if more_year = 1 then diab_flu = 2;
	else if never_chk = 1 then diab_flu = 3;
	else if non_resp = 1  then diab_flu = -7;
	else diab_flu = -9;
 
run;

   
proc format;
	value diab_flu
	 1 = "In the past year"
	 2 = "More than 1 year ago"
	 3 = "Never had eye exam"
	 4 = "No exam in past year"
	-1 = "Inapplicable"
	-7 = "Don't know/Non-response"
	-9 = "Missing";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT diab_flu diab_flu. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT DIABW&yy.F; 
	TABLES &grp.diab_flu / row;
run;


