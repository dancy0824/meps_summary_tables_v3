data MEPS; set MEPS;
	if year > 2001 then do;
		past_year = (DSEY&yy.53=1 | DSEY&ya.53=1);
		more_year = (DSEY&yb.53=1 | DSEB&yb.53=1);
		never_chk = (DSEYNV53 = 1);
		dontknow  = (DSEY&yy.53 = -8);
		non_resp = (DSEY&yy.53= -9);
	end;

	else do;
		past_year = (DSEYE53 in (1,2));
		more_year = (DSEYE53 in (3,4));
		never_chk = (DSEYE53 = 5);
		dontknow  = (DSEYE53 = -8);
		non_resp  = (DSEYE53 in (-7,-9));
	end;

	if past_year = 1 then diab_eye = 1;
	else if more_year = 1 then diab_eye = 2;
	else if never_chk = 1 then diab_eye = 3;
	else if dontknow = 1  then diab_eye = -8;
	else if non_resp = 1  then diab_eye = -7;
	else diab_eye = -9;
run;

proc format;
	value diab_eye
		 1 = "In the past year"
		 2 = "More than 1 year ago"
		 3 = "Never had eye exam"
		 4 = "No exam in past year"
		-1 = "Inapplicable"
		-7 = "Non-response"
		-8 = "Don't know"
	    -9 = "Missing";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT diab_eye diab_eye. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT DIABW&yy.F; 
	TABLES &grp.diab_eye / row;
run;

