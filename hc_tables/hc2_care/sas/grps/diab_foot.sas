data MEPS; set MEPS;
	if year > 2007 then do;
      past_year = (DSFT&yy.53=1 | DSFT&ya.53=1);
      more_year = (DSFT&yb.53=1 | DSFB&yb.53=1);
      never_chk = (DSFTNV53 = 1);
      dontknow  = (DSFT&yy.53 = -8);
      non_resp  = (DSFT&yy.53 in (-7,-9));
      inapp     = (DSFT&yy.53 = -1);
    end;

	else do;
      past_year = (DSCKFT53 >= 1);
      not_past_year = (DSCKFT53 = 0);
      dontknow  = (DSCKFT53 = -8);
      non_resp  = (DSCKFT53 in (-7,-9));
      inapp     = (DSCKFT53 = -1);
	end;

	if past_year = 1 then diab_foot = 1;
	else if more_year = 1 then diab_foot = 2;
	else if never_chk = 1 then diab_foot = 3;
	else if not_past_year = 1 then diab_foot = 4;
	else if inapp = 1     then diab_foot = -1;
	else if dontknow = 1  then diab_foot = -8;
	else if non_resp = 1  then diab_foot = -7;
	else diab_chol = -9;
run;

proc format;
	value diab_foot
	 1 = "In the past year"
	 2 = "More than 1 year ago"
	 3 = "Never had feet checked"
	 4 = "No exam in past year"
	-1 = "Inapplicable"
	-7 = "Non-response"
	-8 = "Don't know"
	-9 = "Missing";
run;

proc surveyfreq data = MEPS missing; 
	FORMAT diab_foot diab_foot. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT DIABW&yy.F; 
	TABLES &grp.diab_foot / row;
run;

