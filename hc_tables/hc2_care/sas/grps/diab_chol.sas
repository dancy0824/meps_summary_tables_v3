data MEPS; set MEPS;
	if year > 2007 then do;
		past_year = (DSCH&yy.53=1 or DSCH&ya.53=1);
		more_year = (DSCH&yb.53=1 or DSCB&yb.53=1);
		never_chk = (DSCHNV53 = 1);
		dontknow  = (DSCH&yy.53 = -8);
		non_resp  = (DSCH&yy.53 in (-7,-9));
	end;	

	else do;
		past_year = (CHOLCK53 = 1);
		more_year = (1 < CHOLCK53 and CHOLCK53 < 6);
		never_chk = (CHOLCK53 = 6);
		dontknow  = (CHOLCK53 = -8);
		non_resp  = (CHOLCK53 in (-7,-9));
	end;

	if past_year = 1 then diab_chol = 1;
	else if more_year = 1 then diab_chol = 2;
	else if never_chk = 1 then diab_chol = 3;
	else if dontknow = 1  then diab_chol = 4;
	else if non_resp = 1  then diab_chol = 5;
	else diab_chol = 6;
run;

proc format;
	value diab_chol
		1 = "In the past year"
		2 = "More than 1 year ago"
		3 = "Never had cholesterol checked"
		4 = "Don't know"
	    5 = "Non-response"
		6 = "Missing";
run;
  

