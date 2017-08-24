data MEPS; set MEPS;
	if year <= 2002 then adult_nosmok = ADDSMK42;
	else adult_nosmok = ADNSMK42;

	domain = (ADSMOK42=1 & CHECK53=1);
run;

proc format;
	value adult_nosmok
	 1 = "Told to quit"
	 2 = "Not told to quit"
	 3 = "Had no visits in the last 12 months"
	-9 = "Not ascertained"
	-1 = "Inapplicable";
run;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing; 
	FORMAT adult_nosmok adult_nosmok. &fmt.;
	STRATA VARSTR;
	CLUSTER VARPSU;
	WEIGHT SAQWT&yy.F; 
	TABLES domain*&grp.adult_nosmok / row;
run;

proc print data = out;
	where domain = 1 and adult_nosmok ne . &where.;
	var adult_nosmok &gp. WgtFreq Frequency RowPercent RowStdErr;
run;

