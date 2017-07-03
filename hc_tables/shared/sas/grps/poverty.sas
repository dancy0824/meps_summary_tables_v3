data MEPS; set MEPS;
	if year = 1996 then POVCAT96 = POVCAT;
	poverty = POVCAT&yy.;
run;

proc format;
	value poverty
		1 = "Negative or Poor"
		2 = "Near-poor"
		3 = "Low income"
		4 = "Middle income"
		5 = "High income";
run;
