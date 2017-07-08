data MEPS; set MEPS;
	agegrps = AGELAST;
	agegrps_v2X = AGELAST;
run;

proc format;
	value agegrps
		low-4 = "Under 5"
		5-17  = "5-17"
		18-44 = "18-44"
		45-64 = "45-64"
		65-high = "65+";
		
	value agegrps_v2X
		low-17  = "Under 18"
		18-64   = "18-64"
		65-high = "65+";
run;
