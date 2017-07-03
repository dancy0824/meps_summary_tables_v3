/* Mental health */
data MEPS; set MEPS;
	if year=1996 then do;
		MNHLTH53=MNTHLTH2;
		MNHLTH42=MNTHLTH2;
		MNHLTH31=MNTHLTH1;
	end;
	
	if MNHLTH53 ge 0 then mental_health=MNHLTH53;
	else if MNHLTH42 ge 0 then mental_health=MNHLTH42;
	else if MNHLTH31 ge 0 then mental_health=MNHLTH31;
	else mental_health = .;
run;

proc format;
	value mental_health
		1 = "Excellent"
		2 = "Very good"
		3 = "Good"
		4 = "Fair"
		5 = "Poor"
		. = "Missing";
run;
  
