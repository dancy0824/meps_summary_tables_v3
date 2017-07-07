data MEPS; set MEPS;
	count = (&event.&sop.&yy. > 0 & PERWT&yy.F > 0);
run; 

proc surveymeans data = MEPS sum missing ; 
	FORMAT &format.;
	VAR count;
	DOMAIN &domain.;
run;
