/* AGELAST */
data MEPS; set MEPS;
  if year=1996 then do;
	  AGE42X=AGE2X;
	  AGE31X=AGE1X;
  end;
  
  if AGE&yr.X >= 0 then AGELAST=AGE&yr.x;
  else if AGE42X >= 0 then AGELAST=AGE42X;
  else if AGE31X >= 0 then AGELAST=AGE31X;
run;

