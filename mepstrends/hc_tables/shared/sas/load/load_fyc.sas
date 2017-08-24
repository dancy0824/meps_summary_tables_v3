ods graphics off;

** Read in dataset and initialize year **;

FILENAME &FYC. "&PUFdir.\&FYC..ssp";
proc xcopy in = &FYC. out = WORK IMPORT;
run;

data MEPS;
	SET &FYC.;
	year = &year.;	
	ind = 1;
	count = 1;

	if year <= 2001 then do;
		VARPSU = VARPSU&yy.;
		VARSTR = VARSTR&yy.;
	end;

	if year <= 1998 then do;
		PERWT&yy.F = WTDPER&yy.;
	end;
run;
