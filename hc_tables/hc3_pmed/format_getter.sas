libname pufs "S:\CFACT\Shared\PUF SAS files\SAS V8";

data rx;
	set pufs.h160a;
run;

proc print data = rx(obs=10);

	var Tc1;
run;
