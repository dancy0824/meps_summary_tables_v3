/* Source of payment */

data MEPS; set MEPS;
	if year <= 1999 then do;
		TOTTRI&yy. = TOTCHM&yy.;
	end;	

	TOTOTH&yy. = TOTOFD&yy. + TOTSTL&yy. + TOTOPR&yy. + TOTOPU&yy. + TOTOSR&yy.;
  	TOTOTZ&yy. = TOTOTH&yy. + TOTWCP&yy. + TOTVA&yy.;
  	TOTPTR&yy. = TOTPRV&yy. + TOTTRI&yy.;
run;
