/* Event type and source of payment */

%macro add_sops(event);
	data MEPS; set MEPS;
		if year <= 1999 then do;
			&event.TRI&yy. = &event.CHM&yy.;
		end;
		
		&event.OTH&yy. = &event.OFD&yy. + &event.STL&yy. + &event.OPR&yy. + &event.OPU&yy. + &event.OSR&yy.;
		&event.OTZ&yy. = &event.OTH&yy. + &event.WCP&yy. + &event.VA&yy.;
		&event.PTR&yy. = &event.PRV&yy. + &event.TRI&yy.;
	run;
%mend;

%macro add_events(sop);
	data MEPS; set MEPS;
		HHT&sop.&yy. = HHA&sop.&yy. + HHN&sop.&yy.; /* Home Health Agency + Independent providers */
		ERT&sop.&yy. = ERF&sop.&yy. + ERD&sop.&yy.; /* Doctor + Facility expenses for OP, ER, IP events */
		IPT&sop.&yy. = IPF&sop.&yy. + IPD&sop.&yy.;
		OPT&sop.&yy. = OPF&sop.&yy. + OPD&sop.&yy.; /* All Outpatient */
		OPY&sop.&yy. = OPV&sop.&yy. + OPS&sop.&yy.; /* Physician only */
		OPZ&sop.&yy. = OPO&sop.&yy. + OPP&sop.&yy.; /* non-physician only */
		OMA&sop.&yy. = VIS&sop.&yy. + OTH&sop.&yy.;
	run;
%mend;

%macro add_event_sops;	
	%let sop_list = EXP SLF PTR MCR MCD OTZ;
	%let event_list = 
			TOT DVT RX  OBV OBD OBO 
			OPF OPD OPV OPS OPO OPP 
			ERF ERD IPF IPD HHA HHN OTH;

	%local i;
	%do i = 1 %to 20;
		%add_sops(event = %scan(&event_list, &i));
	%end;

	%do i = 1 %to 6;
		%add_events(sop = %scan(&sop_list, &i));
	%end;
%mend;

%add_event_sops;
