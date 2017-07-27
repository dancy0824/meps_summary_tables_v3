****************************************************************************
*** Emily Mitchell
*** 05/23/2016
***
*** SAS Code for Stat brief: "Costly conditions, Adults 2013" 
*** 
*** Costly Conditions code to include only Adults 18+, year = 2013;
****************************************************************************;

options cleanup; 
*libname meps "S:\CFACT\Shared\PUF SAS files\SAS V8";
libname meps "C:\Users\emily.mitchell\Desktop\AHRQ\MEPS\PUFs\SAS V8 Local";

*********************************************************;
*** 	Read in and combine 2013 Files:	
***			- Conditions file
***			- Condition-event link file
***			- Event-files
***			- FYC file					
*********************************************************;

**	1. Read in Year-specific event and conditions files
**	2. Combine with conditions files and FYC file (for age variable)
**	3. Sum expenditures across events per person, condition;

%include "C:\Users\emily.mitchell\Desktop\AHRQ\SAS Formats\Conditions_format.sas"; * Read in Zhengyi's formatting code for condition names;

**	1. Reading in Year-specific event and conditions files;

	** Read in Conditions file;
	data cond2013;
		set meps.h162 (keep = DUPERSID CONDN CONDIDX CCCODEX VARSTR VARPSU PERWT13F);  /* conditions file */
	run;

	proc contents data = cond2013;
	run;

	proc print data = cond2013 (obs=10);
	run;

	***********************************************;
	** This is part of code  from Zhengyi email 10/27/2015
	** Make new variables for condition group, based on format;
		DATA cond2013;
		  SET cond2013;
		    CND61CAT= PUT(INPUT(CCCODEX,3.),   CCCFMT.);
			CND_NAME= PUT(INPUT(CND61CAT,$3.), CCCNAME.);
		RUN;

	** End of code from Zhengyi;
	***********************************************;

	** Read in event-condition linking file;
	data link2013;
		set meps.h160IF1 (keep = DUPERSID CONDIDX EVNTIDX EVENTYPE );
	run;

	proc print data = link2013 (obs=5);
	run;


	** Merge conditions with linkage and FYC (to get age variables);
	proc sort data = cond2013; by CONDIDX; run; * Conditions file;
	proc sort data = link2013; by CONDIDX; run; * Conditions-Event Crosswalk;

	data pre_condevent2013;
		merge cond2013 link2013; * merge conditions with linking file;
		by CONDIDX;
		CCC = CCCODEX;
	run;

	** Merge age values onto DUPERSID;
	data FYC2013;
		set meps.h163(keep = DUPERSID VARSTR VARPSU PERWT13F AGELAST);
		IND = 1; *for use later to get total number of people;
	run;

	proc sort data = pre_condevent2013; by DUPERSID; run;
	proc sort data = FYC2013; by DUPERSID; run;

	data condevent2013;
		merge pre_condevent2013(in=aa) FYC2013 ;
		by DUPERSID;
		if aa;
	run;

	proc contents data = pre_condevent2013; run;
	proc contents data = condevent2013; run;

	proc print data = condevent2013 (obs=6);
	run;

** Read in event-specific files;
		data raw_RX2013;
			set meps.h160a (keep = DUPERSID DRUGIDX RXRECIDX LINKIDX RXXP13X VARSTR VARPSU PERWT13F RX:);
			EVNTIDX = LINKIDX; *Since multiple Rx event can be linked to a medical visit (OB, ER, etc.),
								 rename the linking variable to link to the conditions file;
		run;

		* For RX, sum over Event, since 1 event can have multiple prescriptions with expenses;
		proc sort data = raw_RX2013; by DUPERSID EVNTIDX RXCCC: ; run;
		proc means data = raw_RX2013 sum noprint;
			by DUPERSID EVNTIDX RXCCC: ; * RX is a dummy to keep Condition codes in file (should not affect output, since EVNTIDX in there);
			var RXXP13X RXSF13X RXMR13X RXMD13X RXPV13X RXVA13X RXTR13X 
				RXOF13X RXSL13X RXWC13X RXOT13X RXOR13X RXOU13X;
			output out = inRX2013 
					sum =  
					mean(VARSTR VARPSU PERWT13F ) = ;
		run;

		proc print data = inRX2013 (obs=6);
		run;

	data inOM2013; set meps.h160c (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F OM: ); run; 
	data inIP2013; set meps.h160d (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F IP: ); run;
	data inER2013; set meps.h160e (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F ER: ); run;
	data inOP2013; set meps.h160f (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F OP: SEETLKPV); 
			if SEETLKPV ne 2; run;
	data inOB2013; set meps.h160g (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F OB: SEETLKPV);	 
			if SEETLKPV ne 2; run;
	data inHH2013; set meps.h160h (keep = DUPERSID EVNTIDX VARSTR VARPSU PERWT13F HH: ); run; 


	* This macro combines expenditure variables in each event file, and gives them a common name;
	%macro summarize(ET);
		data &ET.2013;
			set in&ET.2013; 

			%if &ET = ER or &ET = OP or &ET = IP %then %do; * These event types have separate varibles for facility and doctor fees -- combine;
				&ET.SF13X = &ET.FSF13X + &ET.DSF13X;
				&ET.MR13X = &ET.FMR13X + &ET.DMR13X;
				&ET.MD13X = &ET.FMD13X + &ET.DMD13X;
				PV = &ET.FPV13X + &ET.DPV13X + &ET.FTR13X + &ET.DTR13X ;
			
				&ET.OTH13X = &ET.FVA13X + &ET.FOF13X + &ET.FSL13X + &ET.FWC13X + &ET.FOT13X + &ET.FOR13X + &ET.FOU13X +
						  	  &ET.DVA13X + &ET.DOF13X + &ET.DSL13X + &ET.DWC13X + &ET.DOT13X + &ET.DOR13X + &ET.DOU13X;
				%end;

			%else %do;
				&ET.OTH13X = &ET.VA13X  + &ET.OF13X + &ET.SL13X + &ET.WC13X + &ET.OT13X + &ET.OR13X + &ET.OU13X; 
				PV = &ET.PV13X + &ET.TR13X; * Private ins. = private + Tricare;
				%end;

			SF = &ET.SF13X;
			MR = &ET.MR13X;
			MD = &ET.MD13X;
			OTH = &ET.OTH13X;
			EXP = &ET.XP13X;
			&ET.XP = &ET.XP13X;
			
			EVENT = "&ET";
			keep DUPERSID EVNTIDX SF MR MD PV OTH EXP &ET.XP VARSTR VARPSU PERWT13F EVENT;

		run;
	%mend summarize;

		%summarize(OM);	%summarize(IP);	%summarize(ER);	%summarize(OP);
		%summarize(OB);	%summarize(HH);	%summarize(RX);

		** Combine events files; 
			data all_events2013;
				set RX2013 IP2013 ER2013 OM2013 OP2013 OB2013 HH2013; 
			run;

			proc contents data = all_events2013;
			run;

			proc print data = all_events2013 (obs=6);
			run;

		** Merge with conditions-link file; 
		proc sort data = condevent2013 nodupkey; 
			by DUPERSID EVNTIDX CCC ; ** Remove duplicates by person ID, EVENT ID, Condition Code;
		run;
		proc sort data = all_events2013 nodupkey; 
			by DUPERSID EVNTIDX;  * There should be 0 duplicates;
		run;

		proc contents data = condevent2013;
		run;

		data am2013;
			merge all_events2013 condevent2013;
			by DUPERSID EVNTIDX;
		run;


		data all_eventsMerge2013;

			set am2013;

			if CONDN = . then delete;
			if EXP = -1 then delete; * These are phone calls with expenditure of -1;

			if 		 0 <= AGELAST < 18 	then AGECAT3 = "0-17 ";
			else if 18 <= AGELAST 		then AGECAT3 = "18+";

			** Combine some condition codes;
			** IMPORTANT UPDATE 4/12/2016: need to do this before de-duplicating event by condition name;

			**	!! This means that CND_NAME ne CND61CAT!! **;
			genital_disorder = findw(CND_NAME,"genital");
			if genital_disorder > 0 then CND_NAME = "Genital Disorders";

			* As per Anitas email, combine GI disorders and Eye disorders;
			if CCC in ('086','087','088','089','090','091') then CND_NAME = "Eye disorders";
			if CCC in ('138','139','140','141','153','154','155') then CND_NAME = "GI disorders";
		run;

		proc contents data = all_eventsMerge2013;
		run;


		proc print data = all_eventsMerge2013 (obs=6);
		run;


		** Deduplicate by Event ID, CND_NAME, so we do not double count expenses for same condition;
		proc sort data = all_eventsMerge2013 out = all_events_nodup2013 nodupkey;
			by DUPERSID EVNTIDX CND_NAME EXP;
		run;

	data all_events;
		length agelast 8. varpsu 8. varstr 8. ; 
		set all_events_nodup2013;
		year = '2013';

		** Make variables that replace missings with 0, so ratio estimates are accurate;
		RXXP_0 = sum(0,RXXP); ERXP_0 = sum(0,ERXP); OMXP_0 = sum(0,OMXP);
		OBXP_0 = sum(0,OBXP); OPXP_0 = sum(0,OPXP); IPXP_0 = sum(0,IPXP);
		HHXP_0 = sum(0,HHXP);

		OPOBXP = sum(OPXP,OBXP); * Combine OP and OB to mimic online tables;
		OPOBXP_0 = sum(0,OPOBXP);

		Any_events = (EXP >= 0);
	run;

**	3. Summing expenditures across events per person, condition;

	* Sum by Person (DUPERSID), Condition (CND61CAT), across Events (EVNTIDX);
	proc sort data = all_events; 
		by year DUPERSID CND_NAME agecat3; 
	run;

	proc means data = all_events sum noprint;
		by year DUPERSID CND_NAME agecat3;
		output out = pers_all 
			sum(EXP  	Any_events  
				SF   	MR   	MD   	PV   	OTH 
				RXXP_0 	IPXP_0	ERXP_0	OMXP_0	OPOBXP_0 HHXP_0	 ) = 	
			mean(VARSTR VARPSU AGELAST PERWT13F) = ; * this mean() will calculate 'mean' of these variables;
	run;

	DATA pers_all;
		set pers_all;
		Any_event = (Any_events > 0); *indicator for whether a person had ANY event
			* (to count # of people with treated condition);
	run;

***********************************************************************************************************;
** use PROC SURVEYMEANS to calculate total and mean expenditures per condition, plus # of events and people;

		proc sort data = pers_all; by CND_name; run;
	
		ods listing close; ods html close;
		proc surveymeans data = pers_all nobs sumwgt mean sum clm clsum cv cvsum;
			*where AGECAT3 = '18+'; ** I have to subset to AGECAT3 = 18+ instead of DOMAIN because of a bug in surveymeans;
			stratum varstr;
			cluster varpsu;
			weight PERWT13F;
			domain CND_NAME;
			var	EXP	 Any_event; 	
			ratio 	RXXP_0 	IPXP_0 	ERXP_0 	OPOBXP_0 HHXP_0
				  	SF   	MR  	MD  	PV  	 OTH   / EXP ;

			ods output 	domain = pers_out1 		
						domainRatio = pers_out2; 	
		run;
		ods listing; ods html;
		
		data N_ppl; *Figure 1;
			set pers_out1;
			where VarName = "Any_event";
			keep CND_NAME Sum StdDev LowerCLSum UpperCLSum;
			rename Sum = Nppl StdDev = SE_Nppl LowerCLSum = LowerCL_Nppl UpperCLSum = UpperCL_Nppl; 
		run;

		data Expenditures; *Figures 2,3;
			set pers_out1;
			where VarName = "EXP";
			keep CND_NAME Mean StdErr  LowerCLMean UpperCLMean /* Figure 2 */
				 	      Sum  StdDev  LowerCLSum  UpperCLSum  /* Figure 3 */;
			rename Mean = MeanEXP StdErr = SE_MeanEXP LowerCLMean = LowerCL_MeanEXP UpperCLMean = UpperCL_MeanEXP
			 	   Sum  = TotEXP  StdDev = SE_TotEXP  LowerCLSum  = LowerCL_TotEXP  UpperCLSum  = UpperCL_TotEXP;
		run;
			

		proc print data = Expenditures;
		run;

		proc transpose data = pers_out2 out = TOS_SOP; *Figures 4,5;
			by CND_NAME;
			id NumeratorName;
			var Ratio;
		run;

		* Get total # of people, to calcualte percent of population with treated condition;
			proc surveymeans data = FYC2013 sumwgt ;
				where AGELAST >= 18;
				stratum varstr;
				cluster varpsu;
				weight PERWT13F;
				var	IND; 	
			run;
			
		** Merge to single data set;
		proc sort data = N_ppl; 	   by CND_NAME; run;
		proc sort data = Expenditures; by CND_NAME; run;
		proc sort data = TOS_SOP; 	   by CND_NAME; run;

		data final_out;
			merge N_ppl Expenditures TOS_SOP;
			by CND_NAME;
			Pct_Pop = Nppl/241687096; *Number of adults;
		run;

		proc sort data = final_out;
			by descending Nppl;
		run;

		data final_out;
			* order variables for output;
			retain CND_NAME Pct_Pop  Nppl  SE_Nppl  LowerCL_Nppl  UpperCL_Nppl /*Figure 1*/
				MeanEXP   SE_MeanEXP  LowerCL_MeanEXP  UpperCL_MeanEXP         /*Figure 2*/
				TotEXP    SE_TotEXP   LowerCL_TotEXP   UpperCL_TotEXP          /*Figure 3*/
				OPOBXP_0  IPXP_0     ERXP_0     RXXP_0      HHXP_0             /*Figure 4*/
				SF		  PV		 MR			MD			OTH                /*Figure 5*/
				;
			set final_out;
			drop _NAME_;
		run;

		proc print data = final_out (obs=11);
		run;


***********************************************************************;

proc export data = final_out
	outfile = "C:\Users\emily.mitchell\Desktop\AHRQ\MEPS\Costly Conditions\CC_SAS_Output.xlsx"
		dbms = xlsx replace;
		sheet = "18+";
run;



