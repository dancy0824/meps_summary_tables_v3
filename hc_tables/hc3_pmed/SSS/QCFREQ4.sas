%JOBSTAR2;
/*:JOBNAME PMED:%programname%*/ 
/*:AFTER -JOB PMED:QCFREQ3*/
 
ODS NOPROCTITLE;
OPTIONS LS=166 PS=70;
**************************************************************************************
Software Version:   9.4
Creation Date:      2008-02-07              
Task:               AH4.FC004
Task Title:         2015 Prescribed Medicines Data Editing and Imputation Work

Program Name: \\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FC004NKK\2015\Prog\QCFREQ4.SAS

Description:  Ranks drug groups by weighted total expenditures.
              Drug groups are based on NDCX and MEDNAMEC.
              Produces frequencies for the top 50 drug groups for total sample and
              subpopulations. As of 2004 data, it will no longer include the additional
              drugs specified in John''s 10/26/99 memo per Stagnitti 9/16/05 email.

Reference:    John Moeller 10/26/99, 7/23/99, 6/18/2002, 6/19/2002
              Marie Stagnitti email 9/16/2005 (AH3.FB001)
              Steve Hill 4/20/2015
              Marie Stagnitti / Nancy Kieffer

Input file:   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FC004NKK\2015\Data\MEPSHC15\PMED08.SAS7BDAT 
              \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FC004NKK\2015\Data\MEPSHC15\RX13MUL4.SAS7BDAT
              \\source.ahrq.local\source\MEPS\HCPUFS\HC165\DATA\H165V1.SAS7BDAT  (FY person-level PUF)
              \\source.ahrq.local\source\MEPS\CONFIDENTIAL\2015\WEIGHT\FY\PERWT14F_ORIG.SAS7BDAT

Output file:  None

Programmer:   Ed Hock / Nancy Kieffer

Update History: Blaine Byars
        2009-08-13 Run on the special version of PMED09B built to incorporate the 
            outlier price/quantity adjustments which were made in program FC012.RX07V04
        2011-06-28 Use PMED09 as input
		    5/1/15 ETH Changed so that DRG_NAM1 is used to assign drug groups instead of NDCX and MEDNAMEC
        03/31/17 (CSM): Edited to allow the use of PERWT&YY.P until the final weight is available
                        Used crosswalk to create PERSID for FYPUF.&FYPNAM
        04/25/17 (CSM): Rerun after Pharmacy review and with final weight
*******************************************************************************************;
%INCLUDE '\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FC004NKK\2015\Prog\STDPARM.SAS';   
 
LIBNAME PMEDS "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FC004NKK\20&YY.\Data\MEPSHC&YY";
LIBNAME FYPUF "\\source.ahrq.local\source\MEPS\HCPUFS\HC&FYPNUM\Deliv02102017";
LIBNAME XLK "\\source.ahrq.local\source\MEPS\CONFIDENTIAL\20&YY\CROSSWALK"; 

%LET HAVFINWT=1;
%MACRO HAVEWGT;
  %GLOBAL WGT;
  %LET WGT=PERWT&YY.P;
  %IF "&HAVFINWT" EQ "1" %THEN %DO;
LIBNAME WTC "\\source.ahrq.local\source\MEPS\CONFIDENTIAL\20&YY\WEIGHT\FY"; 
    %LET WGT=PERWT&YY.F_ORIG;
  %END;
%MEND;
%HAVEWGT
 
TITLE1 "Task AH4.FC004: 20&YY Prescribed Medicine Data Editing and Imputation Work";
 
/*PROC SORT DATA=FYPUF.&FYPNAM (KEEP=PERSID MCREV&YY PW="&PWD") OUT=FYPUF;
  BY PERSID; 
RUN;*/

/*Temp code for 2015: due to H174v1 is NA*/
PROC SORT DATA=XLK.hcxwlk21(WHERE=(HCPANEL IN (19,20))) OUT=XLK; 
	BY DUID; 
RUN; 

DATA FY CHECK;
  LENGTH PERSID $ 8.;
	MERGE FYPUF.&FYPNAM(IN=A)
	      XLK(IN=B);
  BY DUID;
  IF A;
  IF NOT B THEN OUTPUT CHECK;
  PERSID= PUT(ODU,Z5.)||PUT(PID,3.);
  OUTPUT FY;
RUN;
      
PROC SORT DATA=FY OUT=FYPUF(KEEP=PERSID MCREV&YY);
  BY PERSID;
RUN;

PROC PRINT DATA=FY(OBS=50);
TITLE3 "CHECK CREATION OF PERSID";
VAR ODU DUID PERSID DUPERSID;
RUN;
 
DATA PMEDS (KEEP=PERSID EVNTID AGE RXEXP /*MCREV&YY*/ /*WRXEXP*/ PERWT&YY.P DRG_NAM1 DRUG_ID1 DRUGMISS);
  MERGE PMEDS.PMED08
        PMEDS.RX&YY.MUL4(KEEP=UNIQUEID DRG_NAM1 DRUG_ID1)
        ;
    BY UNIQUEID;
  IF RX20&YY=1;
 
  DRUGMISS = (DRUG_ID1 EQ '');
RUN;
PROC SORT;
  BY PERSID;
RUN;

%MACRO HAVEWGT;
  %IF "&HAVFINWT" EQ "1" %THEN %DO;
DATA PERWT&YY.F;
  SET WTC.&WGT (KEEP=PERSID &WGT);
RUN;
PROC SORT;
  BY PERSID;
RUN;
  %END;
%MEND;
%HAVEWGT

DATA PMEDS;
  MERGE PMEDS(IN=IN1)
        FYPUF(KEEP=PERSID MCREV&YY IN=IN2)
%MACRO HAVEWGT;
  %IF "&HAVFINWT" EQ "1" %THEN %DO;
        PERWT&YY.F
  %END;
%MEND;
%HAVEWGT
        ;
    BY PERSID;
  IF IN1;
  IF NOT(IN2) THEN PUT 'ERR' 'OR: NO PUF RECORD FOR ' PERSID=;
  WRXEXP=RXEXP*&WGT;
RUN;
PROC SORT;
  BY DRUG_ID1;
RUN;
 
%MACRO ONEFREQ(SUBSET=1,SUBTIT=);
 
PROC SUMMARY DATA=PMEDS MISSING;
  WHERE (&SUBSET);
  BY DRUG_ID1;
  ID DRUGMISS;
  VAR WRXEXP;
  OUTPUT OUT=RANK (KEEP=DRUG_ID1 DRUGMISS WRXEXP) SUM=;
RUN;
 
PROC SORT DATA=RANK; BY DRUGMISS DESCENDING WRXEXP; RUN;
 
DATA RANK (KEEP=DRUG_ID1 RANK);
  SET RANK;
  RANK = _N_;
RUN;
 
PROC SORT DATA=RANK; BY DRUG_ID1; RUN;
 
DATA PMEDSUB;
  MERGE PMEDS (WHERE=(&SUBSET)) RANK;
  BY DRUG_ID1;
  IF RANK GT 50 THEN DO;
     RANK = 99999;
     DRUG_ID1 = "Other";
     DRG_NAM1 = "All Others";
  END;
RUN;
 
PROC SORT DATA=PMEDSUB; BY PERSID; RUN;
 
DATA PMEDSUB;
  SET PMEDSUB;
  BY PERSID;
  IF FIRST.PERSID THEN DO;
    UPERS1=1;
    WPERS1=&WGT;
  END;
  ELSE DO;
    UPERS1=0;
    WPERS1=0;
  END;
RUN;
 
PROC SORT DATA=PMEDSUB; BY RANK PERSID; RUN;
 
DATA PMEDSUB;
  *LENGTH PARTMED $ 35;
  SET PMEDSUB;
  BY RANK PERSID;
  IF FIRST.PERSID THEN DO;
    UPERS2=1;
    WPERS2=&WGT;
  END;
  ELSE DO;
    UPERS2=0;
    WPERS2=0;
  END;
RUN;
 
PROC REPORT DATA=PMEDSUB NOWD HEADLINE MISSING;
  COLUMN RANK DRG_NAM1 RXEXP WRXEXP &WGT=TMPV1 &WGT=TMPV2 TMPV2X &WGT &WGT=TMPV3 TMPV3X
                      UPERS1=TMPV4 WPERS1=TMPV5 UPERS2=TMPV6 WPERS2=TMPV7 UPERS WPERS;
  DEFINE RANK / GROUP 'Rank' FORMAT=5.;
  DEFINE DRG_NAM1 / GROUP FORMAT=$CHAR35. 'Medicine Name' ;
  DEFINE RXEXP / ANALYSIS SUM FORMAT=COMMA13.2 'Unweighted Dollars Spent (All SOPs)';
  DEFINE WRXEXP / ANALYSIS SUM FORMAT=COMMA18.2 'Weighted Dollars Spent (All SOPs)';
  DEFINE TMPV1 / ANALYSIS N FORMAT=COMMA10. 'Unweighted Frequency of Drug Mentions';
  DEFINE TMPV2 / ANALYSIS PCTN NOPRINT;
  DEFINE TMPV2X / COMPUTED FORMAT=COMMA10.2 'Unweighted Percent of Drug Mentions';
  DEFINE &WGT / ANALYSIS SUM FORMAT=COMMA16.2 'Weighted Frequency of Drug Mentions';
  DEFINE TMPV3 / ANALYSIS PCTSUM NOPRINT;
  DEFINE TMPV3X / COMPUTED FORMAT=COMMA8.2 'Weighted Percent of Drug Mentions';
  DEFINE TMPV4 / ANALYSIS SUM NOPRINT;
  DEFINE TMPV5 / ANALYSIS SUM NOPRINT;
  DEFINE TMPV6 / ANALYSIS SUM NOPRINT;
  DEFINE TMPV7 / ANALYSIS SUM NOPRINT;
  DEFINE UPERS / COMPUTED FORMAT=COMMA10. 'Unweighted Count of People';
  DEFINE WPERS / COMPUTED FORMAT=COMMA16.2 'Weighted Count of People';
  RBREAK AFTER / DOL SUMMARIZE;
  COMPUTE TMPV2X;
    TMPV2X = TMPV2*100;
  ENDCOMP;
  COMPUTE TMPV3X;
    TMPV3X = TMPV3*100;
  ENDCOMP;
  COMPUTE UPERS;
    UPERS = TMPV6;
  ENDCOMP;
  COMPUTE WPERS;
    WPERS = TMPV7;
  ENDCOMP;
  COMPUTE AFTER;
    UPERS = TMPV4;
    WPERS = TMPV5;
  ENDCOMP;
  TITLE3 "FREQUENCY OF MEDICINES WEIGHTED BY &WGT - &SUBTIT";
  TITLE4 "SORTED BY RANK BASED ON WEIGHTED DOLLARS SPENT";
  TITLE5 "TOP 50";
RUN;
 
%MEND;
 
%ONEFREQ(SUBTIT=ALL 20&YY DRUGS);
%ONEFREQ(SUBSET=AGE LT 65,SUBTIT=20&YY DRUGS FOR PEOPLE UNDER AGE 65);
%ONEFREQ(SUBSET=AGE GE 65,SUBTIT=20&YY DRUGS FOR PEOPLE AGE 65 OR OLDER);
%ONEFREQ(SUBSET=MCREV&YY EQ 1,SUBTIT=20&YY DRUGS FOR PEOPLE ON MEDICARE AT ANY TIME DURING THE YEAR);
 
%JOBEND;
