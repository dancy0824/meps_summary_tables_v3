%JOBSTART;

ODS NOPROCTITLE;
OPTIONS LS=155 PS=55;
***********************************************************************************************
Software Version:   9.2
Creation Date:      2008-08-29              
Task:               AH3.FD001
Task Title:         2007 Prescribed Medicines Event PUF

Program Name: AHRQ3\F_DSO\FD001NKK\PROG\TOP10RX07_USE.SAS

Description:  Produce estimates with standard errors of top 10 drugs in 2007 
              by weighted utilization for the Tables Compendia on the Web.
              Produce Z-scores comparing weighted utilization of
              top 10 drugs to each other.

              Output the table statistics and formats in order to convert tables to HTML format.

Reference:    Marie Stagnitti / Nancy Kieffer
              Phone coversation with Marie 11/6/07: Rank using non-confidential data
              Stagnitti's email dated 10/23/08 - asterisk for outlier drug

Input file:   AHRQ3\F_DSO\FD001NKK\DATA\RX07V06.SAS7BDAT
              AHRQPUF\HCPUFS\HC107\DATA\H107V1.SAS7BDAT
              AHRQ3\BC002XXX\DATA\MEPSHC2008\D082410\PERWT08F_ADJ.SAS7BDAT
                                                  (Final expenditure person level weight)

Output file:  AHRQ3\F_DSO\FD001NKK\DATA\FMTLIB\RX07_USEFMT.SAS7BCAT
              AHRQ3\F_DSO\FD001NKK\DATA\TABLES\TOP10RX07_USE.SAS7BDAT

Programmer:   Nancy Kieffer

Update History:  Blaine Byars
							2008-10-30 Null out unreliable estimates on Web reports
							2009-08-08 Use updated VARSTR/VARPSU
********************************************************************************************;
%LET YY=08;
%LET FYPUF=H115V1;     /* FY Person Characteristics PUF file name   */
%LET FYNUM=115;        /* FY Person Characteristics PUF file number */

LIBNAME RXPUF "C:\AHRQ3\F_DSO\FD001NKK\DATA";
LIBNAME RXTAB "C:\AHRQ3\F_DSO\FD001NKK\DATA\TABLES";
LIBNAME RXFMT "C:\AHRQ3\F_DSO\FD001NKK\DATA\FMTLIB";
LIBNAME WGT "C:\AHRQ3\BC002XXX\DATA\MEPSHC2008\D082410";
LIBNAME FYPUF "C:\AHRQPUF\HCPUFS\HC&FYNUM\DATA";
/*LIBNAME VARSTR "C:\AHRQ2\BA002XXX\Data\WGTS\D072409";   2007 only */

OPTIONS FMTSEARCH=(RXFMT.RX&YY._USEFMT);

TITLE1 "Task AH3.FD001: 20&YY Prescribed Medicines Event PUF";

*---------------------------------------------------------------------------------------------
* Add final person-level weight to all FY persons and subset to persons with positive weight.
* File will be used in SUDAAN to read in the full FY sample.
*---------------------------------------------------------------------------------------------;
PROC SORT DATA=FYPUF.&FYPUF (KEEP=PERSID VARSTR VARPSU READ=&PWD)
           OUT=FYPUF;
  BY PERSID;
RUN;

PROC SORT DATA=WGT.PERWT&YY.F_ADJ(KEEP=PERSID PERWT_ADJ2 RENAME=(PERWT_ADJ2=PERWT&YY.F))
           OUT=PERWT;
  BY PERSID;
RUN;

/*For 2007, a special update of VARSTR, VARPSU was delivered;
PROC SORT DATA=VARSTR.p11p12varstr_v2 (KEEP=PERSID VARSTR VARPSU )
           OUT=VARSTR;
  BY PERSID;
RUN;*/
 
DATA FYPERS;
  MERGE FYPUF (IN=A)
        PERWT (IN=B)
        /*VARSTR (IN=INVARSTR)*/
        END=EOF;
  BY PERSID;
   
  C1 + (A AND NOT B);
  C2 + (B AND NOT A);
  C3 + (A AND B);
  /*V3 + (A AND B AND INVARSTR); */
   
  IF EOF THEN DO;
     PUT "IN FYPUF ONLY:  " C1;
     PUT "IN WEIGHT FILE ONLY: " C2;
     PUT "IN BOTH:        " C3;
     /*PUT "IN FYPUF & WEIGHT FILE & NEW VARSTR: " V3;*/
  END;
  
  IF A and PERWT&YY.F > 0;                  
  DROP C1 C2 C3;   
RUN;


*----------------------------------------------------------------------------------------------
* Subset PMED PUF to persons with positive weights.
*----------------------------------------------------------------------------------------------;
DATA PMEDS;
  SET RXPUF.RX&YY.V06 (KEEP=PERSID PERWT&YY.F VARPSU VARSTR PARTITIO_WEB PARTMED_WEB RXXP&YY.X);
  IF PERWT&YY.F > 0;
RUN;

*---------------------------------------------------------------------------------------
* Sum weighted utilization by non-missing medicine name partition groups.
*---------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS;
	 BY PARTITIO_WEB; 
RUN;

PROC SUMMARY DATA=PMEDS (WHERE=(PARTITIO_WEB NE .));
  BY PARTITIO_WEB;
  VAR PERWT&YY.F;
  *WEIGHT PERWT&YY.F; /* bdb 2009-08-05 */
  OUTPUT OUT=PARTSUM SUM=RXUSESUM;
RUN;

*-------------------------------------------------------------------------------
* Sort partition groups in descending order by weighted utilization.
* Assign rank to non-missing PARTITIO_WEB.
*-------------------------------------------------------------------------------;
PROC SORT DATA=PARTSUM;
	 BY DESCENDING RXUSESUM; 
RUN;

DATA PARTRANK;
  SET PARTSUM (KEEP=PARTITIO_WEB);
  RANK=_N_;
RUN;

*--------------------------------------------------------------------------------
* Merge RANK onto PMEDs. Recode all ranks > 10 & missing ranks into RANK=11.
* For ranks not in top 10, set their PARTITIO_WEB medicine name to "all others".
* RANK will be missing for those records where PARTITIO_WEB is missing.
*--------------------------------------------------------------------------------;
PROC SORT DATA=PARTRANK;
	 BY PARTITIO_WEB; 
RUN;

DATA PMEDS;
  MERGE PMEDS
        PARTRANK;
  BY PARTITIO_WEB;

  IF RANK > 10 OR RANK=. THEN DO;
     RANK = 11;
     PARTMED_WEB = "All Others";
  END;
RUN;

*--------------------------------------------------------------------------------------
* Create dummy variable to identify unique persons in RANK.
*--------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS;
	 BY RANK PERSID; 
RUN;

DATA PMEDS;
  SET PMEDS;
  BY RANK PERSID;
  
  IF FIRST.PERSID 
  	THEN RANKPERS=1; 
  	ELSE RANKPERS=0;
  
RUN;

*---------------------------------------------------------------------------------
* Create format for RANK by crossing RANK with PARTMED_WEB.
*---------------------------------------------------------------------------------;
PROC FREQ DATA=PMEDS;
  TABLES RANK*PARTMED_WEB / LIST OUT=RANKFREQ;
  TITLE3 "Verification of RANK by medicine name";
  TITLE5 "RANK based on weighted frequency of drug name";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\RX&YY.V06.SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

DATA FMTDRUGS (KEEP=FMTNAME START LABEL TYPE);
  SET RANKFREQ END=EOF;
  
  LENGTH FMTNAME $8 START $2 LABEL $45;
  
  FMTNAME="RANKFMT";
  START=PUT(RANK,2.);
  NUMLABEL=TRIM(PUT(RANK,8.));
  TYPE="N";
  
  LABEL=NUMLABEL || " " || PARTMED_WEB;
  LABEL=LEFT(RIGHT(LABEL));
 
  OUTPUT;

  IF EOF THEN DO; /* Total line for tables */
     START="99";
     LABEL="TOTAL";
     LABEL=LEFT(RIGHT(LABEL));
     OUTPUT;
   END;
RUN;

PROC SORT DATA=FMTDRUGS; BY START; RUN;

PROC PRINT DATA=FMTDRUGS;
  TITLE3 "Verification of creating formats for variable RANK";
  TITLE5 "WORK.FMTDRUGS: Input Control Dataset used to create permanent format for RANK";
  FOOTNOTE1;
RUN;
 
PROC FORMAT LIBRARY=RXFMT.RX&YY._USEFMT CNTLIN=FMTDRUGS FMTLIB; 
  TITLE3 "Contents of permanent format library for RANK";
  FOOTNOTE1 "C:\AHRQ3\F_DSO\FD001NKK\DATA\FMTLIB\RX&YY._USEFMT";
RUN;

PROC FREQ DATA=PMEDS;
  TABLES RANK / MISSING;
  FORMAT RANK RANKFMT.;
  TITLE3 "Unweighted formatted frequency of RANK";
  TITLE5 "RANK based on weighted frequency of drug name";
  FOOTNOTE1;
RUN;

PROC FREQ DATA=PMEDS;
  TABLES RANK / MISSING;
  WEIGHT PERWT&YY.F;
  FORMAT RANK RANKFMT.;
  TITLE3 "Weighted formatted frequency of RANK";
  TITLE5 "RANK based on weighted frequency of drug name";
RUN;

*------------------------------------------------------------------------------------
* Produce SAS table.
*------------------------------------------------------------------------------------;
PROC SUMMARY DATA=PMEDS MISSING;
  CLASSES RANK;
  VAR RXXP&YY.X RANKPERS;
  WEIGHT PERWT&YY.F;
  OUTPUT OUT=SASTABLE SUMWGT=POPEST SUM=SUMRXEXP SUMPERS;
RUN;

DATA SASTABLE;
  SET SASTABLE;
  IF RANK=. THEN RANK=99; /* Total line */
RUN;

PROC SORT DATA=SASTABLE; BY RANK; RUN;

PROC PRINT DATA=SASTABLE NOOBS SPLIT="*";
  VAR RANK _FREQ_ POPEST SUMRXEXP SUMPERS;
  LABEL RANK    = "Drug*Rank &*Name"
        _FREQ_  = "Drug*Mentions*Unweighted"
        POPEST  = "Drug*Mentions*Weighted"
        SUMRXEXP= "Dollars*Spent*Weighted"
        SUMPERS = "Count of*Persons*Weighted";
  FORMAT RANK RANKFMT.;
  FORMAT _FREQ_ COMMA8.;
  FORMAT POPEST COMMA20.;
  FORMAT SUMRXEXP COMMA20.;
  FORMAT SUMPERS COMMA15.;
  TITLE3 "Utilization and Expenditures for the Top 10 Prescribed Medicines";
  TITLE5 "Ranked by Weighted Utilization";
  TITLE7 "SAS Output";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\RX&YY.V06.SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

*--------------------------------------------------------------------------------------
* Bring in all persons from FY PUF for SUDAAN.
* Flag persons with PMEDs.
*--------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS;
	 BY PERSID; 
RUN;
                                        
DATA PMEDS;
  MERGE FYPERS (IN=INFY)
        PMEDS  (IN=INRX)
  END=EOF;
  BY PERSID;
  IF INFY;
  
  LABEL HASPMED = "1 if person has PMED event";
   
  HASPMED=INRX;
  
  IF NOT INRX
   THEN RXXP&YY.X=0;
   
  C1 + (INFY AND NOT INRX);
  C2 + (INRX AND NOT INFY);
  C3 + (INRX AND INFY);
  
  IF EOF THEN DO;
     PUT "IN FY CONSOLIDATED PUF ONLY: " C1;
     PUT "IN PMED EVENT FILE ONLY: " C2;
     PUT "IN BOTH:                 " C3;
   END;
   
   DROP C1 C2 C3;
RUN;     

*------------------------------------------------------------------------------------
* Produce standard errors.
*------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS;
	 BY VARSTR VARPSU; 
RUN;

TITLE3;
FOOTNOTE1;
PROC DESCRIPT DATA=PMEDS NOPRINT;
  SUBPOPN HASPMED=1;
  WEIGHT PERWT&YY.F;
  NEST VARSTR VARPSU / MISSUNIT;
  VAR _ONE_ RXXP&YY.X RANKPERS;
  SUBGROUP RANK;
  LEVELS 11;
  SETENV LABWIDTH=40;
  OUTPUT NSUM WSUM TOTAL SETOTAL / FILENAME=SESTATS FILETYPE=SAS REPLACE;
  RFORMAT RANK RANKFMT.;
RUN;

*----------------------------------------------------------------------------------
* Reformat SUDAAN output for printing.
*----------------------------------------------------------------------------------;
DATA USE (RENAME=(TOTAL=TOT_USE SETOTAL=SE_USE))
     EXP (RENAME=(TOTAL=TOT_EXP SETOTAL=SE_EXP))
     PER (RENAME=(TOTAL=TOT_PER SETOTAL=SE_PER));
  SET SESTATS;

  /* Move total line from top to bottom */
  IF RANK=0 THEN RANK=99;

  IF VARIABLE=1 THEN OUTPUT USE; ELSE
  IF VARIABLE=2 THEN OUTPUT EXP; ELSE
  IF VARIABLE=3 THEN OUTPUT PER;
  
  DROP PROCNUM TABLENO VARIABLE _C1;
RUN;

PROC SORT DATA=USE; BY RANK; RUN;
PROC SORT DATA=EXP; BY RANK; RUN;
PROC SORT DATA=PER; BY RANK; RUN;

DATA RXTAB.TOP10RX&YY._USE;
  MERGE USE
        EXP
        PER;
  BY RANK;
  LABEL RANK    = "Drug Rank & Name"
        NSUM    = "Drug Mentions, Unweighted"
        TOT_USE = "Drug Mentions, Weighted"
         SE_USE = "Drug Mentions SE"
        TOT_EXP = "Dollars Spent, Weighted"
         SE_EXP = "Dollars Spent SE"
        TOT_PER = "Count of Persons, Weighted"
         SE_PER = "Count of Persons SE";
RUN;

PROC CONTENTS DATA=RXTAB.TOP10RX&YY._USE;
  TITLE3 "Contents of TOP10RX&YY._USE.SAS7BDAT";
  TITLE5 "Dataset for Table Top Rx Ranked by Utilization";
  FOOTNOTE1 "C:\AHRQ3\F_DSO\FD001NKK\DATA\TABLES\TOP10RX&YY._USE.SAS7BDAT";
RUN;

PROC PRINT DATA=RXTAB.TOP10RX&YY._USE NOOBS SPLIT="*";
  ID RANK;
  VAR NSUM
      TOT_USE SE_USE
      TOT_EXP SE_EXP
      TOT_PER SE_PER;
   FORMAT RANK RANKFMT.;
   FORMAT NSUM COMMA8.;
   FORMAT TOT_USE TOT_EXP TOT_PER COMMA20.;
   FORMAT  SE_USE  SE_EXP  SE_PER COMMA17.2;
   LABEL RANK    = "Drug*Rank &*Name"
         NSUM    = "Drug*Mentions*Unweighted"
         TOT_USE = "Drug*Mentions*Weighted"
          SE_USE = "Drug*Mentions*SE"
         TOT_EXP = "Dollars*Spent*Weighted"
          SE_EXP = "Dollars*Spent*SE"
         TOT_PER = "Count of*Persons*Weighted"
          SE_PER = "Count of*Persons*SE";
  TITLE3 "Utilization and Expenditures for the Top 10 Prescribed Medicines";
  TITLE5 "Ranked by Weighted Utilization";
  TITLE7 "SUDAAN Output";
RUN;

*----------------------------------------------------------------------------------------
* Compute z-scores for #1 v.s #2,#3,#4, etc. then #2 v.s #3,#4, etc. for top 10.
*----------------------------------------------------------------------------------------;
FOOTNOTE1;
PROC SORT DATA=RXTAB.TOP10RX&YY._USE (KEEP=RANK TOT_USE SE_USE WHERE=(1 LE RANK LE 10))
           OUT=SETABLE;
  BY RANK;
RUN;

PROC PRINT DATA=SETABLE;
  FORMAT RANK RANKFMT.;
  FORMAT TOT_USE SE_USE COMMA20.2;
  TITLE3 "QC testing for Z-Score computation";
  TITLE5 "before re-arranging variables";
RUN;

/* Put all the observations into a single observation using arrays */
DATA SIGSTAT (DROP=RANK TOT_USE SE_USE);
  SET SETABLE END=EOF;
  BY RANK;

  RETAIN RANK1-RANK10
         STAT1-STAT10
         SE1-SE10;

  ARRAY RNK(10)  RANK1-RANK10;
  ARRAY STAT(10) STAT1-STAT10;
  ARRAY SE(10)   SE1-SE10;
  
  IF _N_=1 THEN NSTAT=0;

  NSTAT+1;
  RNK(NSTAT)=NSTAT;
  STAT(NSTAT)=TOT_USE;
  SE(NSTAT)=SE_USE;

  IF EOF THEN OUTPUT;
RUN;

PROC PRINT DATA=SIGSTAT;
  TITLE3 "QC testing for Z-Score computation";
  TITLE5 "after re-arranging variables";
RUN;

/* Compute Z-Scores */
DATA ZSCORE (KEEP=DRUG1 DRUG2
                  TOTAL1 TOTAL2
                  SE_USE1 SE_USE2
                  STATDIFF SESQRT ZSTAT PVALUE);
  SET SIGSTAT;

  ARRAY RNK(10)   RANK1-RANK10;
  ARRAY CSTAT(10) STAT1-STAT10;
  ARRAY CSE(10)   SE1-SE10;

  DO M=1 TO NSTAT-1;
     DO J=1 TO (NSTAT-M);
        DRUG1=RNK(M);
        TOTAL1=CSTAT(M);
        SE_USE1=CSE(M);

        DRUG2=RNK(M+J);
        TOTAL2=CSTAT(M+J);
        SE_USE2=CSE(M+J);

        STATDIFF=TOTAL1-TOTAL2;
        SESQRT=SQRT((SE_USE1)**2 + (SE_USE2)**2);
        ZSTAT=ABS(STATDIFF / SESQRT);
        PVALUE=PDF("NORMAL",ZSTAT);

        OUTPUT;
     END;
  END;

RUN;

PROC PRINT DATA=ZSCORE NOOBS SPLIT="*";
  BY DRUG1;
  ID DRUG1;
  VAR DRUG2 TOTAL1 TOTAL2 SE_USE1 SE_USE2 STATDIFF SESQRT ZSTAT PVALUE;
  FORMAT DRUG1 DRUG2 RANKFMT.;
  FORMAT TOTAL1 TOTAL2 STATDIFF COMMA16.2;
  FORMAT SE_USE1 SE_USE2 SESQRT COMMA13.2;
  FORMAT ZSTAT 6.3;
  FORMAT PVALUE PVALUE6.3;     
  LABEL DRUG1    = "Drug #1"
        DRUG2    = "Drug #2"
        TOTAL1   = "Weighted*Utilization* for Drug #1"
        TOTAL2   = "Weighted*Utilization* for Drug #2"
        SE_USE1  = "SE of Weighted*Utilization* Drug #1"
        SE_USE2  = "SE of Weighted*Utilization* Drug #2"
        STATDIFF = "TOTAL1-TOTAL2"
        SESQRT   = "SESQRT"
        ZSTAT    = "Z-SCORE"
        PVALUE   = "P-VALUE";
  TITLE3 "Z Scores to Compare Top 10 Drugs when Ranked by Weighted Utilization";
RUN;

*--------------------------------------------------------------------------------------
* Null out estimates with unstable SE for Web reports.
*--------------------------------------------------------------------------------------;

data RXTAB.TOP10RX&YY._USE;
  set RXTAB.TOP10RX&YY._USE;

  if se_exp / tot_exp > 0.90
   then do;
   				se_exp = .;
					tot_exp = .;
			 end;
  if se_use / tot_use > 0.90
   then do;
   				se_use = .;
					tot_use = .;
			 end;
  if se_per / tot_per > 0.90
   then do;
   				se_per = .;
					tot_per = .;
			 end;
run;

proc sql noprint;
  select count(*) into: censored
   from rxtab.TOP10RX&YY._USE
   where se_exp=. or se_use=. or se_per=.
   ;
quit;
%put Note: censored=&censored;

%macro censoreds;
  %if &censored
   %then %do;
options missing='*';
PROC PRINT DATA=RXTAB.TOP10RX&YY._USE NOOBS SPLIT="*";
  ID RANK;
  VAR NSUM
      TOT_USE SE_USE
      TOT_EXP SE_EXP
      TOT_PER SE_PER;
   FORMAT RANK RANKFMT.;
   FORMAT NSUM COMMA8.;
   FORMAT TOT_USE TOT_EXP TOT_PER COMMA20.; 
   FORMAT  SE_USE  SE_EXP  SE_PER COMMA20.2;
   LABEL RANK    = "Drug*Rank &*Name"
         NSUM    = "Drug*Mentions*Unweighted"
         TOT_USE = "Drug*Mentions*Weighted"
          SE_USE = "Drug*Mentions*SE"
         TOT_EXP = "Dollars*Spent*Weighted"
          SE_EXP = "Dollars*Spent*SE"
         TOT_PER = "Count of*Persons*Weighted"
          SE_PER = "Count of*Persons*SE";
  TITLE3 "Utilization and Expenditures for the Top 25 Prescribed Medicines";
  TITLE5 "Ranked by Weighted Expenditures";
  TITLE7 "Showing Estimates Considered Unreliable";
  footnote "* These estimates are not reported, as they are considered unreliable.";
RUN;

		 %end;
%mend censoreds;
%censoreds;

%JOBEND;