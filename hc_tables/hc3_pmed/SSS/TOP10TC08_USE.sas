%JOBSTART;

ODS NOPROCTITLE;
OPTIONS LS=155 PS=55;

*OPTIONS OBS=0;
********************************************************************************************
Software Version:   9.2
Creation Date:      2008-08-29              
Task:               AH3.FD001
Task Title:         2007 Prescribed Medicines Event PUF

Program Name: AHRQ3\F_DSO\FD001NKK\PROG\TOP10TC07_USE.SAS

Description:  Produces estimates with standard errors of the top 10 therapeutic classes (TC)  
              of drugs in 2007 by weighted utilization for the Tables Compendia on the Web.
              Produce Z-scores comparing weighted utilization of top 10 TC to each other.

              Outputs the table statistics and formats in order for conversion to HTML.

Reference:    Marie Stagnitti / Nancy Kieffer
              Stagnitti email dated 4/30/2007
              Stabnitti email 06/25/2009 re final person weights

Input file:   AHRQ3\F_DSO\FD001NKK\DATA\RX07V05.SAS7BDAT		(encrypted PMED PUF)
              AHRQPUF\HCPUFS\HC107\DATA\H107V1.SAS7BDAT			(FY Person Char. PUF)
                                                  (Final expenditure person level weight)

Programmer:   Nancy Kieffer

Update History:  2009-07-14 Blaine Byars -- Use expenditure person level weight 
                in place of Master person level weight -- these weights were declared final
							2009-08-08 Use updated VARSTR/VARPSU
*******************************************************************************************;
%LET YY=08;

%LET RXPUF=RX&YY.V05;
%LET FYPUF=H115V1;     /* FY Person Characteristics PUF file name   */
%LET FYNUM=115;        /* FY Person Characteristics PUF file number */

LIBNAME FMT   "C:\AHRQ3\F_DSO\FD001NKK\DATA\FMTLIB";
LIBNAME RXPUF "C:\AHRQ3\F_DSO\FD001NKK\DATA";  
LIBNAME TCTAB "C:\AHRQ3\F_DSO\FD001NKK\DATA\TABLES";
LIBNAME WGT "C:\AHRQ3\BC002XXX\DATA\MEPSHC2008\D082410";
/*LIBNAME VARSTR "C:\AHRQ2\BA002XXX\Data\WGTS\D072409";   2007 only */

LIBNAME FYPUF "C:\AHRQPUF\HCPUFS\HC&FYNUM\DATA";

OPTIONS FMTSEARCH=(FMT.FORMATS FMT.TC&YY._USEFMT);

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

*--------------------------------------------------------------------------------------
* Get PMEDs for positive weight persons.
*--------------------------------------------------------------------------------------;
DATA PMEDS;
  SET RXPUF.&RXPUF (KEEP=PERSID TC1 RXXP&YY.X PERWT&YY.F VARSTR VARPSU READ=&PWD);
  IF PERWT&YY.F > 0; 
                   
  LABEL TCNAME = "TC1 name";
         
  LENGTH TCNAME $45.;
   
  TCNAME=PUT(TC1,TCNAME.);
   
RUN;

PROC FREQ DATA=PMEDS;
  TABLES TCNAME / MISSING;
  TABLES TC1*TCNAME / LIST MISSING;
  FORMAT TC1 TCNAME.;
  TITLE3 "Frequency of therapeutic class variables";
  TITLE4 "Verification of creating TCNAME";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

PROC SUMMARY DATA=PMEDS NWAY MISSING;
  VAR RXXP&YY.X;
  WEIGHT PERWT&YY.F;
  OUTPUT OUT=TOTRX (DROP=_TYPE_) SUMWGT=TOTUSE SUM=TOTEXP;
RUN;
PROC PRINT DATA=TOTRX NOOBS SPLIT="*";
  SUM _FREQ_ TOTUSE TOTEXP;
  LABEL _FREQ_ = "# of Rx*Unweighted"
        TOTUSE = "# of Rx*Weighted"
        TOTEXP = "Total Exp.*Weighted";
  FORMAT _FREQ_ COMMA8.;
  FORMAT TOTUSE TOTEXP COMMA20.2;
  TITLE3 "QC check on total Rx utilization and expenditures";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

*--------------------------------------------------------------------------------------------
* Determine RANK for non-missing TC1 by weighted Rx utilization.
*--------------------------------------------------------------------------------------------;     
DATA HASTC;
  SET PMEDS;
  IF TC1 NOT IN (-1, -9);
RUN;      

PROC SUMMARY DATA=HASTC NWAY;
  CLASS TC1;
  ID TCNAME;
  VAR PERWT&YY.F;
  OUTPUT OUT=TCFREQ (DROP=_TYPE_) SUM=RXUSESUM;
RUN;

PROC SORT DATA=TCFREQ; BY DESCENDING RXUSESUM; RUN;

PROC PRINT DATA=TCFREQ;
  SUM _FREQ_ RXUSESUM;
  FORMAT _FREQ_ COMMA8.;
  FORMAT RXUSESUM COMMA20.;
  TITLE3 "WORK.TCFREQ: Sum of Rx utilization for non-missing TC1, sorted in descending order";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

DATA TCRANK;
  SET TCFREQ;
  LENGTH RANK 3.;
  
  LABEL RXUSESUM = "Weighted*Utilization"
        RANK     = "TC rank by utilization";        

  IF _N_ LE 10 THEN RANK=_N_; ELSE RANK=11;
     
RUN;

PROC SORT DATA=TCRANK; BY RANK; RUN;

PROC PRINT DATA=TCRANK;
  SUM _FREQ_ RXUSESUM;
  FORMAT _FREQ_ COMMA8.;
  FORMAT RXUSESUM COMMA20.;
  TITLE3 "WORK.TCRANK: Sum of Rx utilization for non-missing TC1, sorted in descending order";
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

*---------------------------------------------------------------------------------------------
* Create format, RANKFMT, for RANK using TCNAME.
*---------------------------------------------------------------------------------------------;
DATA FMTTC;
  SET TCRANK;
  BY RANK;
  IF FIRST.RANK;
RUN;

DATA FMTTC (KEEP=FMTNAME START LABEL TYPE);
  SET FMTTC END=EOF;
   
  LENGTH FMTNAME $8 START $2 LABEL $45;
  
  FMTNAME="RANKFMT";
  START=PUT(RANK,2.);
  NUMLABEL=TRIM(PUT(RANK,8.)); 
  TYPE="N";
              
  IF RANK NE 11 THEN DO;   
     LABEL=NUMLABEL || "    " || TCNAME;
     LABEL=LEFT(RIGHT(LABEL));
     OUTPUT;
  END; ELSE
  IF RANK=11 THEN DO;
     LABEL=NUMLABEL || "    ALL OTHERS";
     LABEL=LEFT(RIGHT(LABEL));
     OUTPUT;
  END;
 
  IF EOF THEN DO; /* Format for the total line in SUDAAN */
     START="99";
     LABEL="TOTAL";
     LABEL=LEFT(RIGHT(LABEL));
     OUTPUT;
  END; 
RUN;

PROC SORT DATA=FMTTC; BY START; RUN;

PROC PRINT DATA=FMTTC;
  TITLE3 "Verification of creating formats for variable RANK";
  TITLE5 "WORK.FMTTC: Input Control Dataset used to create permanent format for RANK";
  FOOTNOTE1;
RUN;

PROC FORMAT LIBRARY=FMT.TC&YY._USEFMT CNTLIN=FMTTC FMTLIB; 
  TITLE3 "Contents of permanent format library for RANK";
  FOOTNOTE1 "C:\AHRQ3\F_DSO\FD001NKK\DATA\FMTLIB\TC&YY._USEFMT.SAS7BCAT";
RUN;

*-----------------------------------------------------------------------------------
* Merge RANK onto PMEDs by TC1.
*-----------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS;  BY TC1; RUN;
PROC SORT DATA=TCRANK; BY TC1; RUN;       

DATA PMEDS;
  MERGE PMEDS  (IN=A)
        TCRANK (IN=B KEEP=TC1 RANK);
  BY TC1;
  IF A;
   
  IF TC1 IN (-1,-9) THEN RANK=11;

RUN;  

PROC FREQ DATA=PMEDS;
  TABLES RANK*TC1 / LIST MISSING;
  FORMAT RANK RANKFMT.;
  FORMAT TC1 TCNAME.;
  TITLE3 "Verification of RANK by TC1"; 
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

PROC FREQ DATA=PMEDS;
  TABLES RANK / MISSING;
  WEIGHT PERWT&YY.F;
  FORMAT RANK RANKFMT.;
  TITLE3 "Weighted frequency of RANK"; 
  FOOTNOTE1 "Source file: AHRQ3\F_DSO\FD001NKK\DATA\&RXPUF..SAS7BDAT";
  FOOTNOTE2 "PERWT&YY.F > 0";
RUN;

*--------------------------------------------------------------------------------------
* Create dummy variable to identify unique persons in RANK.
*--------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS; BY RANK PERSID; RUN;

DATA PMEDS;
  SET PMEDS;
  BY RANK PERSID;
  
  IF FIRST.PERSID THEN RANKPERS=1; ELSE RANKPERS=0;
  
RUN;

*--------------------------------------------------------------------------------------
* Bring in all persons from FY PUF for SUDAAN.
* Flag persons with PMEDs.
*--------------------------------------------------------------------------------------;
PROC SORT DATA=PMEDS; BY PERSID; RUN;
                                        
DATA PMEDS;
  MERGE FYPERS (IN=INFY)
        PMEDS  (IN=INRX)
  END=EOF;
  BY PERSID;
  IF INFY;
  
  LABEL HASPMED = "1 if person has PMED event";
   
  HASPMED=INRX;
  
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

*===============================================================================
* SUDAAN.
*===============================================================================;
PROC SORT DATA=PMEDS; BY VARSTR VARPSU; RUN;

FOOTNOTE1;
TITLE3;
PROC DESCRIPT DATA=PMEDS FILETYPE=SAS NOPRINT;
  SUBPOPN HASPMED=1;
  NEST VARSTR VARPSU;
  WEIGHT PERWT&YY.F;
  VAR _ONE_ RXXP&YY.X RANKPERS;
  SUBGROUPS RANK;
  LEVEL     11;
  OUTPUT NSUM WSUM TOTAL SETOTAL / FILENAME=SESTATS FILETYPE=SAS REPLACE;
RUN;

*----------------------------------------------------------------------------------
* Reformat SUDAAN output for printing.
*----------------------------------------------------------------------------------;
DATA USE (RENAME=(TOTAL=TOT_USE SETOTAL=SE_USE))
     EXP (RENAME=(TOTAL=TOT_EXP SETOTAL=SE_EXP))
     PER (RENAME=(TOTAL=TOT_PER SETOTAL=SE_PER));
  SET SESTATS;
  
  IF RANK=0 THEN RANK=99;  /* total line */
  
  IF VARIABLE=1 THEN OUTPUT USE; ELSE
  IF VARIABLE=2 THEN OUTPUT EXP; ELSE
  IF VARIABLE=3 THEN OUTPUT PER;
  
  DROP PROCNUM TABLENO VARIABLE _C1;
RUN;
  
PROC SORT DATA=USE; BY RANK; RUN;
PROC SORT DATA=EXP; BY RANK; RUN;
PROC SORT DATA=PER; BY RANK; RUN;

DATA TCTAB.TOP10TC&YY._USE;
  MERGE USE
        EXP
        PER;
  BY RANK;
  LABEL RANK    = "Therapeutic Class Rank & Name"
        NSUM    = "Drug Mentions, Unweighted"
        TOT_USE = "Drug Mentions, Weighted"
         SE_USE = "Drug Mentions SE"
        TOT_EXP = "Dollars Spent, Weighted"
         SE_EXP = "Dollars Spent SE"
        TOT_PER = "Count of Persons, Weighted"
         SE_PER = "Count of Persons SE";
RUN; 

PROC PRINT DATA=TCTAB.TOP10TC&YY._USE NOOBS SPLIT="*";
  ID RANK;
  VAR NSUM
      TOT_USE SE_USE
      TOT_EXP SE_EXP
      TOT_PER SE_PER;
   LABEL RANK    = "Therapeputic Class*Rank & Name"
         NSUM    = "Drug*Mentions*Unweighted"
         TOT_USE = "Drug*Mentions*Weighted"
          SE_USE = "Drug*Mentions*SE"
         TOT_EXP = "Dollars*Spent*Weighted"
          SE_EXP = "Dollars*Spent*SE"
         TOT_PER = "Count of*Persons*Weighted"
          SE_PER = "Count of*Persons*SE";
  FORMAT RANK RANKFMT.;
  FORMAT NSUM COMMA8.;
  FORMAT TOT_USE TOT_EXP TOT_PER COMMA20.; 
  FORMAT  SE_USE  SE_EXP  SE_PER COMMA20.2;
  TITLE3 "Utilization and Expenditures for the Top 10 Therapeutic Classes for Prescribed Drugs";
  TITLE5 "Ranked by Weighted Utilization";
  TITLE7 "SUDAAN Output";     
RUN;

*---------------------------------------------------------------------------------------------
* Compute z-scores.
*---------------------------------------------------------------------------------------------;
PROC SORT DATA=TCTAB.TOP10TC&YY._USE (KEEP=RANK TOT_USE SE_USE WHERE=(1 LE RANK LE 11))
                 OUT=SETABLE;
  BY RANK;
RUN;

/* Put the stats for each observation into arrays */
DATA SIGSTAT;
  SET SETABLE END=EOF;
  BY RANK;

  RETAIN RANK1-RANK11
         STAT1-STAT11
         SE1-SE11;

  ARRAY RNK(11)  RANK1-RANK11;
  ARRAY STAT(11) STAT1-STAT11;
  ARRAY SE(11)   SE1-SE11;
  
  IF _N_=1 THEN NSTAT=0;

  NSTAT+1;
  RNK(NSTAT)=NSTAT;
  STAT(NSTAT)=TOT_USE;
  SE(NSTAT)=SE_USE;

  DROP RANK TOT_USE SE_USE;
   
  IF EOF THEN OUTPUT;
RUN;

PROC PRINT DATA=SIGSTAT;
  TITLE3 "QC testing for Z-Score computation after collapsing stats into an arrays";
RUN;

/* Compute Z-Scores */
DATA ZSCORE (KEEP=TC1 EST1 EST1SE
                  TC2 EST2 EST2SE
                  STATDIFF SESQRT ZSTAT PVALUE);
  SET SIGSTAT;
  
  ARRAY CRANK(11) RANK1-RANK11;
  ARRAY CSTAT(11) STAT1-STAT11;
  ARRAY CSE(11)   SE1-SE11;

  DO M=1 TO NSTAT-1;
     DO J=1 TO (NSTAT-M);
        TC1=CRANK(M);
        EST1=CSTAT(M);
        EST1SE=CSE(M);

        TC2=CRANK(M+J);
        EST2=CSTAT(M+J);
        EST2SE=CSE(M+J);

        STATDIFF=EST1-EST2;
        SESQRT=SQRT((EST1SE)**2 + (EST2SE)**2);
        ZSTAT=ABS(STATDIFF / SESQRT);
        PVALUE=PDF("NORMAL",ZSTAT);

        OUTPUT;
     END;
  END;

RUN;

PROC PRINT DATA=ZSCORE NOOBS SPLIT="*";
  BY TC1;
  ID TC1 TC2;
  VAR EST1 EST2 EST1SE EST2SE /*STATDIFF SESQRT*/ ZSTAT PVALUE;
  FORMAT TC1 TC2 RANKFMT.;
  FORMAT EST1 EST2 COMMA18.2;
  FORMAT EST1SE EST2SE COMMA18.2;
  FORMAT ZSTAT 6.3;
  FORMAT PVALUE PVALUE6.3;     
  LABEL TC1    = "RANK/TC #1"
        TC2    = "RANK/TC #2"
        EST1   = "Estimate #1"
        EST2   = "Estimate #2"
        EST1SE = "SE #1"
        EST2SE = "SE #2"
        ZSTAT    = "Z-SCORE"
        PVALUE   = "P-VALUE";
  TITLE3 "Z Scores to Compare Top 10 Therapeutic Classes when Ranked by Weighted Utilization";
RUN;

%JOBEND;

