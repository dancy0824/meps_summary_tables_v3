/***********************************************************************************************************************
Task Number :    AH4.FF004  
                                                                                                                      
Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\QC_PCTPOP_SOP.sas 
                                                                                               
Description :   QC the calcuated coefficent and standard error estimates for percent (%) of population
								with an expense (PCTPOP), by:
								(A) SOURCE OF PAYMENT
								
                 Compare to the R Shiny Server estimates
                                                                                                                                                                                                 
Input Data:	  \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614v2.sas7bdat
							\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4\fyc9614.sas7bdat
                                                  
Programmer  :   Jodi Cisewski                                                                                                                                     

Date        :    June 2, 2017
************************************************************************************************************************/
OPTIONS LS=200 PS=60 OBS=MAX;

libname SAS "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" access=readonly;
libname Rest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" access=readonly;  

TITLE1 "AH4.FF004:  MEPS-HC: Utilization and Expenditures, 1996-2014";

%LET SOP = SOP1 SOP2 SOP3 SOP4 SOP5;

%MACRO YY (YEAR=);

TITLE3 "YEAR=&YEAR";
DATA FY&YEAR;
  SET sas.PERSDATA9614V2 (WHERE=(YEAR=&YEAR));  
  TOTAL=1;
  RUN ;


ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR NOMCAR mean stderr ;
VAR &SOP;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN IND;
ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;

DATA OUT&year (KEEP=YEAR grp2 MEAN STDERR levels2 WHERE=(grp2 NE " "));
  SET OUT1;
   LENGTH grp2 $15  LEVELS2 $32;
    IF VARNAME EQ "SOP1" THEN DO; grp2='sop'; LEVELS2='SLF'; end;
    ELSE IF VARNAME EQ "SOP2" THEN DO; grp2='sop'; LEVELS2='PTR'; end;
    ELSE IF VARNAME EQ "SOP3" THEN DO; grp2='sop'; LEVELS2='MCR'; end;
    ELSE IF VARNAME EQ "SOP4" THEN DO; grp2='sop'; LEVELS2='MCD'; end;
    ELSE IF VARNAME EQ "SOP5" THEN DO; grp2='sop'; LEVELS2='OTZ'; end;

   YEAR=&YEAR;
RUN;
    
 %MEND YY;
 %YY (YEAR=2014)
 %YY (YEAR=2013)
 %YY (YEAR=2012)
 %YY (YEAR=2011)
 %YY (YEAR=2010)
 %YY (YEAR=2009)
 %YY (YEAR=2008)
 %YY (YEAR=2007)
 %YY (YEAR=2006)
 %YY (YEAR=2005)
 %YY (YEAR=2004)
 %YY (YEAR=2003)
 %YY (YEAR=2002)
 %YY (YEAR=2001)
 %YY (YEAR=2000)
 %YY (YEAR=1999)
 %YY (YEAR=1998)
 %YY (YEAR=1997)
 %YY (YEAR=1996);
 run;
 
 data result;
   set 
   out2014
   out2013
   out2012
   out2011
   out2010
   out2009
   out2008
   out2007
   out2006
   out2005
   out2004
   out2003
   out2002
   out2001
   out2000
   out1999
   out1998
   out1997
   out1996;
   
 run;
 
 
PROC SORT DATA=Rest.fyc9614 OUT=RShiny(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 PCTEXP PCTEXP_SE YEAR);
BY  GRP2 LEVELS2 YEAR;
RUN;

DATA RShiny2;
SET RShiny;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
MEAN_R=ROUND(PCTEXP,0.001);
SE_R=ROUND(PCTEXP_SE,0.001);
RUN;


DATA RESULT2;
SET RESULT;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
MEAN_SAS=ROUND(MEAN,0.001);
SE_SAS=ROUND(STDERR,0.001);

RUN;

PROC SORT DATA=RESULT2;
BY GRP3 LEVELS3 YEAR ;
RUN;


DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 MEAN_SAS SE_SAS) RShiny2 (IN=B KEEP=YEAR GRP3 LEVELS3  MEAN_R SE_R);
BY GRP3 LEVELS3 year;
IF A;
DIFF_MEAN=SUM(MEAN_R,-MEAN_SAS);
DIFF_SE=SUM(SE_R,-SE_SAS);
RUN;
 
TITLE2 "Differences in Values Comparing SAS and R Estimates";  
TITLE3 "% of Population with an Expense by Event Type, 1996-2014";
PROC PRINT DATA=ALL;
VAR YEAR GRP3 LEVELS3 MEAN_R MEAN_SAS DIFF_MEAN SE_R SE_SAS DIFF_SE;
WHERE DIFF_MEAN NE 0 OR DIFF_SE NE 0;
RUN;

