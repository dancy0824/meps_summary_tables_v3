/********************************************************************************************************************************************************
Task Number :    AH4.FF004  
                                                                                                                      
Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\QC_PCTPOP_Cross4.sas 
                                                                                               
Description :   QC the calculated coefficent and standard error estimates for percent (%) of population
								with an expense (PCTPOP), Cross Sectional Tables:
								(1) Source of Payment x Event Type:
										(a) SOP Variables:  Out of Pocket, Private, Medicare, Medicaid, Other
										(b) Event Type Variables:  Dental Visitis, Prescription Medicines, Office-based events, physician office visits,
												non-physician office visits, outpatient events, physician hosp visits, non-phy hospital visits, ER visits, inpatient stays
												home health events, HH agency provider, HH independent provider, other medical expenses
								
								Compare to the R Shiny Server estimates
                                                                                                                                                                                            
Input Data:	  \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614V2.sas7bdat
							\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4\fyc9614.sas7bdat  
                                                  
Programmer  :   Jodi Cisewski                                                                                                                                     

Date        :   June 5, 2017
****************************************************************************************************************************************************/
OPTIONS LS=200 PS=60 OBS=MAX;

libname SAS "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" access=readonly;
libname Rest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" access=readonly;  /*Using V2 R estimates*/


%LET VARS = SOP1DV SOP1RX SOP1OB SOP1OB_DR SOP1OB_OT SOP1OP SOP1OP_DR SOP1OP_OT SOP1ER SOP1IP SOP1HH SOP1HH_A SOP1HH_N SOP1OT 
						SOP2DV SOP2RX SOP2OB SOP2OB_DR SOP2OB_OT SOP2OP SOP2OP_DR SOP2OP_OT SOP2ER SOP2IP SOP2HH SOP2HH_A SOP2HH_N SOP2OT 
						SOP3DV SOP3RX SOP3OB SOP3OB_DR SOP3OB_OT SOP3OP SOP3OP_DR SOP3OP_OT SOP3ER SOP3IP SOP3HH SOP3HH_A SOP3HH_N SOP3OT 
						SOP4DV SOP4RX SOP4OB SOP4OB_DR SOP4OB_OT SOP4OP SOP4OP_DR SOP4OP_OT SOP4ER SOP4IP SOP4HH SOP4HH_A SOP4HH_N SOP4OT 
						SOP5DV SOP5RX SOP5OB SOP5OB_DR SOP5OB_OT SOP5OP SOP5OP_DR SOP5OP_OT SOP5ER SOP5IP SOP5HH SOP5HH_A SOP5HH_N SOP5OT;


%MACRO YY (YEAR=);

DATA FY&YEAR (KEEP = IND VARPSU VARSTR PERWTF YEAR TOTAL &vars);
  SET SAS.PERSDATA9614v2 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
RUN;


ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR NOMCAR mean stderr;
VAR &vars;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN IND;
ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;

DATA OUT&year;
  SET OUT1;
  LENGTH grp1 grp2 $15  levels1 LEVELS2 $32;
 %include "\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Jodi\QC\cross4_rename.sas";
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
 
 
 data comb;
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
 

PROC SORT DATA=Rest.fyc9614 OUT=RShiny(WHERE=(GRP1='sop' and GRP2='event' /*and levels1 ne 'EXP' and levels2 ne 'TOT'*/)  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 PCTEXP PCTEXP_SE YEAR); 
BY  GRP1 GRP2 LEVELS1 LEVELS2 YEAR;
RUN;

DATA RShiny2;
SET RShiny;
MEAN_R=ROUND(PCTEXP,0.001);
SE_R=ROUND(PCTEXP_SE,0.001);
RUN;


DATA RESULT2;
SET COMB;
MEAN_SAS=ROUND(MEAN,0.001);
SE_SAS=ROUND(STDERR,0.001);
RUN;


PROC SORT DATA=RESULT2;
BY GRP1 GRP2 LEVELS1 LEVELS2 YEAR ;
RUN;

PROC SORT DATA=RShiny2;
BY GRP1 GRP2 LEVELS1 LEVELS2 YEAR ;
RUN;



DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP1 GRP2 LEVELS1 LEVELS2 MEAN_SAS SE_SAS) RShiny2 (IN=B KEEP=YEAR GRP1 GRP2 LEVELS1 LEVELS2 MEAN_R SE_R);
BY GRP1 GRP2 LEVELS1 LEVELS2 year;
IF A;
DIFF_MEAN=SUM(MEAN_R,-MEAN_SAS);
DIFF_SE=SUM(SE_R,-SE_SAS);
RUN;
 
 
TITLE2 "% of Population with an Expense: SOP x Event Type, 1996-2014";
PROC PRINT DATA=ALL;
VAR YEAR GRP1 GRP2 LEVELS1 LEVELS2 MEAN_R MEAN_SAS DIFF_MEAN SE_R SE_SAS DIFF_SE;
where DIFF_MEAN ne 0 or DIFF_SE ne 0;
RUN;	