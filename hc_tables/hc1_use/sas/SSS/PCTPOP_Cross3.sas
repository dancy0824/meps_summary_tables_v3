/**********************************************************************************************************************************************************
Task Number :    AH4.FF004  
                                                                                                                      
Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\QC_PCTPOP_Cross3.sas 
                                                                                               
Description :   QC the calculated coefficent and standard error estimates for percent (%) of population
								with an expense (PCTPOP), Cross Sectional Tables:
								(1) Demographics x Event Type:
										(a) Demographic Variables: Age Groups, Census Regions, Marital Status, Race/Ethnicity, Sex
										(b) Event Type Variables:  Dental Visitis, Prescription Medicines, Office-based events, physician office visits,
												non-physician office visits, outpatient events, physician hosp visits, non-phy hospital visits, ER visits, inpatient stays
												home health events, HH agency provider, HH independent provider, other medical expenses
								
								Compare to the R Shiny Server Estimates
                                                                                                                                                                                            
Input Data:	  \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614V2.sas7bdat  
							\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4\std_fyc9614.sas7bdat
                                                  
Programmer  :   Jodi Cisewski                                                                                                                                     

Date        :    June 5, 2017
**********************************************************************************************************************************************************/
OPTIONS LS=200 PS=60 OBS=MAX;

libname SAS "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" access=readonly;
libname Rest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" access=readonly;  /*Using V2 R estimates*/

TITLE1 "AH4.FF004:  MEPS-HC: Utilization and Expenditures, 1996-2014";

%let DEMO = AGEGRPS AGEGRPS_V2 REGION MARRIED RACE SEX;
%let eventype = DVPOS RXPOS OBPOS OBPOS_DR OBPOS_OT OPPOS OPPOS_DR OPPOS_OT ERPOS IPPOS HHPOS HHPOS_A HHPOS_N OTPOS;

%include "\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Jodi\QC\formats.sas";



%MACRO YY (YEAR=);

DATA FY&YEAR (KEEP = IND VARPSU VARSTR PERWTF YEAR TOTAL &demo &eventype);
  SET SAS.PERSDATA9614V2 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
RUN;


ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR NOMCAR mean stderr;
VAR &eventype;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN &DEMO;
ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;



DATA OUT&YEAR (KEEP=YEAR grp1 grp2 MEAN STDERR levels1 levels2 WHERE=(grp2 NE " "));
  SET OUT1; 
  LENGTH grp1 grp2 $15  levels1 LEVELS2 $255;

IF VARNAME EQ "DVPOS" THEN DO; grp2='event'; LEVELS2='DVT'; end;
    ELSE IF VARNAME EQ "RXPOS" THEN DO; grp2='event'; LEVELS2='RX'; end;
    ELSE IF VARNAME EQ "OBPOS" THEN DO; grp2='event'; LEVELS2='OBV'; end;
    ELSE IF VARNAME EQ "OBPOS_DR" THEN DO; grp2='event'; LEVELS2='OBD'; end;
    ELSE IF VARNAME EQ "OBPOS_OT" THEN DO; grp2='event'; LEVELS2='OBO'; end;
    ELSE IF VARNAME EQ "OPPOS" THEN DO; grp2='event'; LEVELS2='OPT'; end;
    ELSE IF VARNAME EQ "OPPOS_DR" THEN DO; grp2='event'; LEVELS2='OPY'; end;
    ELSE IF VARNAME EQ "OPPOS_OT" THEN DO; grp2='event'; LEVELS2='OPZ'; end;
    ELSE IF VARNAME EQ "ERPOS" THEN DO; grp2='event'; LEVELS2='ERT'; end;
    ELSE IF VARNAME EQ "IPPOS" THEN DO; grp2='event'; LEVELS2='IPT'; end;
    ELSE IF VARNAME EQ "HHPOS" THEN DO; grp2='event'; LEVELS2='HHT'; end;
    ELSE IF VARNAME EQ "HHPOS_A" THEN DO; grp2='event'; LEVELS2='HHA'; end;
    ELSE IF VARNAME EQ "HHPOS_N" THEN DO; grp2='event'; LEVELS2='HHN'; end;
    ELSE IF VARNAME EQ "OTPOS" THEN DO; grp2='event'; LEVELS2='OMA'; end;
  
  IF AGEGRPS>0 THEN DO; grp1='AGEGRPS'; LEVELS1=PUT(AGEGRPS, AGEGRPS.);END;
    ELSE IF AGEGRPS_V2>0 THEN DO; grp1='AGEGRPS_V2';LEVELS1=PUT(AGEGRPS_V2, AGEGRPS_V2x.);END;
    ELSE IF REGION>0 THEN DO; grp1='REGION'; LEVELS1=PUT(REGION, REGION.);END;
    ELSE IF MARRIED>0 OR MARRIED=-1 THEN DO;grp1='MARRIED'; LEVELS1=PUT(MARRIED, MARRIED.);END;
    ELSE IF RACE>0 THEN DO;grp1='RACE' ; LEVELS1=PUT(RACE, RACE.);END;
    ELSE IF SEX>0 THEN DO; grp1='Sex' ; LEVELS1=PUT(SEX, SEX.);END;  
    
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
 
 
PROC SORT DATA=Rest.std_fyc9614 OUT=RShiny(WHERE=(GRP2='EVENT')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 PCTEXP PCTEXP_SE YEAR);
BY  GRP1 GRP2 LEVELS1 LEVELS2 YEAR;
RUN;


DATA RShiny2;
SET RShiny;
LENGTH grp3 grp4 $15  LEVELS3 LEVELS4 $255; 
GRP3=LOWCASE(COMPRESS(grp1));
LEVELS3=LOWCASE(COMPRESS(levels1));
GRP4=LOWCASE(COMPRESS(GRP2));
LEVELS4=LOWCASE(COMPRESS(LEVELS2));
MEAN_R=PCTEXP;
SE_R=PCTEXP_SE;
RUN;


DATA RESULT2;
SET COMB;
LENGTH grp3 grp4 $15  LEVELS3 LEVELS4 $255;
GRP3=LOWCASE(COMPRESS(grp1));
LEVELS3=LOWCASE(COMPRESS(levels1));
GRP4=LOWCASE(COMPRESS(GRP2));
LEVELS4=LOWCASE(COMPRESS(LEVELS2));
MEAN_SAS=ROUND(MEAN,0.001);
SE_SAS=ROUND(STDERR,0.001);
IF grp3= "agegrps_v2" and LEVELS3="65+" THEN DELETE; *these values will be the same as when grp2=AGEGRPS and levels2="+65";
RUN;


PROC SORT DATA=RESULT2;
BY GRP3 GRP4 LEVELS3 LEVELS4 YEAR ;
RUN;

PROC SORT DATA=RShiny2;
BY GRP3 GRP4 LEVELS3 LEVELS4 YEAR ;
RUN;


DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 GRP4 LEVELS3 LEVELS4 MEAN_SAS SE_SAS) RShiny2 (IN=B KEEP=YEAR GRP3 GRP4 LEVELS3 LEVELS4 MEAN_R SE_R);
BY GRP3 GRP4 LEVELS3 LEVELS4 YEAR;
IF A;
DIFF_MEAN=SUM(MEAN_R,-MEAN_SAS);
DIFF_SE=SUM(SE_R,-SE_SAS);
RUN;
 
 
TITLE2 "% of Population with an Expense: EVENTS x Demo, 1996-2014";
PROC PRINT DATA=ALL;
VAR YEAR GRP3 GRP4 LEVELS3 LEVELS4 MEAN_R MEAN_SAS DIFF_MEAN SE_R SE_SAS DIFF_SE;
where DIFF_MEAN NE 0 OR DIFF_SE NE 0;
RUN;