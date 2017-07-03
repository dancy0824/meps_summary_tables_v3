/***********************************************************************************************************************
Task Number :    AH4.FF004  
                                                                                                                      
Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\QC_PCTPOP.sas 
                                                                                               
Description :   QC the calcuated coefficent and standard error estimates for percent (%) of population
								with an expense (PCTPOP), by:
								(A) Demographic Variables
								(B) Health Variables
								(C) Socio-Economic Status Variables	
                                                                                                                                                                                                 
Input Data:	  \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614v2.sas7bdat
							\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4\std_fyc9614.sas7bdat
                                                  
Programmer  :   Jodi A Cisewski                                                                                                                                     

Date        :    May 31, 2017
************************************************************************************************************************/
OPTIONS LS=200 PS=60 OBS=MAX;

libname SAS "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" access=readonly;
libname Rest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" access=readonly;  

%let demo = AGEGRPS AGEGRPS_V2 REGION MARRIED RACE SEX INSURANCE INSURANCE_V2 HEALTH MENTAL_HEALTH EDUCATION EMPLOYED POVERTY;


*%include "\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Jodi\QC\formats.sas";

PROC FORMAT;
VALUE AGEGRPS
1='UNDER 5'
2='5-17'
3='18-44'
4='45-64'
5='65+'
;
VALUE AGEGRPS_V2X
1='UNDER 18'
2='18-64'
3='65+'
;

VALUE MARRIED
.='. MISSING'
1='MARRIED'
2='WIDOWED'
3='DIVORCED'
4='SEPARATED'
5='NEVER MARRIED'
6='Inapplicable (age < 16)'
;

VALUE INSURANCE
1='ANY PRIVATE'
2='PUBLIC ONLY'
3='UNINSURED'
;

VALUE HEALTH
.=". MISSING" 
1='EXCELLENT' 
2='VERY GOOD' 
3='GOOD' 
4='FAIR' 
5='POOR'
;

VALUE MENTAL_HEALTH
.=". MISSING"
1='EXCELLENT'
2='VERY GOOD'
3='GOOD'
4='FAIR'
5='POOR'
;

VALUE EMPLOYED
.='. MISSING'
-1='Inapplicable (age < 16)'
1='EMPLOYED'
2='NOT EMPLOYED'
;

VALUE RACE
1='HISPANIC'
2='WHITE'
3='BLACK'
4='American Indian, Alaska Native,'
5='Asian, Hawaiian, or Pacific Isla'
9='WHITE AND OTHER'
;

VALUE EDUCATION
.='. MISSING'
1='LESS THAN HIGH SCHOOL'
2='HIGH SCHOOL'
3='SOME COLLEGE'
4='Inapplicable (age < 18)'
;

VALUE insurance_v2X
1='<65, Any private'
2='<65, Public only'
3='<65, UNINSURED'
4='65+, MEDICARE ONLY'
5='65+, Medicare and private'
6='65+, Medicare and other public'
7='65+, NO MEDICARE'
;

VALUE POVERTY
1='Negative or Poor'
2='Near-poor'
3='LOW INCOME'
4='MIDDLE INCOME'
5='HIGH INCOME'
;

 VALUE SEX
    1='Male'
    2='Female';
    
   VALUE REGION
    1='NORTHEAST'
    2='MIDWEST'
    3='SOUTH'
    4='WEST';
    
    VALUE IND
    1='Total';
run;

%MACRO YY (YEAR=);


DATA FY&YEAR (KEEP = TESTVAR IND VARPSU VARSTR PERWTF YEAR TOTAL &demo);
  SET sas.PERSDATA9614V2 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
 /*create dummy variable for people who have an expense*/
  IF TOTEXP > 0 THEN TESTVAR=1;
  ELSE TESTVAR=0;
  RUN ;



ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR NOMCAR mean stderr ;
VAR TESTVAR;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN TOTAL &demo;
ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;

DATA OUT&year (KEEP=YEAR grp2 MEAN STDERR levels2 WHERE=(grp2 NE " "));
  SET OUT1;
   LENGTH grp2 $15  LEVELS2 $255;
    IF TOTAL=1 THEN do; grp2='ind'; levels2='Total'; end;
    ELSE IF AGEGRPS>0 THEN DO; grp2='AGEGRPS'; LEVELS2=PUT(AGEGRPS, AGEGRPS.);END;
    ELSE IF AGEGRPS_V2>0 THEN DO; grp2='AGEGRPS_V2';LEVELS2=PUT(AGEGRPS_V2, AGEGRPS_V2x.);END;
    ELSE IF REGION>0 THEN DO; grp2='REGION'; LEVELS2=PUT(REGION, REGION.);END;
    ELSE IF MARRIED>0 OR MARRIED=-1 THEN DO;grp2='MARRIED'; LEVELS2=PUT(MARRIED, MARRIED.);END;
    ELSE IF RACE>0 THEN DO;grp2='RACE' ; LEVELS2=PUT(RACE, RACE.);END;
    ELSE IF SEX>0 THEN DO; grp2='Sex' ; LEVELS2=PUT(SEX, SEX.);END;
    ELSE IF INSURANCE>0 THEN DO; grp2='INSURANCE' ; LEVELS2=PUT(INSURANCE, INSURANCE.);END;
    ELSE IF INSURANCE_V2>0 THEN DO; grp2='INSURANCE_V2' ; LEVELS2=PUT(INSURANCE_V2, INSURANCE_V2x.);END;
    ELSE IF HEALTH>0 THEN DO; grp2='HEALTH' ; LEVELS2=PUT(HEALTH, HEALTH.);END;
    ELSE IF MENTAL_HEALTH>0 THEN DO; grp2='MENTAL_HEALTH' ; LEVELS2=PUT(MENTAL_HEALTH, MENTAL_HEALTH.);END;
    ELSE IF EDUCATION>0 OR EDUCATION=-1 THEN DO; grp2='EDUCATION' ; LEVELS2=PUT(EDUCATION, EDUCATION.);END;
    ELSE IF EMPLOYED>0 OR EMPLOYED=-1 THEN DO; grp2='EMPLOYED' ; LEVELS2=PUT(EMPLOYED, EMPLOYED.);END;
    ELSE IF POVERTY>0 THEN DO; grp2='POVERTY' ; LEVELS2=PUT(POVERTY, POVERTY.);END;
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
 
DATA RShiny2 (KEEP=YEAR GRP3 LEVELS3 MEAN_R SE_R);
SET REST.fyc9614 (WHERE=(GRP1='ind' and LEVELS1='Total')) ;
LENGTH grp3 $15  LEVELS3 $255;  
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
MEAN_R=ROUND(PCTEXP, 0.001);
SE_R=ROUND(PCTEXP_SE, 0.001);

IF GRP3 IN ('sop', 'event')  or LEVELS3='missing' THEN DELETE;
RUN;

DATA RESULT2;
SET RESULT;
LENGTH grp3 $15  LEVELS3 $255; 

  IF GRP2='AGEGRPS_V2' THEN GRP2='AGEGRPS' ;
  IF GRP2='INSURANCE_V2' THEN GRP2='INSURANCE' ; 
  
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
MEAN_SAS=ROUND(MEAN,0.001);
SE_SAS=ROUND(STDERR,0.001);


RUN;

PROC SORT DATA=RESULT2 NODUPKEY;
BY GRP3 LEVELS3 YEAR ;
RUN;

PROC SORT DATA=RSHINY2;
BY GRP3 LEVELS3 YEAR ;
RUN;

DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 MEAN_SAS SE_SAS) RShiny2 (IN=B KEEP=YEAR GRP3 LEVELS3  MEAN_R SE_R);
BY GRP3 LEVELS3 year;
*IF A;
DIFF_MEAN= MEAN_R - MEAN_SAS;
DIFF_SE=SE_R - SE_SAS;
RUN;

PROC FREQ DATA=ALL;
TABLES DIFF:/LIST MISSING;
RUN;
 
 
TITLE2 "Differences in Values Comparing SAS and R Estimates";
TITLE3 "% of Population with an Expense by demographic/health/socio-economic variables, 1996-2014";
PROC PRINT DATA=ALL;
VAR YEAR GRP3 LEVELS3 MEAN_R MEAN_SAS DIFF_MEAN SE_R SE_SAS DIFF_SE;
WHERE DIFF_MEAN NE 0 OR DIFF_SE NE 0;
RUN;

