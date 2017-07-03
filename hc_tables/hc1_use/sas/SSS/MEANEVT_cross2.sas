%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\MEANEVT_cross2.sas                                                                                                
 * Description :    Calculate cross section estimates - mean exp per event by demo*sop and compare them with V4 of R estimates for 1996-2014                                                                                                                                                                                                
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSDATA9614, EVTdata9614.sas7bdat  
 *                  \\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4\EVT9614
 * Output Data :    no                                                      
 * Programmer  :    Bidong Liu                                                                                                                                      
 * Date        :    5/31/2017
*********************************************************************;
OPTIONS LS=200 PS=60 OBS=MAX ;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";
libname V4R "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";

title1 "AH4.FF004";
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

PROC SORT DATA=TASK.PERSDATA9614 OUT=PERS;
  BY YEAR DUPERSID;
RUN;

PROC SORT DATA=TASK.EVNTDATA9614 (KEEP=YEAR DUPERSID SOP1EXP SOP2EXP SOP3EXP SOP4EXP SOP5EXP XP) OUT=EVNT;
  BY YEAR DUPERSID;
RUN;

DATA PUF;
  MERGE EVNT (IN=A) 
        PERS (IN=B KEEP=YEAR DUPERSID SEX VARSTR  VARPSU PERWTF  AGEGRPS  AGEGRPS_V2  REGION    MARRIED  
                         RACE INSURANCE   INSURANCE_V2  HEALTH  MENTAL_HEALTH EDUCATION   EMPLOYED     POVERTY );
  BY YEAR DUPERSID;
  IF A THEN SUB=1;
  ELSE IF NOT A THEN DO;
    SUB=2;
    SOP1EXP=0; 
    SOP2EXP=0; 
    SOP3EXP=0; 
    SOP4EXP=0; 
    SOP5EXP=0; 
    XP=0;
 END;
RUN;        

%MACRO YY (YEAR=);
TITLE3 "YEAR=&YEAR";

%macro xc (grp1=, f1=, N=);

DATA FY&YEAR;
  SET PUF (WHERE=(YEAR=&YEAR));
  IND=1;
  LEVELS1=&GRP1;
RUN;

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR MEAN STDERR missing;
VAR XP SOP1EXP SOP2EXP SOP3EXP SOP4EXP SOP5EXP;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN LEVELS1*SUB;
ODS OUTPUT domain=OUT;
RUN;
ODS LISTING;

DATA OUT&N (DROP=LEVELS1);
  SET OUT (WHERE=(SUB=1));
  LENGTH GRP1 GRP2 $20 L1 LEVELS2 $50;
  IF LEVELS1 NE .;
   
  GRP1="&GRP1";
  GRP2="SOP";
  YEAR=&YEAR;
  
   L1=PUT(LEVELS1, &f1..);
   SAS_EST=ROUND(MEAN, .1);
   SAS_SE=ROUND(STDERR, .1);
  
   IF VARNAME='XP' THEN LEVELS2='XP'; 
   IF VARNAME='SOP1EXP' THEN LEVELS2='SF';
   IF VARNAME='SOP2EXP' THEN LEVELS2='PR';
   IF VARNAME='SOP3EXP' THEN LEVELS2='MR'; 
   IF VARNAME='SOP4EXP' THEN LEVELS2='MD';
   IF VARNAME='SOP5EXP' THEN LEVELS2='OZ';
   
   IF LEVELS2 NE '';
RUN;

%mend xc;
%xc (grp1=AGEGRPS,    f1=AGEGRPS,    n=1)
%xc (grp1=AGEGRPS_V2, f1=AGEGRPS_V2X, n=2)
%xc (grp1=MARRIED,    f1=MARRIED,     N=3)
%XC (grp1=EMPLOYED,   f1=EMPLOYED,    n=4)
%xc (grp1=INSURANCE,  f1=INSURANCE,   n=5)
%xc (grp1=INSURANCE_V2, f1=INSURANCE_V2X, n=6)

%xc (grp1=REGION,    f1=REGION,    n=7)
%xc (grp1=RACE,      f1=RACE,      n=8)
%xc (grp1=HEALTH,    f1=HEALTH,    n=9)
%xc (grp1=MENTAL_HEALTH, f1=MENTAL_HEALTH,  N=10)
%xc (grp1=POVERTY,    f1=POVERTY,    n=11);
%xc (grp1=SEX,        f1=SEX,    n=12);
%xc (grp1=EDUCATION,  f1=EDUCATION, n=13);
%xc (grp1=IND,        f1=IND,    n=14);

RUN;

DATA ALL (RENAME=(L1=LEVELS1));
  SET OUT5 OUT1 OUT2 OUT3 OUT4 OUT6 OUT7 OUT8 OUT9 OUT10 OUT11 OUT12 OUT13 OUT14;
RUN;

DATA SAS;
  SET  ALL;
  GRP1=UPCASE(COMPRESS(GRP1));
  LEVELS1=UPCASE(COMPRESS(LEVELS1)); 
  IF GRP1='AGEGRPS_V2' THEN GRP1='AGEGRPS' ;
  IF GRP1='INSURANCE_V2' THEN GRP1='INSURANCE' ; 
RUN;

PROC SORT DATA=SAS NODUPKEY;
 BY GRP1 GRP2 LEVELS1 LEVELS2;
RUN;

DATA R;
SET V4R.EVT9614 (WHERE=(YEAR=&YEAR));
  GRP1=UPCASE(COMPRESS(GRP1));
  LEVELS1=UPCASE(COMPRESS(LEVELS1));
  GRP2=UPCASE(COMPRESS(GRP2));
  LEVELS2=UPCASE(COMPRESS(LEVELS2));
  
  R_EST=ROUND(MEANEVT, .1);
  R_SE=ROUND(MEANEVT_SE, .1);  
  
  IF GRP2='SOP';
  IF LEVELS1 NE 'MISSING';
RUN;

PROC SORT DATA=R NODUPKEY;
BY YEAR GRP1 GRP2 LEVELS1 LEVELS2;
RUN;

DATA COMP;
MERGE SAS (IN=B KEEP=YEAR GRP1 GRP2 LEVELS1 LEVELS2 SAS_EST SAS_SE)
      R   (IN=A KEEP=YEAR GRP1 GRP2 LEVELS1 LEVELS2 R_EST R_SE) ;
BY YEAR GRP1 GRP2 LEVELS1 LEVELS2;
R=0;
SAS=0;
IF A THEN R=1;
IF B THEN SAS=1;

DIFF_EST=0; 
DIFF_SE=0;

IF A AND B AND R_EST NE SAS_EST THEN DIFF_EST=1;
IF A AND B AND R_SE NE SAS_SE THEN DIFF_SE=1;
RUN;

PROC FREQ DATA=COMP;
TABLES R*SAS DIFF:/LIST MISSING;
RUN;

PROC PRINT DATA=COMP (WHERE=(R NE 1 OR SAS NE 1));
RUN;

PROC PRINT DATA=COMP (WHERE=(DIFF_EST=1 OR DIFF_SE=1));
RUN;

%MEND YY;
%YY (YEAR=1996);
%YY (YEAR=1997);
%YY (YEAR=1998);
%YY (YEAR=1999);
%YY (YEAR=2000);
%YY (YEAR=2001);
%YY (YEAR=2002);
%YY (YEAR=2003);
%YY (YEAR=2004);
%YY (YEAR=2005);
%YY (YEAR=2006);
%YY (YEAR=2007);
%YY (YEAR=2008);
%YY (YEAR=2009);
%YY (YEAR=2010);
%YY (YEAR=2011);
%YY (YEAR=2012);
%YY (YEAR=2013);
%YY (YEAR=2014);

RUN;

%jobend;

run;

