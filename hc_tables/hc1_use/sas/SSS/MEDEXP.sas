%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Sample.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    no                                                      
 * Programmer  :    Bidong Liu                                                                                                                                      
 * Date        :    5/12/2017
*********************************************************************;
OPTIONS LS=200 PS=60 OBS=MAX;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";
libname taskest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";

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
run;


DATA RESULT;
LENGTH levels2 $45;
levels2="XXXXXXXXXXXXXXXXX";
IF _N_ LT 0;
RUN;


%MACRO YY;
%do year=1996 %to 2014;

TITLE3 "YEAR=&YEAR";
DATA FY;
  SET TASK.PERSDATA9614 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
RUN;

PROC SORT DATA=Fy;
BY VARSTR VARPSU;
RUN;

ODS LISTING CLOSE;
PROC DESCRIPT DATA=FY;
  NEST VARSTR VARPSU;
  WEIGHT PERWTF;
  SUBPOPN TOTEXP>0;
  VAR TOTEXP;
  CLASS TOTAL AGEGRPS AGEGRPS_V2 REGION MARRIED RACE SEX INSURANCE INSURANCE_V2 HEALTH MENTAL_HEALTH EDUCATION EMPLOYED POVERTY;
  TABLES TOTAL AGEGRPS AGEGRPS_V2 REGION MARRIED RACE SEX INSURANCE INSURANCE_V2 HEALTH MENTAL_HEALTH EDUCATION EMPLOYED POVERTY;
  PERCENTILES 50/ MEDIAN ;
  OUTPUT /FILETYPE=SAS REPLACE PERCENTILE=DEFAULT  FILENAME=OUT1;
RUN;
ODS LISTING;


DATA OUT2 (KEEP=YEAR medexp_s  medexp_s_se grp2 levels2 WHERE=(grp2 NE " "));
  SET OUT1;
   LENGTH grp2 $15  LEVELS2 $32;
    IF TOTAL=1 THEN do; grp2='ind'; levels2='Total'; end;
    ELSE IF AGEGRPS>0 THEN DO; grp2='AGEGRPS'; LEVELS2=PUT(AGEGRPS, AGEGRPS.);END;
    ELSE IF AGEGRPS_V2>0 THEN DO; grp2='AGEGRPS';LEVELS2=PUT(AGEGRPS_V2, AGEGRPS_V2X.);END;
    ELSE IF REGION>0 THEN DO; grp2='REGION'; LEVELS2=PUT(REGION, REGION.);END;
    ELSE IF MARRIED>0 OR MARRIED=-1 THEN DO;grp2='MARRIED'; LEVELS2=PUT(MARRIED, MARRIED.);END;
    ELSE IF RACE>0 THEN DO;grp2='RACE' ; LEVELS2=PUT(RACE, RACE.);END;
    ELSE IF SEX>0 THEN DO; grp2='Sex' ; LEVELS2=PUT(SEX, SEX.);END;
    ELSE IF INSURANCE>0 THEN DO; grp2='INSURANCE' ; LEVELS2=PUT(INSURANCE, INSURANCE.);END;
    ELSE IF INSURANCE_V2>0 THEN DO; grp2='INSURANCE' ; LEVELS2=PUT(INSURANCE_V2, INSURANCE_V2X.);END;
    ELSE IF HEALTH>0 THEN DO; grp2='HEALTH' ; LEVELS2=PUT(HEALTH, HEALTH.);END;
    ELSE IF MENTAL_HEALTH>0 THEN DO; grp2='MENTAL_HEALTH' ; LEVELS2=PUT(MENTAL_HEALTH, MENTAL_HEALTH.);END;
    ELSE IF EDUCATION>0 OR EDUCATION=-1 THEN DO; grp2='EDUCATION' ; LEVELS2=PUT(EDUCATION, EDUCATION.);END;
    ELSE IF EMPLOYED>0 OR EMPLOYED=-1 THEN DO; grp2='EMPLOYED' ; LEVELS2=PUT(EMPLOYED, EMPLOYED.);END;
    ELSE IF POVERTY>0 THEN DO; grp2='POVERTY' ; LEVELS2=PUT(POVERTY, POVERTY.);END;
    
    YEAR=&YEAR;
    ORDER=_N_;
    medexp_s= QTILE  ;
    medexp_s_se=SEQTILE;
RUN;

DATA RESULT;
SET RESULT OUT2;
RUN;

%END;
%MEND YY;
%YY 
 
PROC SORT DATA=TASKest.FYC9614 OUT=REST(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 MEDEXP MEDEXP_SE YEAR);
BY YEAR GRP2 LEVELS2;
RUN;

DATA REST2;
SET REST;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
RUN;

DATA RESULT2;
SET RESULT;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
RUN;

PROC SORT DATA=RESULT2 nodupkey;
BY YEAR GRP3 LEVELS3;
RUN;

DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 medexp_s medexp_s_se) REST2(IN=B KEEP=YEAR GRP3 LEVELS3 MEDEXP MEDEXP_SE);
BY YEAR GRP3 LEVELS3;
IF A;
DIFF_MEDEXP=abs(round(SUM(MEDEXP,-medexp_s), 1));
DIFF_SE=abs(round(SUM(medexp_se, -medexp_s_se),1));
RUN;

TITLE2 "# OF EVENTS (in millions) BY 14 DEMOGRAPHIC/HEALTH/SOCIO-ECONOMIC STATUS, FOR 1996-2014";
PROC PRINT DATA=ALL(WHERE=(DIFF_MEDEXP GE 5 OR DIFF_SE GE 5)) LABEL;
VAR YEAR GRP3 LEVELS3 MEDEXP MEDEXP_S DIFF_MEDEXP MEDEXP_SE MEDEXP_S_SE DIFF_SE;
FORMAT MEDEXP MEDEXP_S DIFF_MEDEXP MEDEXP_SE MEDEXP_S_SE DIFF_SE COMMA20.0;
RUN;

%jobend;