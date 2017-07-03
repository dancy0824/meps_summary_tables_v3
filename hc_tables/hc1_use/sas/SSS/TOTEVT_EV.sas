%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\TOTEVT.EV.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    NO                                                    
 * Programmer  :    Bidong Liu                                                                                                                                      
 * Date        :    5/18/2017
*********************************************************************;
OPTIONS LS=200 PS=60 OBS=MAX;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";
libname taskest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";



TITLE1 "AH4.FF004";

PROC SORT DATA=TASK.PERSDATA9614 OUT=PER;
  BY YEAR DUPERSID;
RUN;

PROC SORT DATA=TASK.EVNTDATA9614 OUT=EVNT;
  BY YEAR DUPERSID;
RUN;

DATA EVNT1;
  MERGE EVNT(IN=A) PER(IN=B KEEP=YEAR DUPERSID  VARPSU VARSTR PERWTF);
  BY YEAR DUPERSID;
  SUB=2;
  IF A THEN SUB=1;
RUN;


DATA RESULT;
LENGTH DEMO $45;
DEMO="XXXXXXXXXXXXXXXXX";
IF _N_ LT 0;
RUN;

%MACRO YY;
%DO YEAR=1996 %TO 2014;

DATA FY;
SET EVNT1(WHERE=(YEAR=&YEAR));
  IND=1;
  TOTAL=1;
RUN;

/*
PROC MEANS DATA=FY NMISS;
VAR IND SUB YEAR TOTAL HH_PROV OP_DR OB_DR;
RUN;
*/

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY SUMWGT STD MISSING;
  VAR IND;
  CLUSTER VARPSU; 
  STRATA VARSTR;
  WEIGHT PERWTF;
  DOMAIN SUB*TOTAL SUB*EVNTYP SUB*HH_PROV SUB*OP_DR SUB*OB_DR;
  ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;


/*
DATA OUT2 (WHERE=(grp2 NE " "));
  SET OUT1(where=(sub=1 ));
   LENGTH grp2 $15  LEVELS2 $32;
IF TOTAL=1 THEN do; grp2='ind'; levels2='Total'; end;
IF EVNTYP='DVIS' THEN do; grp2='event'; levels2='Dental Visits';end;
  ELSE IF EVNTYP='PMED' THEN do; grp2='event'; levels2='Prescription Medicines';end;
  ELSE IF EVNTYP='MVIS' THEN do; grp2='event'; levels2='Office-based Events';end;
  ELSE IF EVNTYP='OPAT' THEN do; grp2='event'; levels2='Outpatient Events';end;
  ELSE IF EVNTYP='EROM' THEN do; grp2='event'; levels2='Emergency Room Visits';end;
  ELSE IF EVNTYP='STAZ' THEN do; grp2='event'; levels2='Inpatient Stays';end;
  ELSE IF EVNTYP='HVIS' THEN do; grp2='event'; levels2='Home Health Events';end;
  ELSE IF EVNTYP='OMED' THEN do; grp2='event'; levels2='Other Medical Expenses';end;
    
IF HH_PROV=1 THEN do; grp2='event_v2'; levels2="Home Health Agency";end;
  ELSE IF HH_PROV=2 THEN do; grp2='event_v2'; levels2="Home Health Independent";end;
IF OP_DR=1 THEN do; grp2='event_v2'; levels2='Physician Hosp. Visits';end;
  ELSE IF OP_DR=2 THEN do; grp2='event_v2'; levels2='Non-physician Hosp. Visits';end;
IF OB_DR=1 THEN do; grp2='event_v2'; levels2='Physician Office Visits';end;
  ELSE IF OB_DR=2 THEN do; grp2='event_v2'; levels2='Non-physician Office Visits';end;

     YEAR=&YEAR;
RUN;
*/

DATA OUT2 (WHERE=(grp2 NE " "));
  SET OUT1(where=(sub=1 ));
   LENGTH grp2 $15  LEVELS2 $32;
IF TOTAL=1 THEN do; grp2='ind'; levels2='Total'; end;
IF EVNTYP='DVIS' THEN do; grp2='event'; levels2='DVT';end;
  ELSE IF EVNTYP='PMED' THEN do; grp2='event'; levels2='RX';end;
  ELSE IF EVNTYP='MVIS' THEN do; grp2='event'; levels2='OBV';end;
  ELSE IF EVNTYP='OPAT' THEN do; grp2='event'; levels2='OPT';end;
  ELSE IF EVNTYP='EROM' THEN do; grp2='event'; levels2='ERT';end;
  ELSE IF EVNTYP='STAZ' THEN do; grp2='event'; levels2='IPT';end;
  ELSE IF EVNTYP='HVIS' THEN do; grp2='event'; levels2='HHT';end;
  ELSE IF EVNTYP='OMED' THEN do; grp2='event'; levels2='OMA';end;
    
IF HH_PROV=1 THEN do; grp2='event'; levels2="HHA";end;
  ELSE IF HH_PROV=2 THEN do; grp2='event'; levels2="HHN";end;
IF OP_DR=1 THEN do; grp2='event'; levels2='OPY';end;
  ELSE IF OP_DR=2 THEN do; grp2='event'; levels2='OPZ';end;
IF OB_DR=1 THEN do; grp2='event'; levels2='OBD';end;
  ELSE IF OB_DR=2 THEN do; grp2='event'; levels2='OBO';end;

     YEAR=&YEAR;
RUN;


DATA RESULT;
SET RESULT OUT2;
RUN;

%END;
%MEND YY;
%YY 

PROC SORT DATA=TASKest.EVT9614 OUT=REST(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 TOTEVT TOTEVT_SE YEAR);
BY  GRP2 LEVELS2 descending year;
RUN;

DATA REST2;
SET REST;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
RUN;

DATA RESULT2;
SET RESULT;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));;
RUN;

PROC SORT DATA=RESULT2;
BY GRP3 LEVELS3 descending year;
RUN;

DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 SUMWGT STDDEV) REST2(IN=B KEEP=YEAR GRP3 LEVELS3 TOTEVT TOTEVT_SE);
BY  GRP3 LEVELS3 descending year;
IF A;
TOTEVTX=TOTEVT/1000000;
TOTEVT_SEX=TOTEVT_SE/1000000;
SUMWGTX=SUMWGT/1000000;
STDDEVX=STDDEV/1000000;
DIFF_SUM=abs(round(SUM(TOTEVTX,-SUMWGTX), 0.1));
DIFF_SE=abs(round(SUM(TOTEVT_SEX, -STDDEVX),0.1));
RUN;




TITLE2 "# OF EVENTS (in millions) BY 14 EVENT TYPES, FOR 1996-2014";
PROC PRINT DATA=ALL(WHERE=(DIFF_SUM NE 0 OR DIFF_SE NE 0)) LABEL NOOBS;
VAR YEAR GRP3 LEVELS3 TOTEVTX SUMWGTX DIFF_SUM TOTEVT_SEX STDDEVX DIFF_SE;
LABEL TOTEVTX="# of event R"
 SUMWGTX="# of event SAS"
  DIFF_SUM="# of event Diff" 
  TOTEVT_SEX="SE R"
  STDDEVX="SE SAS" 
  DIFF_SE="SE Diff"
  ;
FORMAT TOTEVTX SUMWGTX DIFF_SUM TOTEVT_SEX STDDEVX DIFF_SE COMMA20.1;
RUN;
