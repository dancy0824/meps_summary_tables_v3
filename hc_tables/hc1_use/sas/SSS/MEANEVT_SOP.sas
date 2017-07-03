%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\MEANEVT_SOP.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    NO                                                     
 * Programmer  :    Bidong Liu                                                                                                                                      
 * Date        :    5/18/2017
*********************************************************************;
OPTIONS LS=160 PS=60 OBS=MAX;
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
IF SUB=2 THEN XP=0;
  TOTAL=1;
RUN;

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY MEAN STDERR MISSING;
  VAR XP sop1exp sop2exp sop3exp sop4exp sop5exp;
  CLUSTER VARPSU; 
  STRATA VARSTR;
  WEIGHT PERWTF;
 DOMAIN SUB;
  ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;



DATA OUT2 ( WHERE=(grp2 NE " "));
  SET OUT1(WHERE=(SUB=1));
   LENGTH grp2 $15  LEVELS2 $32;
  IF VARNAME='XP' THEN do; grp2='ind'; levels2='Total'; end;
  IF varname="SOP1EXP" THEN DO; grp2='sop'; levels2="SF";END;
  IF varname="SOP2EXP" THEN DO; grp2='sop'; levels2='PR';END;
  IF varname="SOP3EXP" THEN DO; grp2='sop'; levels2='MR';END;
  IF varname="SOP4EXP" THEN  DO; grp2='sop'; levels2='MD'; END;
  IF varname="SOP5EXP" THEN  DO; grp2='sop'; levels2='OZ';END;

     YEAR=&YEAR;
RUN;





DATA RESULT;
SET RESULT OUT2;
RUN;

%END;
%MEND YY;
%YY 




PROC SORT DATA=TASKest.EVT9614 OUT=REST(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 MEANEVT MEANEVT_SE YEAR);
BY  GRP2 LEVELS2 DESCENDING  YEAR;
RUN;

DATA REST2;
SET REST;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
/*
TOTEVT=round(TOTEVT,1);
TOTEVT_SE=round(TOTEVT_SE,1);*/
RUN;

DATA RESULT2;
SET RESULT;
GRP3=LOWCASE(COMPRESS(GRP2));
LEVELS3=LOWCASE(COMPRESS(LEVELS2));;
RUN;

PROC SORT DATA=RESULT2;
BY GRP3 LEVELS3 DESCENDING  YEAR;
RUN;

DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 MEAN STDERR) REST2(IN=B KEEP=YEAR GRP3 LEVELS3 MEANEVT MEANEVT_SE);
BY  GRP3 LEVELS3 DESCENDING  YEAR;
IF A;
DIFF_SUM=abs(round(SUM(MEANEVT,-MEAN)));
DIFF_SE=abs(round(SUM(MEANEVT_SE, -STDERR)));
RUN;


TITLE1 "Mean expenditure per event($) by SOURCE OF PAYMENT, United States, 1996-2014";
PROC PRINT DATA=ALL(WHERE=(DIFF_SUM NE 0 OR DIFF_SE NE 0)) LABEL NOOBS;
VAR YEAR GRP3 LEVELS3 MEANEVT MEAN DIFF_SUM MEANEVT_SE STDERR DIFF_SE;
LABEL MEANEVT="MEAN EXPENDITURE PER EVENT($) R"
 MEAN="MEAN EXPENDITURE PER EVENT($) SAS"
  DIFF_SUM="MEAN EXPENDITURE PER EVENT($) Diff" 
  MEANEVT_SE="SE R"
  STDERR="SE SAS" 
  DIFF_SE="SE Diff"
  ;
FORMAT MEANEVT MEAN DIFF_SUM MEANEVT_SE STDERR DIFF_SE COMMA20.0;
RUN;



%jobend;





