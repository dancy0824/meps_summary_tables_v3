%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\TOTEVT_SOP.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :                                                       
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
  IND=1;
  TOTAL=1;
RUN;

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY SUMWGT STD MISSING;
  VAR IND;
  CLUSTER VARPSU; 
  STRATA VARSTR;
  WEIGHT PERWTF;
  DOMAIN SUB*TOTAL SUB*SOP1 SUB*SOP2 SUB*SOP3 SUB*SOP4 SUB*SOP5;
  ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;


DATA OUT2 ( WHERE=(grp2 NE " "));
  SET OUT1(WHERE=(SUB=1));
   LENGTH grp2 $15  LEVELS2 $32;
  IF TOTAL=1 THEN do; grp2='ind'; levels2='Total'; end;
  IF SOP1=1 THEN DO; grp2='sop'; levels2="SF";END;
  IF SOP2=1 THEN DO; grp2='sop'; levels2='PR';END;
  IF SOP3=1 THEN DO; grp2='sop'; levels2='MR';END;
  IF SOP4=1 THEN  DO; grp2='sop'; levels2='MD'; END;
  IF SOP5=1 THEN  DO; grp2='sop'; levels2='OZ';END;

     YEAR=&YEAR;
RUN;






DATA RESULT;
SET RESULT OUT2;
RUN;

%END;
%MEND YY;
%YY 



PROC SORT DATA=TASKest.EVT9614 OUT=REST(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 TOTEVT TOTEVT_SE YEAR);
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
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
RUN;

PROC SORT DATA=RESULT2;
BY GRP3 LEVELS3 DESCENDING  YEAR;
RUN;

DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 SUMWGT STDDEV) REST2(IN=B KEEP=YEAR GRP3 LEVELS3 TOTEVT TOTEVT_SE);
BY  GRP3 LEVELS3 DESCENDING  YEAR;
IF A;
TOTEVTX=TOTEVT/1000000;
TOTEVT_SEX=TOTEVT_SE/1000000;
SUMWGTX=SUMWGT/1000000;
STDDEVX=STDDEV/1000000;
DIFF_SUM=abs(round(SUM(TOTEVTX,-SUMWGTX), 0.1));
DIFF_SE=abs(round(SUM(TOTEVT_SEX, -STDDEVX),0.1));
RUN;

/*
proc sort data=all;
by levels3 descending year ;
run;


TITLE2 "# OF EVENTS (in millions) BY SOP, FOR 1996-2014";
PROC PRINT DATA=ALL LABEL;
VAR YEAR GRP3 LEVELS3  SUMWGTx  STDDEVx ;
by levels3;
LABEL 
 SUMWGTx="# of event SAS"

  STDDEVx="SE SAS" 

  ;
FORMAT SUMWGTx  STDDEVx  COMMA20.1;
RUN;
*/






TITLE2 "# OF EVENTS (in millions) BY SOP, FOR 1996-2014";
PROC PRINT DATA=ALL(WHERE=(DIFF_SUM NE 0 OR DIFF_SE NE 0)) LABEL;
VAR YEAR GRP3 LEVELS3 TOTEVTx SUMWGTx DIFF_SUM TOTEVT_SEx STDDEVx DIFF_SE;
LABEL TOTEVTx="# of event R"
 SUMWGTx="# of event SAS"
  DIFF_SUM="# of event Diff" 
  TOTEVT_SEx="SE R"
  STDDEVx="SE SAS" 
  DIFF_SE="SE Diff"
  ;
FORMAT TOTEVTx SUMWGTx DIFF_SUM TOTEVT_SEx STDDEVx DIFF_SE COMMA20.1;
RUN;

%jobend;


