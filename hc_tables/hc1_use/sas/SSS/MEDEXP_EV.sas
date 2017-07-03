%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\MEDEXP_EV.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    MEDEXP_EV.XLS                                                      
 * Programmer  :    Bidong Liu                                                                                                                                      
 * Date        :    5/18/2017
*********************************************************************;
OPTIONS LS=160 PS=60 OBS=MAX MPRINT;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";
libname taskest "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";

TITLE1 "AH4.FF004";
DATA RESULT;
  LENGTH GRP2 $15   LEVELS2 $32;;
grp2="XXXXXXX";
levels2="xxx";
IF _N_ LT 0;
RUN;






/*
TITLE2 "QC";
PROC MEANS DATA=TASK.PERSDATA9614  NMISS;
  VAR TOTEXP DVEXP RXEXP OBEXP OBYEXP OBZEXP OPEXP OPYEXP OPZEXP EREXP IPEXP  HHEXP HHAEXP HHNEXP  OTEXP;
RUN;
*/



%MACRO YY (VARLIST);
%DO YEAR=1996 %TO 2014;
  %LOCAL VAR OUTPUT I;
  %DO I=1 %TO %SYSFUNC(COUNTW(&VARLIST));
  %LET VAR=%SCAN(&VARLIST,&I);

  TITLE3 "YEAR=&YEAR";
  DATA FY;
    SET TASK.PERSDATA9614 (WHERE=(YEAR=&YEAR));
    TOTAL=1;
  RUN;
  
  PROC SORT DATA=FY;
    BY VARSTR VARPSU;
  RUN;
  
  ODS LISTING CLOSE;
  PROC DESCRIPT DATA=FY ;
    NEST VARSTR VARPSU;
    WEIGHT PERWTF;
    SUBPOPN &var>0;
    VAR &var;
    CLASS TOTAL;
    TABLES TOTAL;
    PERCENTILES 50/ MEDIAN;
    OUTPUT /FILETYPE=SAS REPLACE PERCENTILE=DEFAULT  FILENAME=OUT1;
  RUN;
  ODS LISTING;
  
  DATA AA(KEEP=YEAR MEDEXP_S MEDEXP_S_SE GRP2 LEVELS2);
  SET OUT1(WHERE=(TOTAL=1));
    YEAR=&YEAR;
    MEDEXP_S=QTILE;
    MEDEXP_S_SE=SEQTILE;
    GRP2="           " ;
    LEVELS2="            ";
    %IF &VAR=TOTEXP %THEN %DO; GRP2="IND"; LEVELS2="Total"; %END;
    %IF &VAR=DVEXP %THEN %DO; GRP2="event"; LEVELS2="DVT"; %END;
    %IF &VAR=RXEXP %THEN %DO; GRP2="event"; LEVELS2="RX"; %END;
    %IF &VAR=OBEXP %THEN %DO; GRP2="event"; LEVELS2="OBV"; %END;
    %IF &VAR=OBYEXP %THEN %DO; GRP2="event"; LEVELS2="OBD"; %END;
    %IF &VAR=OBZEXP %THEN %DO; GRP2="event"; LEVELS2="OBO"; %END;
    %IF &VAR=OPEEXP %THEN %DO; GRP2="event"; LEVELS2="OPT"; %END;
    %IF &VAR=OPYEXP %THEN %DO; GRP2="event"; LEVELS2="OPY"; %END;
    %IF &VAR=OPZEXP %THEN %DO; GRP2="event"; LEVELS2="OPZ"; %END;
    %IF &VAR=EREXP %THEN %DO; GRP2="event"; LEVELS2="ERT"; %END;
    %IF &VAR=IPEXP %THEN %DO; GRP2="event"; LEVELS2="IPT"; %END;      
    %IF &VAR=HHEXP %THEN %DO; GRP2="event"; LEVELS2="HHT"; %END;
    %IF &VAR=HHAEXP %THEN %DO; GRP2="event"; LEVELS2="HHA"; %END;
    %IF &VAR=HHNEXP %THEN %DO; GRP2="event"; LEVELS2="HHN"; %END;
    %IF &VAR=OTEXP %THEN %DO; GRP2="event"; LEVELS2="OMA"; %END;
  
  RUN;
  
  DATA RESULT;
  SET RESULT  AA;
  RUN;
  %END;


%END;
%MEND YY;
%YY (TOTEXP DVEXP RXEXP OBEXP OBYEXP OBZEXP OPEXP OPYEXP OPZEXP EREXP IPEXP  HHEXP HHAEXP HHNEXP  OTEXP)


PROC SORT DATA=TASKest.FYC9614 OUT=REST(WHERE=(GRP1='ind' and LEVELS1='Total')  KEEP=GRP1 LEVELS1 GRP2 LEVELS2 MEDEXP MEDEXP_SE YEAR);
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
LEVELS3=LOWCASE(COMPRESS(LEVELS2));
IF GRP3="" THEN DELETE;
RUN;

PROC SORT DATA=RESULT2;
BY GRP3 LEVELS3 descending year;
RUN;

PROC SORT DATA=Rest2;
BY GRP3 LEVELS3 descending year;
RUN;


DATA ALL;
MERGE RESULT2(IN=A KEEP=YEAR GRP3 LEVELS3 MEDEXP_S MEDEXP_S_SE) REST2(IN=B KEEP=YEAR GRP3 LEVELS3  MEDEXP MEDEXP_SE);
BY  GRP3 LEVELS3 descending year;
IF A;
DIFF_MEDEXP=abs(round(SUM(MEDEXP,-MEDEXP_S), 1));
DIFF_SE=abs(round(SUM(MEDEXP_SE, -MEDEXP_S_SE),1));
RUN;


PROC PRINT DATA=ALL(WHERE=(DIFF_MEDEXP GE 8 OR  DIFF_SE GE 8 )) NOOBS;
VAR YEAR GRP3 LEVELS3  MEDEXP MEDEXP_S DIFF_MEDEXP MEDEXP_SE  MEDEXP_S_SE DIFF_SE;
FORMAT MEDEXP MEDEXP_S DIFF_MEDEXP MEDEXP_SE  MEDEXP_S_SE DIFF_SE COMMA20.0;
RUN;






