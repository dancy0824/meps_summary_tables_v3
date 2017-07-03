/*********************************************************************

 MEANEXP_CROSS2_QC.SAS:
   Program to compare R Cross-Sectional data (R_vers) to SAS (S_vers) for CROSS2 (Demographic by Source of Payment), by Year

*********************************************************************/

OPTIONS OBS=MAX LS=160 PS=79 NOCENTER MPRINT MPRINTNEST ;

%let begyear = 1996 ;
%let endyear = 2014 ;

%let QCvar = MEANEXP ;
%let cross = CROSS2 ;

/*********************************************************************************/
/* this program will compare the following:

/* source SAS dataset created by SSS for computing stats for comparing to R stats */
LIBNAME FYCLIB  "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" ACCESS = READONLY ;
%let S_vers = PERSDATA9614V2 ;  /* datetime stamp is 5/25/2017 12:13 pm */

/* STANDARDIZED SAS VERSION of R tables sent from AHRQ for display on Shiny server website */
LIBNAME COMPLIB "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" ACCESS = READONLY ;
%let R_vers = STD_FYC9614 ;         /* datetime stamp is 6/9/2017 11:56 am */

/*********************************************************************************/

/* include file with macro variable declarations for the DEMO/HEALTH STATUS/SES grouping variables */
%include "\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Cyndi\inc_DEMO_VARS_V4.SAS" ;
%let MAXDEMOVARS = 14 ;  /* OVERRIDE TO <14 FOR TESTING */

/*********************************************************************************/

/* use/modify these to filter R file to run comparisons */
%LET WHERECLAUSE_CROSS2  = %STR ( WHERE ALSO GRP2 EQ "SOP" AND LEVELS2n NE 6 AND MEANEXP IS NOT MISSING ; ) ;
%LET WHERECLAUSE_AGE = %STR ( WHERE ALSO AGEGRPS_V2 NE 3 ;) ;

/*********************************************************************************/

%macro YY (year) ;

  DATA FYC&year ;
    SET FYCLIB.&S_vers (KEEP = YEAR PERWTF VARSTR VARPSU &DEMOVARS SOP:) ;
    WHERE YEAR EQ &year ;
    ATTRIB _ALL_ LABEL = ' ' ;
  RUN ;

  %do i = 1 %to 5 ;

    ODS LISTING CLOSE;

    PROC SURVEYMEANS
      DATA = FYC&year
      MISSING
      MEAN STDERR ;
      CLUSTER VARPSU;
      STRATA  VARSTR;
      WEIGHT  PERWTF;
      VAR     SOP&i.EXP ;
      DOMAIN
        %do j = 1 %to &MAXDEMOVARS ;
           &&DEMOVAR&j * SOP&i
        %end ;
      ;
      ODS OUTPUT DOMAIN = OUT_&year._SOP&i ;
    RUN;

    ODS LISTING;

    *TITLE "&year SOP&i=1" ;
    *PROC PRINT ;
    *  WHERE SOP&i EQ 1 ;
    *RUN ;

    DATA &cross._&year._SOP&i  ;

      SET OUT_&year._SOP&i ;
      WHERE SOP&i EQ 1 ;
      &WHERECLAUSE_AGE ;

      KEEP GRP1 GRP2 LEVELS1n LEVELS2n YEAR &QCvar &QCvar._SE ;
      LENGTH YEAR 8 GRP1 GRP2 $15 LEVELS1n LEVELS2n &QCvar &QCvar._SE 8 ;

      YEAR       = &year ;

      &QCvar     = ROUND (MEAN, 1) ;
      &QCvar._SE = ROUND (STDERR, 1) ;

      GRP1 = SCAN (DOMAINLABEL, 1) ;
      GRP2 = "SOP" ;

      SELECT (GRP1) ;
       %do x = 1 %to &MAXDEMOVARS ;
        WHEN ("&&DEMOVAR&x") LEVELS1n = &&DEMOVAR&x ;
       %end ;
       OTHERWISE PUT '** UNK DEMO VAR ** ' GRP1= ;
      END ;

      LEVELS2n = &i ;

    RUN;

    *TITLE "&cross._&year SOP&i" ;
    *PROC PRINT ;
    *RUN ;

    PROC APPEND BASE = &cross._&year DATA = &cross._&year._SOP&i ;
    RUN ;

  %end ;

  PROC APPEND BASE = QC_&cross DATA = &cross._&year ;
  RUN ;

%mend YY ;

%MACRO ALL_YEARS ;

  %do yr = &begyear %to &endyear ;
    %YY (&yr)
  %end ;

%MEND ALL_YEARS ;

%ALL_YEARS

PROC SORT DATA = QC_&cross ;
  BY YEAR GRP1 GRP2 LEVELS1n LEVELS2n ;
RUN ;

*proc print n ;
*run ;

PROC SORT DATA = COMPLIB.&R_vers (KEEP = YEAR GRP1 GRP2 LEVELS1n LEVELS2n &QCvar &QCvar._SE)
  OUT = COMP ;
  WHERE (YEAR BETWEEN &begyear AND &endyear) ;
  &WHERECLAUSE_CROSS2 ;
  BY YEAR GRP1 GRP2 LEVELS1n LEVELS2n ;
RUN ;

TITLE "DIFFERENCES FOR &QCvar &cross, by Year" ;

PROC COMPARE NOSUMMARY
  DATA    = QC_&cross
  COMPARE = COMP
  OUTNOEQUAL
  OUT = PRT_QC
  ;
  BY YEAR ;  *GRP1 GRP2 LEVELS1n LEVELS2n ;
  ID YEAR GRP1 GRP2 LEVELS1n LEVELS2n ;
  VAR &QCvar &QCvar._SE ;
RUN ;

PROC PRINT UNIFORM N ;
  VAR YEAR GRP1 GRP2 LEVELS1n LEVELS2n &QCvar: ;
  BY YEAR ;
RUN;

DATA PRINT_CHECK_COMP ;
  MERGE COMP (IN=INR RENAME = (&QCvar=&QCvar._R &QCvar._SE=&QCvar._SE_R) )
   QC_&cross (IN=INS RENAME = (&QCvar=&QCvar._S &QCvar._SE=&QCvar._SE_S) )
  ;
  BY YEAR GRP1 GRP2 LEVELS1n LEVELS2n ;
  IF (INR AND NOT INS) OR (INS AND NOT INR) ;
  IF INR THEN FLAG = "R" ;
  ELSE IF INS THEN FLAG = "S" ;
RUN ;

TITLE "OBS DIFFERENCES FOR &QCvar &cross" ;
PROC PRINT UNIFORM N NOOBS ;
  VAR YEAR FLAG GRP1 LEVELS1n GRP2 LEVELS2n &QCvar: ;
  BY YEAR ;
RUN;




/*********************************************************************************/

%put &=SYSCC ;

/*********************************************************************************/