/*********************************************************************

 TOTPOP_CROSS2_QC.SAS:
   Program to compare R Cross-Sectional data (R_vers) to SAS (S_vers) for CROSS2 (Demographic by Source of Payment), by Year

*********************************************************************/

OPTIONS OBS=MAX LS=160 PS=79 NOCENTER MPRINT MPRINTNEST ;

%let begyear = 1996 ;
%let endyear = 2014 ;

%let QCvar = TOTPOP ;
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
%LET WHERECLAUSE_CROSS2  = %STR ( WHERE ALSO GRP1 NE "SOP" AND GRP2 EQ "SOP"  AND LEVELS2n NE 6 ; ) ;
%LET WHERECLAUSE_AGE = %STR ( WHERE ALSO AGEGRPS_V2 NE 3 ;) ;

/*********************************************************************************/

/*********************************************************************************/

%macro YY (year) ;

  DATA FYC&year ;
    SET FYCLIB.&S_vers (KEEP = YEAR PERWTF VARSTR VARPSU &DEMOVARS SOP1-SOP5) ;
    WHERE YEAR EQ &year ;
    ATTRIB _ALL_ LABEL = ' ' ;
  RUN ;
  ODS LISTING CLOSE;

  PROC SURVEYMEANS
    DATA = FYC&year
    MISSING
    SUM STD ;
    VAR     SOP1 SOP2 SOP3 SOP4 SOP5 ;
    CLUSTER VARPSU;
    STRATA  VARSTR;
    WEIGHT  PERWTF;
    DOMAIN  &DEMOVARS ;
    ODS OUTPUT DOMAIN = OUT_&year ;
  RUN;

  ODS LISTING;

  *TITLE "&year" ;
  *proc print ;
  *run ;

  DATA &cross._&year  ;

    SET OUT_&year ;
    &WHERECLAUSE_AGE ;

    KEEP GRP1 GRP2 LEVELS1n LEVELS2n YEAR &QCvar &QCvar._SE ;
    LENGTH YEAR 8 GRP1 GRP2 $15 LEVELS1n LEVELS2n &QCvar &QCvar._SE 8 ;

    YEAR       = &year ;

    &QCvar     = ROUND (SUM    /1000, .1) ;
    &QCvar._SE = ROUND (STDDEV /1000, .1) ;

    GRP1 = DOMAINLABEL ;
    GRP2 = "SOP" ;

    SELECT (GRP1) ;
     %do x = 1 %to &MAXDEMOVARS ;
      WHEN ("&&DEMOVAR&x") LEVELS1n = &&DEMOVAR&x ;
     %end ;
     OTHERWISE PUT '** UNK DEMO VAR ** ' GRP1= ;
    END ;

    LEVELS2n = INPUT (SUBSTR (VARNAME,4,1), 1.) ;

  RUN;

  *TITLE "&year" ;
  *proc print ;
  *run ;

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

/*********************************************************************************/

%put &=SYSCC ;

/*********************************************************************************/