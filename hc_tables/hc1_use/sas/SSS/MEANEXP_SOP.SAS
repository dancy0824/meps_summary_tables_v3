/*********************************************************************

 MEANEXP_SOP_QC.SAS:
   Program to compare R trend data (R_vers) to SAS (S_vers) for Source of Payment variables, by Year

*********************************************************************/

OPTIONS OBS=MAX LS=160 PS=79 NOCENTER MPRINT MPRINTNEST ;

%let begyear = 1996 ;
%let endyear = 2014 ;

%let QCvar = MEANEXP ;

/*********************************************************************************/
/* this program will compare the following:

/* source SAS dataset created by SSS for computing stats for comparing to R stats */
LIBNAME FYCLIB  "\\derived.ahrq.local\derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data" ACCESS = READONLY ;
%let S_vers = PERSDATA9614V2 ;  /* datetime stamp is 5/25/2017 12:13 pm */

/* STANDARDIZED SAS VERSION of R tables sent from AHRQ for display on Shiny server website */
LIBNAME COMPLIB "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4" ACCESS = READONLY ;
%let R_vers = STD_FYC9614 ;         /* datetime stamp is 6/9/2017 11:56 am */

/*********************************************************************************/

/* use/modify these to filter R file to run comparisons */
%LET WHERECLAUSE_SOP = %STR(WHERE ALSO GRP1 EQ "IND" AND GRP2 EQ "SOP" AND LEVELS2n NE 6 AND MEANEXP IS NOT MISSING ; ) ;

/*********************************************************************************/

%macro YY (year) ;

  DATA FYC&year ;
    SET FYCLIB.&S_vers (KEEP = YEAR PERWTF VARSTR VARPSU IND SOP:) ;
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
      DOMAIN  IND * SOP&i ;
      ODS OUTPUT DOMAIN = OUT_&year._SOP&i ;
    RUN;

    ODS LISTING;

    *TITLE "&year SOP&i" ;
    *PROC PRINT ;
    *  WHERE SOP&i EQ 1 ;
    *RUN ;

    DATA &QCvar._&year._SOP&i  ;

      SET OUT_&year._SOP&i ;
      WHERE SOP&i EQ 1 ;
      KEEP GRP2 LEVELS2n YEAR &QCvar &QCvar._SE ;
      LENGTH YEAR 8 GRP2 $15 LEVELS2n &QCvar &QCvar._SE 8 ;

      YEAR       = &year ;

      &QCvar     = ROUND (MEAN, 1) ;
      &QCvar._SE = ROUND (STDERR, 1) ;

      GRP2 = "SOP" ;
      LEVELS2n = &i ;

    RUN;

    *TITLE "&QCvar._&year SOP&i" ;
    *PROC PRINT ;
    *RUN ;

    PROC APPEND BASE = &QCvar._&year DATA = &QCvar._&year._SOP&i ;
    RUN ;

  %end ;

  PROC APPEND BASE = QC_&QCvar DATA = &QCvar._&year ;
  RUN ;

%mend YY ;

%MACRO ALL_YEARS ;

  %do yr = &begyear %to &endyear ;
    %YY (&yr)
  %end ;

%MEND ALL_YEARS ;

%ALL_YEARS

PROC SORT DATA = QC_&QCvar ;
  BY YEAR GRP2 LEVELS2n ;
RUN ;

*proc print n ;
*run ;

PROC SORT DATA = COMPLIB.&R_vers (KEEP = YEAR GRP1 GRP2 LEVELS2n &QCvar &QCvar._SE)
  OUT = COMP (DROP = GRP1) ;
  WHERE (YEAR BETWEEN &begyear AND &endyear) ;
  &WHERECLAUSE_SOP ;
  BY YEAR GRP2 LEVELS2n ;
RUN ;

TITLE "DIFFERENCES FOR &QCvar, by Year" ;

PROC COMPARE NOSUMMARY
  DATA    = QC_&QCvar
  COMPARE = COMP
  OUTNOEQUAL
  OUT = PRT_QC
  ;
  BY YEAR ;  *GRP2 LEVELS2n ;
  ID YEAR GRP2 LEVELS2n ;
  VAR &QCvar &QCvar._SE ;
RUN ;

PROC PRINT UNIFORM N ;
  VAR YEAR GRP2 LEVELS2n &QCvar: ;
  BY YEAR ;
RUN;

/*********************************************************************************/

%put &=SYSCC ;

/*********************************************************************************/