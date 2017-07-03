%JOBSTART;

ODS NOPROCTITLE;
OPTIONS LS=155 PS=55;
*OPTIONS OBS=0;
********************************************************************************************
Software Version:   9.2
Creation Date:      2008-08-29              
Task:               AH3.FD001
Task Title:         2007 Prescribed Medicines Event PUF

Program Name: AHRQ3\F_DSO\FD001NKK\PROG\RX08V06.SAS

Description:  Creates 6th version of the PMED event PUF.

              Creates variables PARTITIO_WEB and PARTMED_WEB to use in programs that generate
              the top drugs ranked on frequency and ranked on expenditures for the 
              Tables Compendia on the Web. Variables are based on RXNAME and RXNDC.
              
Reference:    Marie Stagnitti / Nancy Kieffer

Input File:   AHRQ3\F_DSO\FD001NKK\DATA\RX08V05.SAS7BDAT

Output File:  AHRQ2\F_DSO\FC008NKK\DATA\RX08V06.SAS7BDAT
             
Programmer:   Nancy Kieffer (using code developed by Ed Hock for PMED editing 
              tasks in jobs QCFREQ1-QCFREQ4).

Modifications: Blaine Byars 
              2008-10-14 Use Puf names 
              2009-07-13 Identify masked RXNAME with numeric beginning rather than RXNAME value of -9
							2009-08-12 For 2007, change PARTMED_WEB=SUPPLY to a meaningful name.
********************************************************************************************;
%LET YY=08; /* current PUF year */

LIBNAME INOUT "C:\AHRQ3\F_DSO\FD001NKK\DATA"; 

libname inout "C:\Users\emily.mitchell\Desktop\AHRQ\MEPS\PUFs\SAS V8 Local";

TITLE1 "Task AH3.FD001: 20&YY Prescribed Medicines Event PUF";
     
PROC FORMAT;
  VALUE PARTITIO
    . = "."
  1-1936 = "1-1936";
  
  VALUE $RXNAME
  "  "   = "BLANK"
  "-1"  = "-1 INAPPLICABLE"
  "-7"  = "-7 REFUSED"
  "-8"  = "-8 DK"
  "-9"  = "-9 NOT ASCERTAINED"
  OTHER = "0-ZZZZZZZZZZ";
RUN;

DATA RX&YY.V05;
  SET INOUT.RX&YY.V05;
  
  LENGTH RXNAME_C $50;
  
  RXNAME_C = MEDNAMEC;  
  
  /* If PUF name masked, then mask MEDNAMEC, EXCEPT FOR CASES WITH NON-MISSING NDC */
  /* These cases have MED1=-9 from the PC file */
  *IF RXNAME="-9" THEN RXNAME_C="-9";
  IF CHGFLG=1 
   THEN DO;
   				 	IF RXNDC="-9"
   				 	 THEN RXNAME_C="-9";  
   				 	 ELSE RXNAME_C=RXNAME;  /* = TC1 */
   			END;
  
  
  /* Mononine presents a new case for 2007.  It is the 1st and only case
  /* where rxndc is masked with -9, but rxname is masked with a different name.
  /* This created issues for the logic here to group drugs with the same rxndc, i.e. -9.
  /* Set mononine ndc temporarily to a unique dummy value. */
  /* if rxname = "COAGULATION MEDICINE"  
  /*  then do;
  /*          RXNAME_C = "COAGULATION MEDICINE" ;
  /*          rxndc = "dummy";
  /*       end;
  /* */
  
RUN;
  
PROC SORT DATA=RX&YY.V05 (KEEP=RXRECID RXNDC RXNAME RXNAME_C CHGFLG)
           OUT=PMEDS; 
  BY RXNDC RXNAME_C; 
RUN;

/* Summarize to RXNDC RXNAME_C level */
PROC SUMMARY DATA=PMEDS NWAY;
  WHERE RXNAME_C NE "-9";
  BY RXNDC RXNAME_C;
  OUTPUT OUT=NDCMED (KEEP=RXNDC RXNAME_C _FREQ_);
RUN;

/* Sort by descending frequency */
PROC SORT DATA=NDCMED;
   BY RXNDC DESCENDING _FREQ_; 
RUN;

*--------------------------------------------------------------------------------------------------
* Create RXNDC-level dataset containing the most frequently occuring RXNAME_C in the RXNDC group.
* There may be multiple RXNAME_Cs within an RXNDC group with the same highest frequency.
* If so, output all records with the highest freq.
*--------------------------------------------------------------------------------------------------;
DATA NDCMED (DROP=TOPFREQ);
  SET NDCMED;
  BY RXNDC;
  RETAIN TOPFREQ;
  IF FIRST.RXNDC THEN DO;
    TOPFREQ = _FREQ_;
  END;
  IF _FREQ_ EQ TOPFREQ THEN OUTPUT;
RUN;

/* Create two datasets to compare */
DATA NDCMED1
     NDCMED2 (KEEP=RXNDC RXNAME_C);
  SET NDCMED NOBS=NOBS END=EOF;
  OBSNO = _N_;
  OUTPUT NDCMED1;
  OUTPUT NDCMED2;
  IF EOF THEN CALL SYMPUT('NOBS',REVERSE(TRIM(REVERSE(PUT(NOBS,10.)))));
RUN;

*-------------------------------------------------------------------------------------------
* Define partitions of RXNDC/RXNAME_C combinations such that no RXNDC or RXNAME_C of any 
* RXNDC/RXNAME_C combination in a partition exists in any other partition.
*-------------------------------------------------------------------------------------------;
DATA NDCPART (KEEP=OBSNO PARTITIO_WEB);
  SET NDCMED (KEEP=RXNDC RXNAME_C RENAME=(RXNDC=NDC1)) END=EOF;
  
  LABEL PARTITIO_WEB = "Groups records of same drug (RXNDC/RXNAME) to construct Compendia Tables";
  
  ARRAY USED(&NOBS) U1-U&NOBS;
  RETAIN U1-U&NOBS;
  RETAIN NEXTPART 1;

  IF USED(_N_) EQ . THEN DO;
     USED(_N_) = NEXTPART;
     NEXTPART = NEXTPART + 1;
  END;
  PARTITIO_WEB = USED(_N_);

  DO I = _N_ + 1 TO &NOBS;
    /* Open second PMED file and go to the ith observation (2nd-end) */
    SET NDCMED2 (KEEP=RXNDC RXNAME_C
                 RENAME=(RXNDC=NDC2 RXNAME_C=MEDNAME2)) POINT=I;
    IF (NDC1 EQ NDC2) OR (RXNAME_C EQ MEDNAME2) THEN DO;
       IF (USED(I) NE .) AND (USED(I) NE PARTITIO_WEB) THEN DO;
          DO J = 1 TO &NOBS;
             IF (USED(J) EQ USED(I)) AND (J NE I) THEN USED(J) = PARTITIO_WEB;
          END;
       END;
       USED(I) = PARTITIO_WEB;
    END;
  END;

  /* Create a dataset with N observations containing partition group variable */
  IF EOF THEN DO;
    DO I = 1 TO &NOBS;
      OBSNO = I;
      PARTITIO_WEB = USED(I);
      OUTPUT;
    END;
  END;
RUN;

*-----------------------------------------------------------------------------------------
* Merge partition number back onto PMED file.
*-----------------------------------------------------------------------------------------;
DATA NDCMED (DROP=OBSNO);
  MERGE NDCMED1
        NDCPART;
  BY OBSNO;
RUN;

PROC SORT DATA=NDCMED; BY RXNDC; RUN;

DATA NDCPART (KEEP=RXNDC PARTITIO_WEB RXNAME_C RENAME=(RXNAME_C=NDCMED));
  SET NDCMED;
  BY RXNDC;
  IF FIRST.RXNDC;
  LABEL RXNAME_C = "Most frequently occurring RXNAME_C for PARTITIO_WEB";
RUN;

DATA PMEDS;
  MERGE PMEDS NDCPART;
  BY RXNDC;
  LENGTH PARTMISS_WEB 3;
  LABEL  PARTMISS_WEB = "1 if PARTITIO_WEB is missing, else 0";
  PARTMISS_WEB = (PARTITIO_WEB EQ .);
RUN;

PROC SORT DATA=PMEDS;
		BY PARTITIO_WEB RXNDC NDCMED; 
RUN;

/* Recalculate NDCMED using RXNAME */
PROC SUMMARY DATA=PMEDS NWAY MISSING;
  WHERE RXNAME NE "-9";
  BY PARTITIO_WEB RXNDC;
  CLASS RXNAME;
  OUTPUT OUT=NDCMEDX (KEEP=PARTITIO_WEB RXNDC RXNAME _FREQ_);
RUN;

PROC SORT DATA=NDCMEDX;
   BY PARTITIO_WEB RXNDC DESCENDING _FREQ_; 
RUN;

DATA NDCMEDX;
  SET NDCMEDX;
  BY PARTITIO_WEB RXNDC;
  IF FIRST.RXNDC;
RUN;

DATA PMEDS;
  MERGE PMEDS   (DROP=NDCMED)
        NDCMEDX (KEEP=PARTITIO_WEB RXNDC RXNAME RENAME=(RXNAME=NDCMED));
  BY PARTITIO_WEB RXNDC;
RUN;

PROC SUMMARY DATA=PMEDS NWAY;
  WHERE RXNAME_C NE "-9";
  BY PARTITIO_WEB;
  CLASS RXNAME_C;
  OUTPUT OUT=SMED (KEEP=PARTITIO_WEB RXNAME_C _FREQ_);
RUN;

PROC SORT DATA=SMED;
   BY PARTITIO_WEB DESCENDING _FREQ_; 
RUN;

DATA SMED;
  SET SMED;
  BY PARTITIO_WEB;
  IF FIRST.PARTITIO_WEB;
RUN;

DATA PMEDS;
  MERGE PMEDS SMED (KEEP=PARTITIO_WEB RXNAME_C RENAME=(RXNAME_C=PARTMED_WEB));
  BY PARTITIO_WEB;
  LABEL PARTMED_WEB = "PARTITIO_WEB PC medicine name";
RUN;

*----------------------------------------------------------------------------------------------
* Merge PARTITIO_WEB data onto PUF records BY RXRECID.
*----------------------------------------------------------------------------------------------;
PROC SORT DATA=INOUT.RX&YY.V05 
          OUT=RX&YY.V05; 
   BY RXRECID; 
RUN;

PROC SORT DATA=PMEDS; 
   BY RXRECID; 
RUN;

DATA INOUT.RX&YY.V06;
   MERGE RX&YY.V05 (IN=A)
         PMEDS     (IN=B KEEP=RXRECID PARTITIO_WEB PARTMED_WEB PARTMISS_WEB) END=EOF;
   BY RXRECID;
   
    /* relabel PARTITIO so this variable is not used for the Compendia Tables */
   LABEL PARTITIO = "Groups records of same drug (MEDNAME1/NDCX)";
   
   /* For 2007 data, change PARTMED_WEB of "SUPPLY" to meaningful name --------------*/
   IF PARTMED_WEB = "SUPPLY"
    THEN PARTMED_WEB = "DIABETIC SUPPLY/EQUIPMENT";
   /*--------------------------------------------------------------------------------*/
   
   C1 + (A AND NOT B);
   C2 + (B AND NOT A);
   C3 + (A AND B);

   IF EOF THEN DO;
      PUT 'IN PMEDPUF ONLY: ' C1;
      PUT 'IN PMEDS ONLY:  ' C2;
      PUT 'IN BOTH:        ' C3;
   END;
   
   DROP C1 C2 C3;
RUN;

*-----------------------------------------------------------------------------------------
* Contents and verification output.
*-----------------------------------------------------------------------------------------;
FOOTNOTE1 "DSN=AHRQ2\F_DSO\FC008NKK\DATA\RX&YY.V06.SAS7BDAT";

PROC CONTENTS DATA=INOUT.RX&YY.V06 POSITION;
  TITLE3 "Contents of RX&YY.V06";
RUN;

PROC PRINT DATA=INOUT.RX&YY.V06 (OBS=16);
  TITLE3 "Sample print of RX&YY.V06.SAS7BDAT";
RUN;

PROC FREQ DATA=INOUT.RX&YY.V06;
  TABLES PARTMISS_WEB PARTITIO_WEB / MISSING;
  TABLES PARTMISS_WEB*PARTITIO_WEB*RXNAME / LIST MISSING;
  FORMAT PARTITIO_WEB PARTITIO.;
  FORMAT RXNAME $RXNAME.;
  TITLE3 "Verification of newly created variables";
RUN;

/* Show values of PARTMED_WEB which were edited to be meaningful names ---------------*/
PROC FREQ DATA=PMEDS;
  WHERE PARTMED_WEB="SUPPLY";
  TABLE PARTMED_WEB * RXNDC * RXNAME * RXNAME_C/LIST MISSING;
  TITLE3 "Records with PARTMED_WEB=SUPPLY prior to changing to a meaningful name";
RUN;

PROC SQL;
  CREATE TABLE CHANGED AS
  SELECT RXRECID, PARTMED_WEB, RXNDC, RXHHNAME, RXNAME, MEDNAME1, MEDNAMEC
  FROM INOUT.RX&YY.V06
  WHERE RXRECID IN(SELECT RXRECID FROM PMEDS
  									WHERE PARTMED_WEB="SUPPLY"
  									);
QUIT;

PROC FREQ DATA=CHANGED;
  TABLE PARTMED_WEB * RXNDC * RXHHNAME * RXNAME * MEDNAME1 * MEDNAMEC/LIST MISSING NOPERCENT;
  TITLE3 "Records formerly with PARTMED_WEB=SUPPLY after changing to a meaningful name";
RUN;
 
/*------------------------------------------------------------------------------------*/

*PROC SUMMARY DATA=INOUT.RX&YY.V06 (WHERE=(RXNAME="-9" & PARTMISS_WEB=0)) NWAY MISSING;
PROC SUMMARY DATA=INOUT.RX&YY.V06 (WHERE=( ("1" <= RXNAME <= "999") & PARTMISS_WEB=0)) NWAY MISSING;
  CLASSES RXNAME MEDNAME1 MEDNAMEC RXNDC PARTMED_WEB PARTITIO_WEB;
  OUTPUT OUT=PARTTEST (DROP=_TYPE_);
RUN;
PROC PRINT DATA=PARTTEST;
  SUM _FREQ_;
  TITLE3 "Cases where RXNAME/MEDNAME1 are masked but Rx was assigned to a partition group";
RUN;

PROC FREQ DATA=INOUT.RX&YY.V06;
  TABLES PARTITIO_WEB*PARTMED_WEB / LIST MISSING;
  TITLE3 "Verification of newly created variables";
RUN;

PROC FREQ DATA=INOUT.RX&YY.V06;
  TABLES PARTMED_WEB*PARTITIO_WEB / LIST MISSING;
  TITLE3 "Verification of newly created variables";
RUN;

PROC SUMMARY DATA=INOUT.RX&YY.V06 NWAY MISSING;
  CLASSES PARTITIO_WEB PARTMED_WEB MEDNAMEC RXNAME;
  OUTPUT OUT=PARTXTAB (DROP=_TYPE_);
RUN;

PROC PRINT DATA=PARTXTAB (OBS=1000);
  BY PARTITIO_WEB PARTMED_WEB;
  ID PARTITIO_WEB PARTMED_WEB;
  TITLE3 "Sample xtab for verification of new variables, PARTITIO_WEB & PARTMED_WEB";
RUN;

DATA QCPART;
  SET INOUT.RX&YY.V06 (KEEP=PARTITIO_WEB PARTMED_WEB MEDNAMEC RXNAME RXNDC);
  IF PARTMED_WEB IN ("PROVENTIL","ALBUTEROL",
                     "SULFATRIM","BACTRIM", 
                     "HYDROCHLOROTHIAZIDE",
                     "LANCETS",
                     "ATORVASTATIN","LIPITOR",
                     "AMLODIPINE", "NORVASC",
                     "LEVOTHYROXINE","SYNTHROID",
                     "SIMVASTATIN","ZOCOR");
RUN;

PROC SUMMARY DATA=QCPART NWAY MISSING;
  CLASSES PARTITIO_WEB PARTMED_WEB RXNDC MEDNAMEC RXNAME;
  OUTPUT OUT=RXNDCTAB (DROP=_TYPE_);
RUN;

PROC SORT DATA=RXNDCTAB; BY PARTITIO_WEB; RUN;

PROC PRINT DATA=RXNDCTAB; 
  BY PARTITIO_WEB; 
  LABEL PARTITIO_WEB="PARTITIO_WEB";
  TITLE3 "Verification of PARTITIO_WEB/PARTMED_WEB with imputed NDC & Rx name using selected drug names";
RUN;

%JOBEND;
