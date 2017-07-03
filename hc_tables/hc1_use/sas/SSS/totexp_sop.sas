%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\totexp_sop.sas                                                                                                
 * Description :    SAS program to calculate one estimate (TOTEXP) by source of payment for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    no                                                      
 * Programmer  :   Sample program from  Zhengyi Fang Modified by Darya Leyzarovich                                                                                                                                     
 * Date        :    5/17/2017
*********************************************************************;
OPTIONS LS=160 PS=60 OBS=max;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";

libname est "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";

title1 "AH4.FF004";




%MACRO YY (YEAR=, exp_evnt=, fmt=);

TITLE3 "YEAR=&YEAR";
DATA FY&YEAR;
  SET TASK.PERSDATA9614 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
RUN;

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR nomcar mean stderr SUM STD sumwgt;
VAR &exp_evnt;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
/*
DOMAIN &domain_var;*/
ODS OUTPUT statistics =OUT1;
RUN;
ODS LISTING;

DATA OUT&YEAR (keep=year sum_bil stddev_bil demo1 demo2 var1 var2);

  SET OUT1;
  
   LENGTH DEMO1 demo2 $50  var1 var2 $20;
 
 demo1="total";
 
 demo2=lowcase(compress("&fmt"));
 
 var1="ind";
 var2="sop";
 
    sum_bil=sum/1000000000;
    stddev_bil=stddev/1000000000;
    sum_bil=round(sum_bil, .1);
    stddev_bil=round(stddev_bil, .1);
 
  
    YEAR=&YEAR;
    
 
RUN;


    
 %MEND YY;
 
 %macro demog(exp=, fmt1=);
 
 %YY (YEAR=2014, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2013, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2012, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2011, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2010, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2009, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2008, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2007, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2006, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2005, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2004, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2003, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2002, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2001, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=2000, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=1999, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=1998, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=1997, exp_evnt=&exp, fmt=&fmt1);
 %YY (YEAR=1996, exp_evnt=&exp, fmt=&fmt1);
 
 data comb_&exp;
   set 
   out2014
   out2013
   out2012
   out2011
   out2010
   out2009
   out2008
   out2007
   out2006
   out2005
   out2004
   out2003
   out2002
   out2001
   out2000
   out1999
   out1998
   out1997
   out1996;
 run;
 
 TITLE3 "total expenditures and SE for &fmt1, for 1996-2014";
 proc print data=comb_&exp noobs label;
 /*
 format     sum: comma20.0  stddev: comma16.0;*/
 run;
 
 /*merge all event types*/
 
 
proc sort data=comb_&exp;
by year;
run;


 
 data comb;
%if "&exp"="sop1exp" %then %do;
set comb_&exp;
%end;

%else %do;
 set comb comb_&exp;
 by year;
 %end;
 run;

 
 
 
 %mend;
 
 %demog(exp=sop1exp, fmt1=SLF);
 %demog(exp=sop2exp, fmt1=PTR);
 %demog(exp=sop3exp, fmt1=MCR);
 %demog(exp=sop4exp, fmt1=MCD);
 %demog(exp=sop5exp, fmt1=OTZ);
 
  proc sort data=comb;
 by descending year;
 run;
 
 
  TITLE3 "total expenditures and SE for all sop, for 1996-2014";
 proc print data=comb(obs=20) noobs ;
 /*
 format     sum: comma20.0  stddev: comma16.0;*/
 run;
 
 proc sort data=comb nodupkey;
by demo1 demo2 year;
run;


data fyc9614;
set est.fyc9614(where=(grp1="ind" and grp2="sop"));
    totexp_bil=totexp/1000000000;
    totexp_se_bil=totexp_se/1000000000;
    totexp_bil=round(totexp_bil, .1);
    totexp_se_bil=round(totexp_se_bil, .1);

demo1=lowcase(compress(levels1));
demo2=lowcase(compress(levels2));



run;


proc sort data=fyc9614 nodupkey;
by demo1 demo2 year;
run;

data qc(keep=demo1 demo2 year sum_bil stddev_bil totexp_bil totexp_se_bil var1 var2 grp1 grp2 diff_totexp diff_totexp_se);
merge comb (in=a) fyc9614 (in=b);
by demo1 demo2 year;
if b;

diff_totexp=abs(totexp_bil-sum_bil);
diff_totexp_se=abs(totexp_se_bil-stddev_bil);

run;


proc print data=qc(obs=20);
run;


proc print data=qc(where=(diff_totexp=.)    obs=25);
title2 "sample print of records that did not find match between the files";
run;

/*subset on records where expenditures did not match and export*/




data unmatch_cross1;
set qc;

if diff_totexp>0   or  diff_totexp_se>0;
run;
 


proc export data=unmatch_cross1
outfile="\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Darya\QC\totsop_unmatch.xls"
dbms= excel replace;
run;

data nomatch;
set qc;

if diff_totexp=.;
run;


proc export data=nomatch 
outfile="\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Darya\QC\check6.xls"
dbms= excel replace;
run;

 

%jobend;
