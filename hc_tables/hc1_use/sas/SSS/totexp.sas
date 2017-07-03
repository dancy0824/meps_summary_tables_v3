%JOBSTART;
*********************************************************************
 * Task Number :    AH4.FF004                                                                                                                        
 * Program Name:    \\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Sample.sas                                                                                                
 * Description :    Sample SAS program to calculate one estimate (TOTEXP) by 14 groups for 1996-2014                                                                                                                                                                                                  
 * Input Data:	   \\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data\PERSdata9614.sas7bdat  
 * Output Data :    no                                                      
 * Programmer  :   Sample program from  Zhengyi Fang Modified by Darya Leyzarovich                                                                                                                                     
 * Date        :    5/12/2017
*********************************************************************;
OPTIONS LS=160 PS=60 OBS=max;
libname task "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Data";

libname est "\\derived.ahrq.local\Derived\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\R_Estimates\V4";

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
.='MISSING'
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
.="MISSING" 
1='EXCELLENT' 
2='VERY GOOD' 
3='GOOD' 
4='FAIR' 
5='POOR'
;

VALUE MENTAL_HEALTH
.="MISSING"
1='EXCELLENT'
2='VERY GOOD'
3='GOOD'
4='FAIR'
5='POOR'
;

VALUE EMPLOYED
.='MISSING'
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
.='MISSING'
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


%MACRO YY (YEAR=, domain_var=, fmt=);

TITLE3 "YEAR=&YEAR";
DATA FY&YEAR;
  SET TASK.PERSDATA9614 (WHERE=(YEAR=&YEAR));
  TOTAL=1;
RUN;

ODS LISTING CLOSE;
PROC SURVEYMEANS DATA=FY&YEAR nomcar mean stderr SUM STD sumwgt;
VAR TOTEXP;
CLUSTER VARPSU; 
STRATA VARSTR;
WEIGHT PERWTF;
DOMAIN &domain_var;
ODS OUTPUT domain=OUT1;
RUN;
ODS LISTING;

DATA OUT&YEAR (keep=year demo1 demo2 stddev sum var1 var2 stddev_bil  sum_bil);
  SET OUT1;
  
   LENGTH DEMO1 demo2 $50  var1 var2 $20;
   if "&domain_var"="total" then
    demo2="&fmt";
    else demo2=lowcase(compress(&fmt));
    
    demo1="total";
    
    sum_bil=sum/1000000000;
    stddev_bil=stddev/1000000000;
    sum_bil=round(sum_bil, .1);
    stddev_bil=round(stddev_bil, .1);
    
    
    
    var1="ind";
    var2="&domain_var";
    
    YEAR=&YEAR;
    
    
RUN;





    
 %MEND YY;
 
 %macro demog(domain=, fmt1=);
 
 %YY (YEAR=2014, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2013, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2012, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2011, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2010, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2009, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2008, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2007, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2006, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2005, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2004, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2003, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2002, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2001, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=2000, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=1999, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=1998, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=1997, domain_var=&domain, fmt=&fmt1);
 %YY (YEAR=1996, domain_var=&domain, fmt=&fmt1);
 
 data comb_&domain;
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
 
 
 TITLE3 "total expenditures and SE for &domain, for 1996-2014";
 proc print data=comb_&domain noobs label;
 format     totexp: comma20.0  se: comma16.0;
 run;
 
 data comb;
%if "&domain"="total"   %then %do;
set comb_&domain;
%end;

%else %do;
 set comb comb_&domain;
 %end;
 run;
 
 %mend;
 
 %demog(domain=total, fmt1=total);
 %demog(domain=AGEGRPS, fmt1=PUT(AGEGRPS, AGEGRPS.));
 %demog(domain=AGEGRPS_v2, fmt1=PUT(AGEGRPS_V2, AGEGRPS_V2X.));
 %demog(domain=region, fmt1=PUT(REGION, REGION.));
 %demog(domain=married, fmt1=PUT(MARRIED, MARRIED.));
 %demog(domain=race, fmt1=PUT(RACE, RACE.));
 %demog(domain=sex, fmt1=PUT(SEX, SEX.));
 %demog(domain=insurance, fmt1=PUT(INSURANCE, INSURANCE.));
 %demog(domain=insurance_v2, fmt1=PUT(INSURANCE_V2, INSURANCE_V2X.));
 %demog(domain=health, fmt1=PUT(HEALTH, HEALTH.));
 %demog(domain=mental_health, fmt1=PUT(MENTAL_HEALTH, MENTAL_HEALTH.));
 %demog(domain=education, fmt1=PUT(EDUCATION, EDUCATION.));
 %demog(domain=employed, fmt1=PUT(EMPLOYED, EMPLOYED.));
 %demog(domain=poverty, fmt1=PUT(POVERTY, POVERTY.));
 
 proc print data=comb(obs=20) noobs;
 title3 "sample print of combined data";
 /*
  format    sum comma20.0  stddev comma16.0;*/
 run;
 
 proc sort data=comb nodupkey;
by demo1 demo2 year;
run;


data fyc9614;
set est.fyc9614(where=(levels1="Total" and grp2 not in ("event", "sop")));

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
outfile="\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Darya\QC\totexp_unmatch.xls"
dbms= excel replace;
run;

data nomatch;
set qc;

if diff_totexp=.;
run;


proc export data=nomatch 
outfile="\\programs.ahrq.local\programs\MEPS\AHRQ4_CY2\F_DSO\FF004ETH\Prog\SAS\Darya\QC\check4.xls"
dbms= excel replace;
run;

 
  
 

%jobend;
