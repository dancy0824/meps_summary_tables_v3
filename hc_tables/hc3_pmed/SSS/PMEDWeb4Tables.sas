*********************************************************************************************
Software Version:   9.2
Creation Date:      2008-08-29              
Task:               AH3.FD001 
Task Title:         AH3.FD001: 2007 Prescribed Medicines Event PUF

Program Name:  AHRQ3\F_DSO\FD001NKK\Prog\PMEDWeb4Tables.sas

Description:   Create Prescription Drug Estimates Tables for Web Site

Programmer:    Ed Hock 

Reference:     

Input files:   AHRQ3\F_DSO\FD001NKK\Data\Tables\top10rx07_use.sas7bdat		
               AHRQ3\F_DSO\FD001NKK\Data\Tables\top25rx07_exp.sas7bdat
               AHRQ3\F_DSO\FD001NKK\Data\Tables\top10tc07_use.sas7bdat		
               AHRQ3\F_DSO\FD001NKK\Data\Tables\top10tc07_exp.sas7bdat
               AHRQ3\F_DSO\FD001NKK\Data\Fmtlib\rx07_usefmt.sas7bcat
               AHRQ3\F_DSO\FD001NKK\Data\Fmtlib\rx07_expfmt.sas7bcat
               AHRQ3\F_DSO\FD001NKK\Data\Fmtlib\tc07_usefmt.sas7bcat
               AHRQ3\F_DSO\FD001NKK\Data\Fmtlib\tc07_expfmt.sas7bcat

Output files:  AHRQ3\F_DSO\FD001NKK\Out\hcdrugest_totexp2007.htm
               AHRQ3\F_DSO\FD001NKK\Out\hcdrugest_totpur2007.htm
               AHRQ3\F_DSO\FD001NKK\Out\hctcest_totexp2007.htm
               AHRQ3\F_DSO\FD001NKK\Out\hctcest_totpur2007.htm

Update History:  Blaine Byars
							2008/11 Add censoring and footnotes
*********************************************************************************************;
%JOBSTART
OPTIONS LS=132 PS=79 MPRINT COMPRESS=NO;
%LET YEAR=2008;
LIBNAME FMT 'C:\AHRQ3\F_DSO\FD001NKK\Data\Fmtlib';
LIBNAME IN 'C:\AHRQ3\F_DSO\FD001NKK\Data\TABLES';
FILENAME RXHTMLxp "C:\AHRQ3\F_DSO\FD001NKK\Out\hcdrugest_totexp&YEAR..htm" RECFM=V LRECL=10000;
FILENAME RXHTMLut "C:\AHRQ3\F_DSO\FD001NKK\Out\hcdrugest_totpur&YEAR..htm" RECFM=V LRECL=10000;
FILENAME TCHTMLxp "C:\AHRQ3\F_DSO\FD001NKK\Out\hctcest_totexp&YEAR..htm" RECFM=V LRECL=10000;
FILENAME TCHTMLut "C:\AHRQ3\F_DSO\FD001NKK\Out\hctcest_totpur&YEAR..htm" RECFM=V LRECL=10000;
%LET NRXUSE=10; /* Number of drugs to include in utilization table */
%LET NRXEXP=25; /* Number of drugs to include in expenditure table */
%LET NTCUSE=10; /* Number of therapeutic classes to include in utilization table */
%LET NTCEXP=10; /* Number of therapeutic classes to include in expenditure table */ 
%LET RXUSEF=top10rx%SUBSTR(&YEAR,3,2)_use; /* PMED utilization input file */
%LET RXEXPF=top25rx%SUBSTR(&YEAR,3,2)_exp; /* PMED expenditure input file */
%LET TCUSEF=top10tc%SUBSTR(&YEAR,3,2)_use; /* therapeutic class utilization input file */
%LET TCEXPF=top10tc%SUBSTR(&YEAR,3,2)_exp; /* therapeutic class expenditure input file */ 
%LET TBRXUSE=1.2; /* Table number of utilization table */
%LET TBRXEXP=1.1; /* Table number of expenditure table */
%LET TBTCUSE=2.2; /* Table number of utilization table */
%LET TBTCEXP=2.1; /* Table number of expenditure table */ 

*======================================================================================;
%global typestrt
				typestrc
				;
				
%MACRO header(catg);
/* catg is XP for expense report, or UT for utilization */

 /* Write out beginning HTML */
DATA _NULL_;
  FILE &TYPE.HTML&catg.;
  PUT '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'
    / '<html lang="en">'
    / '<head>'
    / '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">'
  %LET TYPESTRT=;
  %LET TYPESTRC=;
  %IF "&TYPE" EQ "RX" %THEN %DO;
    %IF &NRXUSE EQ &NRXEXP %THEN %DO;
    / "<title>&YEAR Drug Estimates: Top &NRXUSE Drugs</title>"
    %END;
    %ELSE %DO;
    / "<title>&YEAR Drug Estimates: Top Drugs</title>"
    %END;
    %LET TYPESTRT=Prescribed Drugs;
    %LET TYPESTRC=Prescribed<br />drug;
  %END;
  %ELSE %DO;
    %IF &NTCUSE EQ &NTCEXP %THEN %DO;
    / "<title>&YEAR Drug Estimates: Top &NTCUSE Therapeutic Classes</title>"
    %END;
    %ELSE %DO;
    / "<title>&YEAR Drug Estimates: Top Therapeutic Classes</title>"
    %END;
    %LET TYPESTRT=Therapeutic Classes;
    %LET TYPESTRC=Therapeutic<br />class;
  %END;
    / '<link href="/mepsweb/css/meps_style.css" rel="stylesheet" type="text/css">'
    / '</head>'
    / ' '
    / '<body><!--#include virtual="/mepsweb/_include/ahrqheader.html" -->'

    / '<table width="765" border="0" align="center" cellpadding="0" cellspacing="0">'
    / '  <tr bgcolor="#FFFFFF">'
    / '    <td bgcolor="#FFFFFF"><div align="right">'
    / '      <!--#include virtual="/mepsweb/_include/header_NOBC.html" --> '
    / '    </div></td>'
    / '  </tr>'
    / '  <tr>'
    / '    <td valign="top" bgcolor="#FFFFFF">'
    / '      <table width="765"  border="0" align="center" cellpadding="5" cellspacing="0">'
    ;
  STOP;
RUN;
%mend header;
*======================================================================================;
%macro tail(catg);

 /* Write out ending HTML */
DATA _NULL_;
  FILE &TYPE.HTML&catg. MOD;
  PUT ' '
    / '      </td>'
    / '    </tr>'
    / ' '
    / '    </table>    </td>'
    / '  </tr>'
    / '  <tr>'
    / '    <td><!--#include virtual="/mepsweb/_include/footer.html" --></td>'
    / '  </tr>'
    / '</table>'
    / '</body>'
    / '</html>'
    ;
  STOP;
RUN;

%mend tail;
*======================================================================================;

%MACRO ONEFILE(TYPE);

%header(xp);

/* initialize flags for footnotes */
%let rse30=0;
%let less100=0;

 /* Write out expenditures table  */
OPTIONS FMTSEARCH=(FMT.&TYPE%SUBSTR(&YEAR,3,2)_expfmt);
DATA _NULL_;
  FILE &TYPE.HTMLxp MOD;
  SET IN.&&&TYPE.EXPF(WHERE=(RANK LE &&N&TYPE.EXP)) END=EOF;
 
  length tot_exp_ tot_per_ $18;
  
  TOT_EXP_=PUT(TOT_EXP,COMMA16.);
  TOT_PER_=PUT(TOT_PER,COMMA16.);

  IF NSUM LT 100 THEN DO;
    TOT_EXP_='--';
		TOT_PER_='--';
		call symput('less100','1');
  END;
  ELSE do;
  				IF SE_EXP/TOT_EXP*100 GT 30 
  				 	THEN do;
  				 					TOT_EXP_=compress(TOT_EXP_)||"*";
  				 					call symput('rse30','1');
  				 			 end;
  				 			 
    			if se_per/tot_per*100 > 30
    				then do;
    								TOT_PER_=compress(TOT_PER_)||"*";
    								call symput('rse30','1');
    						 end;			
  	  END;

  
  IF _N_ EQ 1 THEN DO;
    PUT '  <tr>'
      / '    <td>&nbsp;</td>'
      / '    <td valign="top" bgcolor="#FFFFFF" class="contentStyle"><div align="center">'
      / '	     <table border="0" align="center" cellpadding="7" >'
      / '        <thead>'
      / '          <tr>'
      / '      <td colspan="4" align="center" nowrap ><span class="contentStyle"><strong>Prescribed Drug Estimates: ' "&YEAR<br>"
      / '			 <hr align="center"></td>'
      / '          </tr>'
      / '          <tr>'
      / '      <td colspan="4" align="center"><b>Table' " &&TB&TYPE.EXP Top &TYPESTRT by Total Expenditures, &YEAR</font></b>"
      / '           <hr align="center"></td>'
      / '          <tr>'
      / '            <td align="center" nowrap> '
      / '              <p align="left"><strong>' "&TYPESTRC</strong></p>"
      / '            </td>'
      / '            <td align="center"  nowrap scope="col"> '
      / '              <p align="center"><strong>Total <br>expenditures<br>ranking<s/trong></p>'
      / '            </td>'
      / '            <td align="center"  nowrap scope="col">'
      / '              <p align="center"><strong>Total<br>expenditures</strong></p>'
      / '            </td>'
      / '            <td align="center"  nowrap scope="col">'
      / '              <p align="center"><strong>Total number<br>of persons<br>with expenditures</strong></p>'
      / '            </td>'
      / '          </tr>'
      / '        </thead>'
      / '        <tbody>'
      / '          <tr>'
      / '           <td colspan="4" scope="row"><hr align="center"></td>'
      / '          </tr>'
      ;
  END;
  LENGTH PMEDSTR $ 50;
  PMEDSTR = PUT(RANK,RANKFMT.);
  P = INDEX(PMEDSTR,' ');
  IF (P LE 3) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));
%IF "&TYPE" EQ "TC" %THEN %DO;
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
  P = INDEX(PMEDSTR,' ');
  IF (P LE 4) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));  
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
%END;
  PUT '    <tr>'
    / '      <td align="left" scope="row"><strong>' PMEDSTR '</td>'
    / '      <td align="center">' RANK 2. '</td>'
    / '      <td align="right">' TOT_EXP_ '</td>'
    / '      <td align="right">' TOT_PER_ '</td>'
    / '    </tr>'
    ;
  IF EOF THEN DO;
    PUT '        </tbody>'
      / '      </table>'
      ;
  END;
RUN;
%put RSE30=&RSE30.;
%put LESS100=&LESS100.;

 /* Write out standard errors for expenditures table  */
DATA _NULL_;
  FILE &TYPE.HTMLxp MOD;
  SET IN.&&&TYPE.EXPF(WHERE=(RANK LE &&N&TYPE.EXP)) END=EOF;
  
  length se_exp_ se_per_ $18;
  
  se_EXP_=PUT(se_EXP,COMMA16.);
  se_PER_=PUT(se_PER,COMMA16.);

  IF NSUM LT 100 THEN DO;
    se_EXP_='--';
		se_PER_='--';
		call symput('less100','1');
  END;
  ELSE do;
  				IF SE_EXP/TOT_EXP*100 GT 30 
  				 	THEN do;
  				 					se_EXP_=compress(se_EXP_)||"*";
  				 					call symput('rse30','1');
  				 			 end;
  				 			 
    			if se_per/tot_per*100 > 30
    				then do;
    								se_PER_=compress(se_PER_)||"*";
    								call symput('rse30','1');
    						 end;			
  	  END;

  IF _N_ EQ 1 THEN DO;
    PUT '      <p class="contentStyle">'
      / ' <table border="0" align="center" cellpadding="6" class="contentStyle">'
      / '  <thead>'
      / '    <tr>'
      / '      <td colspan="3" align="center" nowrap ><b><strong>Standard Errors for Table' " &&TB&TYPE.EXP: Top &TYPESTRT by Total Expenditures, &YEAR" '</strong></b>'
      / '        <hr align="center"></td>'
      / '      </tr>'
      / '          <tr>'
      / '            <td align="center"  nowrap> '
      / '              <p align="left"><strong>' "&TYPESTRC</strong></p>"
      / '            </td>'
      / '            <td align="center"  scope="col">'
      / '              <p align="center"><strong>Total<br>expenditures</strong></p>'
      / '            </td>'
      / '            <td align="center"  nowrap scope="col">'
      / '              <p align="center"><strong>Total number<br>of persons<br>with expenditures</strong></p>'
      / '            </td>'
      / '          </tr>'
      / '      </thead>'
      / '      <tbody>'
      / '          <tr>'
      / '           <td colspan="4" scope="row"><hr align="center"></td>'
      / '          </tr>'
      ;
  END;
  LENGTH PMEDSTR $ 50;
  PMEDSTR = PUT(RANK,RANKFMT.);
  P = INDEX(PMEDSTR,' ');
  IF (P LE 3) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));
%IF "&TYPE" EQ "TC" %THEN %DO;
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
  P = INDEX(PMEDSTR,' ');
  IF (P LE 4) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));  
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
%END;
  PUT '    <tr>'
    / '      <td align="left" scope="row"><strong>' PMEDSTR '</strong></td>'
    / '      <td align="right">' SE_EXP_ '</td>'
    / '      <td align="right">' SE_PER_ '</td>'
    / '    </tr>'
    ;
  IF EOF THEN DO;
    PUT '      </tbody>'
      / '    </table>'
      ;
      %if &less100=1
       %then %do;
      					PUT '<p align="left" class="footnotetext">-- Less than 100 sample cases.</TD>';
      			 %end;
      %if &rse30=1
       %then %do;
      					PUT '<p align="left" class="footnotetext">* Relative standard error equal to or greater than 30%.</TD>';
      			 %end;
  END;
RUN;
%put RSE30=&RSE30.;
%put LESS100=&LESS100.;

%tail(xp);

*======================================================================================;

*======================================================================================;

%header(ut);

/* initialize flags for footnotes */
%let rse30=0;
%let less100=0;

 /* Write out purchases table  */
OPTIONS FMTSEARCH=(FMT.&TYPE%SUBSTR(&YEAR,3,2)_usefmt);
DATA _NULL_;
  FILE &TYPE.HTMLut MOD;
  SET IN.&&&TYPE.USEF(WHERE=(RANK LE &&N&TYPE.USE)) END=EOF;
  
  length tot_use_ tot_per_ $18;
  
  TOT_use_=PUT(TOT_use,COMMA16.);
  TOT_PER_=PUT(TOT_PER,COMMA16.);

  IF NSUM LT 100 THEN DO;
    TOT_use_='--';
		TOT_PER_='--';
		call symput('less100','1');
  END;
  ELSE do;
  				IF SE_use/TOT_use*100 GT 30 
  				 	THEN do;
  				 					TOT_use_=compress(TOT_use_)||"*";
  				 					call symput('rse30','1');
  				 			 end;
  				 			 
    			if se_per/tot_per*100 > 30
    				then do;
    								TOT_PER_=compress(TOT_PER_)||"*";
    								call symput('rse30','1');
    						 end;			
  	  END;

  IF _N_ EQ 1 THEN DO;
    PUT '  <tr>'
      / '    <td>&nbsp;</td>'
      / '    <td valign="top" bgcolor="#FFFFFF" class="contentStyle"><div align="center">'
      / '	     <table border="0" align="center" cellpadding="7" >'
      / '        <thead>'
      / '          <tr>'
      / '      <td colspan="4" align="center" nowrap ><span class="contentStyle"><strong>Prescribed Drug Estimates: ' "&YEAR<br>"
      / '			 <hr align="center"></td>'
      / '          </tr>'
      / '          <tr>'
      / '      <td colspan="4" align="center"><b>Table' " &&TB&TYPE.USE Top &TYPESTRT by Total Purchases, &YEAR</b>"
      / '           <hr align="center"></td>'
      / '          <tr>'
      / '            <td align="center"  nowrap> '
      / '              <p align="left"><strong>' "&TYPESTRC</strong></p>"
      / '            </td>'
      / '            <td align="center" nowrap scope="col"> '
      / '              <p align="center"><strong>Total <br>purchases<br>ranking</strong></p>'
      / '            </td>'
      / '            <td align="center"  scope="col">'
      / '              <p align="center"><strong>Total<br>purchases</strong></p>'
      / '            </td>'
      / '            <td align="center"  nowrap scope="col">'
      / '              <p align="center"><strong>Total number<br>of persons<br>with purchases</strong></p>'
      / '            </td>'
      / '          </tr>'
      / '        </thead>'
      / '        <tbody>'
      ;
  END;
  LENGTH PMEDSTR $ 50;
  PMEDSTR = PUT(RANK,RANKFMT.);
  P = INDEX(PMEDSTR,' ');
  IF (P LE 3) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));
%IF "&TYPE" EQ "TC" %THEN %DO;
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
  P = INDEX(PMEDSTR,' ');
  IF (P LE 4) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));  
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
%END;
  PUT '    <tr>'
    / '      <td align="left" scope="row"><strong>' PMEDSTR '</strong></td>'
    / '      <td align="center">' RANK 2. '</td>'
    / '      <td align="right">' TOT_USE_ '</td>'
    / '      <td align="right">' TOT_PER_ '</td>'
    / '    </tr>'
    ;
  IF EOF THEN DO;
    PUT '        </tbody>'
      / '      </table>'
      ;
  END;
RUN;
%put RSE30=&RSE30.;
%put LESS100=&LESS100.;

 /* Write out standard errors for purchases table  */
DATA _NULL_;
  FILE &TYPE.HTMLut MOD;
  SET IN.&&&TYPE.USEF(WHERE=(RANK LE &&N&TYPE.USE)) END=EOF;
  
  length se_use_ se_per_ $18;
  
  se_use_=PUT(se_use,COMMA16.);
  se_PER_=PUT(se_PER,COMMA16.);

  IF NSUM LT 100 THEN DO;
    se_use_='--';
		se_PER_='--';
		call symput('less100','1');
  END;
  ELSE do;
  				IF SE_use/TOT_use*100 GT 30 
  				 	THEN do;
  				 					se_use_=compress(se_use_)||"*";
  				 					call symput('rse30','1');
  				 			 end;
  				 			 
    			if se_per/tot_per*100 > 30
    				then do;
    								se_PER_=compress(se_PER_)||"*";
    								call symput('rse30','1');
    						 end;			
  	  END;
  	  
  IF _N_ EQ 1 THEN DO;
    PUT '      <p class="contentStyle">'
      / ' <table border="0" align="center" cellpadding="6" class="contentStyle">'
      / '  <thead>'
      / '    <tr>'
      / '      <td colspan="3" align="center" nowrap ><b><strong>Standard Errors for Table' " &&TB&TYPE.USE: Top &TYPESTRT by Total Purchases, &YEAR" '</strong></b>'
      / '        <hr align="center"></td>'
      / '      </tr>'
      / '      		<tr>'
      / '            <td align="center" nowrap> '
      / '              <p align="left"><strong>' "&TYPESTRC</font></strong></p>"
      / '            </td>'
      / '            <td align="center" >'
      / '              <p align="center"><strong>Total<br>purchases</strong></font></p>'
      / '            </td>'
      / '            <td align="center" >'
      / '              <p align="center"><strong>Total number<br>of persons<br>with purchases</strong></font></p>'
      / '            </td>'
      / '          </tr>'
      / '      </thead>'
      / '      <tbody>'
      / '          <tr>'
      / '           <td colspan="4" scope="row"><hr align="center"></td>'
      / '          </tr>'
      ;
  END;
  LENGTH PMEDSTR $ 50;
  PMEDSTR = PUT(RANK,RANKFMT.);
  P = INDEX(PMEDSTR,' ');
  IF (P LE 3) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));
%IF "&TYPE" EQ "TC" %THEN %DO;
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
  P = INDEX(PMEDSTR,' ');
  IF (P LE 4) THEN PMEDSTR = PROPCASE(SUBSTR(PMEDSTR,P+1));  
  PMEDSTR = REVERSE(TRIM(REVERSE(PMEDSTR)));
%END;
  PUT '    <tr>'
    / '      <td align="left" scope="row"><strong>' PMEDSTR '</strong></td>'
    / '      <td align="right">' SE_USE_ '</td>'
    / '      <td align="right">' SE_PER_ '</td>'
    / '    </tr>'
    ;
  IF EOF THEN DO;
    PUT '      </tbody>'
      / '    </table>'
      ;
      %if &less100=1
       %then %do;
      					PUT '<p class="footnotetext">-- Less than 100 sample cases.</TD>';
      			 %end;
      %if &rse30=1
       %then %do;
      					PUT '<p class="footnotetext">* Relative standard error equal to or greater than 30%.</TD>';
      			 %end;
  END;
RUN;
%put RSE30=&RSE30.;
%put LESS100=&LESS100.;

%tail(ut);


%MEND ONEFILE;

%ONEFILE(RX)
%ONEFILE(TC)
