clear
set more off
capture log close

**********************************************************************************
*
*PROGRAM:     \\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP.do
*
*DESCRIPTION: Replicate Shiny Server statistics for:
*			  Total expenditures ($)
*			  by
*			  NONE, DEMOGRAPHICS, HEALTH VARIABLES, and
*			  SOCIO_ECONOMIC STATUS
*
*INPUT FILE:  \\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Data\persdata9614_test.dta
*
*********************************************************************************



log using \\files.s-3.com\HPDA\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP.log, replace

* read in the consolidated data file (1996-2014) *;
use "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Data\persdata_1996_2014.dta", clear

keep totexp year perwtf varpsu varstr agegrps_v2 region married race sex insurance health mental_health education employed poverty

* identify the survey design characteristics *;

svyset [pw=perwtf], strata(varstr) psu(varpsu) 
           
* Sum of expenditures by demographic variables;

gen float btotexp=totexp/1000000000

gen float one=1

* Create loop over year and output to overall file;

** YEAR **;
*** Define results file ;
postfile bs_postfile year All_Sum All_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_year.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(one)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar Sum_tot=round(e_b_[1,1],.1)

scalar Se_tot=round(sqrt(e_v_[1,1]),.1)

post bs_postfile (`year') (Sum_tot) (Se_tot)
}

postclose bs_postfile

* Create loop over year and output to demographic files;

** AGE GROUPS **;
*** Define results file ;
postfile bs_postfile year age0_17_Sum age0_17_SE age18_64_Sum age18_64_SE age65p_Sum age65p_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_age.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(agegrps_v2)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar agegrps_v21_tot=round(e_b_[1,1],.1)
scalar agegrps_v22_tot=round(e_b_[1,2],.1)
scalar agegrps_v23_tot=round(e_b_[1,3],.1)

scalar agegrps_v21_se=round(sqrt(e_v_[1,1]),.1)
scalar agegrps_v22_se=round(sqrt(e_v_[2,2]),.1)
scalar agegrps_v23_se=round(sqrt(e_v_[3,3]),.1)

post bs_postfile (`year') (agegrps_v21_tot) (agegrps_v21_se) (agegrps_v22_tot) (agegrps_v22_se) (agegrps_v23_tot) (agegrps_v23_se)
}

postclose bs_postfile

** REGION **;
*** Define results file ;
postfile bs_postfile year Northeast_Sum Northeast_SE Midwest_Sum Midwest_SE South_Sum South_SE West_Sum West_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_region.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(region)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar region1_tot=round(e_b_[1,1],.1)
scalar region2_tot=round(e_b_[1,2],.1)
scalar region3_tot=round(e_b_[1,3],.1)
scalar region4_tot=round(e_b_[1,4],.1)

scalar region1_se=round(sqrt(e_v_[1,1]),.1)
scalar region2_se=round(sqrt(e_v_[2,2]),.1)
scalar region3_se=round(sqrt(e_v_[3,3]),.1)
scalar region4_se=round(sqrt(e_v_[4,4]),.1)

post bs_postfile (`year') (region1_tot) (region1_se) (region2_tot) (region2_se) (region3_tot) (region3_se) (region4_tot) (region4_se)
}

postclose bs_postfile

** Marital Status **;
*** Define results file ;
postfile bs_postfile year Married_Sum Married_SE Widowed_Sum Widowed_SE Divorced_Sum Divorced_SE Separated_Sum Separated_SE Never_Married_Sum Never_Married_SE Inapplicable_Sum Inapplicable_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_married.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(married)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar married1_tot=round(e_b_[1,1],.1)
scalar married2_tot=round(e_b_[1,2],.1)
scalar married3_tot=round(e_b_[1,3],.1)
scalar married4_tot=round(e_b_[1,4],.1)
scalar married5_tot=round(e_b_[1,5],.1)
scalar married6_tot=round(e_b_[1,6],.1)

scalar married1_se=round(sqrt(e_v_[1,1]),.1)
scalar married2_se=round(sqrt(e_v_[2,2]),.1)
scalar married3_se=round(sqrt(e_v_[3,3]),.1)
scalar married4_se=round(sqrt(e_v_[4,4]),.1)
scalar married5_se=round(sqrt(e_v_[5,5]),.1)
scalar married6_se=round(sqrt(e_v_[6,6]),.1)

post bs_postfile (`year') (married1_tot) (married1_se) (married2_tot) (married2_se) (married3_tot) (married3_se) (married4_tot) (married4_se) (married5_tot) (married5_se) (married6_tot) (married6_se)
}

postclose bs_postfile

** Race/Ethnicity **;
*** Define results file ;

* Recode race to be between 1-6 instead of 1-5,9;
gen race_o=race

replace race=6 if race_o==9

postfile bs_postfile year Hispanic_Sum Hispanic_SE White_Sum White_SE Black_Sum Black_SE AI_AN_Multiple_Sum AI_AN_Multiple_SE AS_HI_PI_Sum AS_HI_PI_SE White_Other_Sum White_Other_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_race.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(race)

matrix e_b_=e(b)
matrix e_v_=e(V)

if inrange(`year',1996, 2001) {
  scalar race1_tot=round(e_b_[1,1],.1) 
  scalar race2_tot=. 
  scalar race3_tot=round(e_b_[1,2],.1)
  scalar race4_tot=.
  scalar race5_tot=.
  scalar race6_tot=round(e_b_[1,3],.1) 
  
  scalar race1_se=round(sqrt(e_v_[1,1]),.1)
  scalar race2_se=.
  scalar race3_se=round(sqrt(e_v_[2,2]),.1)
  scalar race4_se=.
  scalar race5_se=.
  scalar race6_se=round(sqrt(e_v_[3,3]),.1)
  }

if inrange(`year',2002, 2014) {
  scalar race1_tot=round(e_b_[1,1],.1)
  scalar race2_tot=round(e_b_[1,2],.1)
  scalar race3_tot=round(e_b_[1,3],.1)
  scalar race4_tot=round(e_b_[1,4],.1)
  scalar race5_tot=round(e_b_[1,5],.1)
  scalar race6_tot=round(e_b_[1,6],.1)
  
  scalar race1_se=round(sqrt(e_v_[1,1]),.1)
  scalar race2_se=round(sqrt(e_v_[2,2]),.1)
  scalar race3_se=round(sqrt(e_v_[3,3]),.1)
  scalar race4_se=round(sqrt(e_v_[4,4]),.1)
  scalar race5_se=round(sqrt(e_v_[5,5]),.1)
  scalar race6_se=round(sqrt(e_v_[6,6]),.1)
  }

post bs_postfile (`year') (race1_tot) (race1_se) (race2_tot) (race2_se) (race3_tot) (race3_se) (race4_tot) (race4_se) (race5_tot) (race5_se) (race6_tot) (race6_se)
}

postclose bs_postfile

** Sex **;
*** Define results file ;
postfile bs_postfile year Male_Sum Male_SE Female_Sum Female_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_sex.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(sex)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar sex1_tot=round(e_b_[1,1],.1)
scalar sex2_tot=round(e_b_[1,2],.1)

scalar sex1_se=round(sqrt(e_v_[1,1]),.1)
scalar sex2_se=round(sqrt(e_v_[2,2]),.1)

post bs_postfile (`year') (sex1_tot) (sex1_se) (sex2_tot) (sex2_se)
}

postclose bs_postfile

** INSURANCE COVERAGE **;
*** Define results file ;
postfile bs_postfile year Private_Sum Private_SE Public_Sum Public_SE Uninsured_Sum Uninsured_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_insurance.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(insurance)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar insurance1_tot=round(e_b_[1,1],.1)
scalar insurance2_tot=round(e_b_[1,2],.1)
scalar insurance3_tot=round(e_b_[1,3],.1)

scalar insurance1_se=round(sqrt(e_v_[1,1]),.1)
scalar insurance2_se=round(sqrt(e_v_[2,2]),.1)
scalar insurance3_se=round(sqrt(e_v_[3,3]),.1)

post bs_postfile (`year') (insurance1_tot) (insurance1_se) (insurance2_tot) (insurance2_se) (insurance3_tot) (insurance3_se)
}

postclose bs_postfile

** Perceived Health Status **;
*** Define results file ;
postfile bs_postfile year Excellent_Sum Excellent_SE Very_Good_Sum Very_Good_SE Good_Sum Good_SE Fair_Sum Fair_SE Poor_Sum Poor_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_health.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(health)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar health1_tot=round(e_b_[1,1],.1)
scalar health2_tot=round(e_b_[1,2],.1)
scalar health3_tot=round(e_b_[1,3],.1)
scalar health4_tot=round(e_b_[1,4],.1)
scalar health5_tot=round(e_b_[1,5],.1)

scalar health1_se=round(sqrt(e_v_[1,1]),.1)
scalar health2_se=round(sqrt(e_v_[2,2]),.1)
scalar health3_se=round(sqrt(e_v_[3,3]),.1)
scalar health4_se=round(sqrt(e_v_[4,4]),.1)
scalar health5_se=round(sqrt(e_v_[5,5]),.1)

post bs_postfile (`year') (health1_tot) (health1_se) (health2_tot) (health2_se) (health3_tot) (health3_se) (health4_tot) (health4_se) (health5_tot) (health5_se)
}

postclose bs_postfile

** Mental Health Status **;
*** Define results file ;
postfile bs_postfile year Excellent_Sum Excellent_SE Very_Good_Sum Very_Good_SE Good_Sum Good_SE Fair_Sum Fair_SE Poor_Sum Poor_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_mental_health.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(mental_health)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar mental_health1_tot=round(e_b_[1,1],.1)
scalar mental_health2_tot=round(e_b_[1,2],.1)
scalar mental_health3_tot=round(e_b_[1,3],.1)
scalar mental_health4_tot=round(e_b_[1,4],.1)
scalar mental_health5_tot=round(e_b_[1,5],.1)

scalar mental_health1_se=round(sqrt(e_v_[1,1]),.1)
scalar mental_health2_se=round(sqrt(e_v_[2,2]),.1)
scalar mental_health3_se=round(sqrt(e_v_[3,3]),.1)
scalar mental_health4_se=round(sqrt(e_v_[4,4]),.1)
scalar mental_health5_se=round(sqrt(e_v_[5,5]),.1)

post bs_postfile (`year') (mental_health1_tot) (mental_health1_se) (mental_health2_tot) (mental_health2_se) (mental_health3_tot) (mental_health3_se) (mental_health4_tot) (mental_health4_se) (mental_health5_tot) (mental_health5_se)
}

postclose bs_postfile

** Education **;
*** Define results file ;
postfile bs_postfile year LT_High_School_Sum LT_High_School_SE High_School_Sum High_School_SE Some_College_Sum Some_College_SE Inapplicable_Sum Inapplicable_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_education.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(education)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar education1_tot=round(e_b_[1,1],.1)
scalar education2_tot=round(e_b_[1,2],.1)
scalar education3_tot=round(e_b_[1,3],.1)
scalar education4_tot=round(e_b_[1,4],.1)

scalar education1_se=round(sqrt(e_v_[1,1]),.1)
scalar education2_se=round(sqrt(e_v_[2,2]),.1)
scalar education3_se=round(sqrt(e_v_[3,3]),.1)
scalar education4_se=round(sqrt(e_v_[4,4]),.1)

post bs_postfile (`year') (education1_tot) (education1_se) (education2_tot) (education2_se) (education3_tot) (education3_se) (education4_tot) (education4_se)
}

postclose bs_postfile

** Employment Status **;
*** Define results file ;
postfile bs_postfile year Employed_Sum Employed_SE Not_employed_Sum Not_employed_SE Inapplicable_Sum Inapplicable_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_employed.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(employed)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar employed1_tot=round(e_b_[1,2],.1)
scalar employed2_tot=round(e_b_[1,3],.1)
scalar employed3_tot=round(e_b_[1,1],.1)

scalar employed1_se=round(sqrt(e_v_[2,2]),.1)
scalar employed2_se=round(sqrt(e_v_[3,3]),.1)
scalar employed3_se=round(sqrt(e_v_[1,1]),.1)

post bs_postfile (`year') (employed1_tot) (employed1_se) (employed2_tot) (employed2_se) (employed3_tot) (employed3_se)
}

postclose bs_postfile

** Poverty **;
*** Define results file ;
postfile bs_postfile year Neg_or_Poor_Sum Neg_or_Poor_SE Near_Poor_Sum Near_Poor_SE Low_income_Sum Low_income_SE Middle_income_Sum Middle_income_SE High_income_Sum High_income_SE using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_poverty.dta", replace
  
forvalues year=1996/2014 {
svy: total btotexp if year == `year', over(poverty)

matrix e_b_=e(b)
matrix e_v_=e(V)

scalar poverty1_tot=round(e_b_[1,1],.1)
scalar poverty2_tot=round(e_b_[1,2],.1)
scalar poverty3_tot=round(e_b_[1,3],.1)
scalar poverty4_tot=round(e_b_[1,4],.1)
scalar poverty5_tot=round(e_b_[1,5],.1)

scalar poverty1_se=round(sqrt(e_v_[1,1]),.1)
scalar poverty2_se=round(sqrt(e_v_[2,2]),.1)
scalar poverty3_se=round(sqrt(e_v_[3,3]),.1)
scalar poverty4_se=round(sqrt(e_v_[4,4]),.1)
scalar poverty5_se=round(sqrt(e_v_[5,5]),.1)

post bs_postfile (`year') (poverty1_tot) (poverty1_se) (poverty2_tot) (poverty2_se) (poverty3_tot) (poverty3_se) (poverty4_tot) (poverty4_se) (poverty5_tot) (poverty5_se)
}

postclose bs_postfile

** WRITE TO EXCEL FILE WITH WORKSHEET FOR EACH SUBGROUP **;

clear 

local subpop "year age region married race sex insurance health mental_health education employed poverty" 

foreach i of local subpop {
 
use "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_`i'.dta"

format *Sum *SE

* Sort in descending order *;
gsort -year

export excel using "\\files.s-3.com\hpda\AHRQ\AHRQ4\FF004ETH\Prog\Stata\Cindy\TOTEXP_Demographics.xlsx", sheet("`i'") sheetreplace firstrow(var)

clear

}

log close
exit, clear