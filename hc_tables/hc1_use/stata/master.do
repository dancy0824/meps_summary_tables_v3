cd "C:\Users\emily.mitchell\Desktop\Programming\GitHub\meps_summary_tables\hc_tables\code\stata_code"

sysuse auto
global var price
do "subset.do" \\ whatever directory above program is in


set more off
use "S:\CFACT\Shared\PUF Stata files\h171.dta", clear
svyset varpsu [pweight=perwt14f], str(varstr)
svy: total totexp14
