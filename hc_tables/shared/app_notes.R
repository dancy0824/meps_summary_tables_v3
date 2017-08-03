######################################
##    ABBREVIATIONS AND MESSAGES    ##
######################################

CFACT <- "Center for Financing, Access and Cost Trends"
AHRQ  <- "Agency for Healthcare Research and Quality"
MEPS  <- "Medical Expenditure Panel Survey"

controlTotals_message <- "(Standard errors are approximately zero for control totals)"

suppressed_message <- " -- Estimates suppressed due to inadequate precision (see <a  target='_blank_'
href = 'https://meps.ahrq.gov/survey_comp/precision_guidelines.shtml'>
FAQs</a> for details).<br>"

rse_message <- " * Relative standard error is greater than 30%.<br>"


###############################
##          NOTES            ##
###############################

notes <- list()


################################
##   Use and Expenditures     ##
################################

notes$totEXP <- notes$meanEXP <- notes$meanEXP0 <- notes$medEXP <- "
Expenditures include payments from all sources to hospitals, 
physicians, other health care providers (e.g. dental care), 
and pharmacies for health services reported by respondents.
"

notes$totEVT <- "
Events include all dental visits, office-based and outpatient visits, inpatient stays, 
emergency room visits, and home health events. A <i>home health event</i> is defined as one month of home health service.
"

notes$event <- "
<i>Other medical expenses</i> are expenses for medical equipment such as eyeglasses, hearing aids, or wheelchairs.
"


###########################################
##   Accessibility and Quality of Care   ##
###########################################

notes$diab_foot <- "
After 2007, the questionnaire for foot care changed slightly, splitting 
<i>No exam in past year</i> into exam <i>More than 1 year ago</i> and 
<i>Never had feet checked</i>.
"

notes$difficulty <- "
Difficulty categories are not mutually exclusive. For instance, a person can have difficulty obtaining both
medical and dental care.
"


################################
##        Demographics        ##
################################


notes$health <- "
A small percentage of persons (< 2 percent) had a missing response for <i>perceived health status</i>.
"

notes$mental_health <- "
A small percentage of persons (< 2 percent) had a missing response for <i>perceived mental health status</i>.
"

notes$married <- "
A small percentage of persons (< 2 percent) had a missing value for <i>marital status</i>.
"

notes$education <- "
A small percentage of persons (< 2 percent) had a missing response for <i>education</i>.
"

notes$employed <- "
A small percentage of persons (< 2 percent) had a missing response for <i>employment status</i>.
"

notes$insurance <- "
<i>Uninsured</i> refers to persons uninsured during the entire year. <br>
<i>Public only</i> and <i>any private</i> health insurance categories refer to individuals with public or private insurance at any time during the period.<br>
Individuals with both public and private insurance (including TRICARE) are classified as having <i>any private</i> insurance.

<i>65+, No Medicare</i> refers to persons age 65+ without Medicare but with private, or Medicaid, or uninsured.
"

# 65+, No Medicare refers to persons age 65+ without Medicare but with private, or Medicaid, or uninsured.         

notes$sop <- "
<i>Private</i> insurance includes Tricare (Armed-Forces-related coverage).<br>

<i>Other</i> includes: 
<ul>
<li> other public programs such as Department of Veterans Affairs (except Tricare)  </li>
<li> other Federal sources (e.g. Indian Health Service, military treatment facilities) </li>
<li> other State and local sources excluding Medicaid (e.g. community and neighborhood clinics, State and local health departments)</li>
<li> worker's compensation</li>
<li> other unclassified sources (e.g., automobile, homeowner's, liability)</li>
</ul>

<i>Other</i> also includes:
<ul>
<li> private insurance payments reported for persons without private health insurance coverage during the year, as defined in MEPS </li>
<li> Medicaid payments reported for persons who were not enrolled in the Medicaid program at any time during the year </li>
</ul
"

notes$poverty <- "
<i>Negative or Poor</i>: Household income below the Federal poverty line.<br>
<i>Near poor</i>: Household income over the poverty line through 125 percent of the poverty line.<br>
<i>Low income</i>: Household income 125 percent through 200 percent of the poverty line.<br>
<i>Middle income</i>: over 200 percent to 400 percent of the poverty line.<br>
<i>High income</i>: over 400 percent of the poverty line.
"

notes$race <- "
All race categories (excluding <i>Hispanic</i>) refer to non-Hispanic groups.

After 2001, the <i>White and Other</i> category was split into three separate groups: 
(i) <i>White</i>, 
(ii) <i>American Indian, Alaska Native, or Multiple Races</i>, and 
(iii) <i>Asian, Hawaiian, or Pacific Islander</i>.
"


