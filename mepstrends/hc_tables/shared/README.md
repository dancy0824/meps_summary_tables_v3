This folder contains R code that is shared by all household-component (HC) apps.

[r](r)/[sas](sas)/stata: R/SAS/Stata code for loading FYC files, loading and combining Event files, defining demographic subgroups (e.g. age, race, sex,...), and defining survey design (if applicable). Also contains code-specific shared functions for calculating estimates.

[app_global.R](app_global.R): formatting functions and Shiny 508 functions, built specifically web accessibility and compatibility with the [18F U.S. Web Design Standards](https://standards.usa.gov/)

[app_notes.R](app_notes.R): Abbreviations and dictionaries used in the 'Notes' section of the app (e.g. description of insurance categories)

[app_preamble.R](app_preamble.R): Loads libraries and codes needed for each app

[puf_names.csv](puf_names.csv): MEPS public use files dataset names from [meps_file_names.csv](https://github.com/HHS-AHRQ/MEPS/blob/master/Quick_Reference_Guides/meps_file_names.csv) in the [HHS-AHRQ/MEPS](https://github.com/HHS-AHRQ/MEPS) repository.

[puf_expanded.csv](puf_expanded.csv): An extended version of [puf_names.csv](puf_names.csv), easier to manipulate programmatically
