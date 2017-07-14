This folder contains individual apps (folders starting with 'hc') as well as components shared by all apps ([shared](shared))

[UPDATE.R](UPDATE.R) Reads in the [meps_file_names.csv](https://github.com/HHS-AHRQ/MEPS/blob/master/Quick_Reference_Guides/meps_file_names.csv) from the [HHS-AHRQ/MEPS](https://github.com/HHS-AHRQ/MEPS) repository, then outputs [puf_names.csv](shared/puf_names.csv) and [puf_expanded.csv](shared/puf_expanded.csv) in the [shared](shared) folder.

The following apps are in progress:
* **Use, Expenditures, and Population**: [hc1_use](hc1_use)
* **Accessibility and Quality of care**: [hc2_care](hc2_care) (in progress)
* **Prescription Medicines**: [hc3_pmed](hc3_pmed) (preliminary stages)

Each app folder contains (or will eventually contain) the following components:
* **r**,**sas**,**stata** folders: R/SAS/Stata code and output for generating master summary table
* **app.R**: Main app file defining form elements and default inputs, and calling modules
* **app_code.R**: Functions to select and display R/SAS/Stata code based on user inputs
* **app_info.R**: Basic information about app, including title, description, and instructions (read by [build.R](../build.R) to generate [index.html](../index.html))
* **global.R**: Dictionaries and functions specific to the app, used to run app and generate estimates
* **run_...**: R code to compare and combine tables generated in R/SAS/Stata folders for different statistics, years
* **...TABLES.Rdata**: Rdata file containing master summary table for app

The [shared](shared) folder contains the following components used by all the apps:
* **PUFS**: SAS transport files (.ssp) of original PUFS, read in by R/SAS/Stata codes to create estimates
* **r/sas/stata**: R/SAS/Stata code for loading FYC files, loading and combining Event files, defining demographic subgroups (e.g. age, race, sex,...), and defining survey design (if applicable). Also contains shared functions.
* **app_functions.R**: formatting functions shared across all apps
* **app_functions508.R**: Shiny functions, built specifically web accessibility and compatibility with the [18F U.S. Web Design Standards](https://standards.usa.gov/)
* **app_notes.R**: Abbreviations and dictionaries used in the 'Notes' section of the app (e.g. description of insurance categories)
* **app_preamble.R**: Loads libraries and codes needed for each app, plus a bootstrapPage builder function
* **global.R**: Dictionaries and functions shared across all apps, used to run app and generate estimates
* **puf_transfer.R**: Downloads MEPS Public Use Files (PUFs) from the web and stores them as SAS transfer files (.ssp) (not shown on GitHub)
