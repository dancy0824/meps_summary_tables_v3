This folder contains individual apps (folders starting with 'hc') as well as components shared by all apps ([shared](shared))

## Updating tables

After the yearly release of the Full-Year-Consolidated File (FYC), the app tables can be updated by running (double-clicking) [UPDATE.bat](UPDATE.bat) (on Windows). This is a batch file that runs:
1. [shared/puf_update.R](shared/puf_update.R) to read in most recent updates to Public Use Files (PUF) releases
2. [shared/puf_transfer.R](shared/puf_transfer.R) transfers new PUFs from MEPS website to local folder C:/MEPS (may need to create this folder if it doesn't exist)
3. XX/r/run_USE.R (XX = app folder) to run estimates on new data
4. XX/run_merge.R (XX = app folder) to append new estimates onto existing data


## Apps

The following apps are in progress:
* **Use, Expenditures, and Population**: [hc1_use](hc1_use)
* **Accessibility and Quality of care**: [hc2_care](hc2_care) 
* **Prescription Medicines**: (preliminary stages)
* **Medical Conditions**: (preliminary stages)

Each app folder contains (or will eventually contain) the following components:
* **r**,**sas**,**stata** folders: R/SAS/Stata code and output for generating master summary table
* **app.R**: Main app file defining form elements and default inputs, and calling modules
* **app_code.R**: Functions to select and display R/SAS/Stata code based on user inputs
* **app_info.R**: Basic information about app, including title, description, and instructions (read by [build.R](../build.R) to generate [index.html](../index.html))
* **dictionaries.R**: Dictionaries specific to the app
* **run_merge**: R code to compare and combine tables generated in R/SAS/Stata folders for different statistics, years
* **TABLES.Rdata**: Rdata file containing master summary table for app
