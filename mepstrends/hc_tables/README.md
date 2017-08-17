This folder contains individual apps (folders starting with 'hc') as well as components shared by all apps ([shared](shared))

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
* **app_info.R**: Basic information about app, including title, description, and instructions
* **dictionaries.R**: Dictionaries specific to the app
* **TABLES.Rdata**: Rdata file containing master summary table for app
