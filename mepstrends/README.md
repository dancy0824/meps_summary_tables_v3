This folder contains code to deploy MEPS summary tables apps on Shiny server.

## Components

[hc_tables](hc_tables) contains sub-folders for MEPS summary tables apps generated from the household component of MEPS.

[lib](lib) has Bootstrap components needed for styling [index.html](index.html)

[modules](modules) contains Shiny modules for subsetting and filtering data based on user inputs, plus separate modules for Table, Plot, and Code outputs.

[www](www) contains custom CSS, Javascript, and images for Shiny apps.

[index.html](index.html) is the landing page that displays all available MEPS summary tables apps. WARNING: Do not edit directly. This file is automatically generated.

[template.html](template.html) is the main HTML template for all app pages, including header and footer, plus "lang='en'" attribute for web accessibility.