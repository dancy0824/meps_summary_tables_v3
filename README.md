This repository contains preliminary steps for building the MEPS summary tables Shiny application.

## Quick Start

To run the app for the **Use, Expenditures, and Population** table, run this code in R (you may need to install the 'shiny' package with `install.packages('shiny')`):

```r
library(shiny)
shiny:::runGitHub("meps_summary_tables_v3","e-mitchell",subdir="hc_tables/hc1_use")
```

> **Note**: Relative links to MEPS summary tables home will not work

## Components

[hc_tables](hc_tables) contains sub-folders for MEPS summary tables apps generated from the household component of MEPS.

[modules](modules) contains Shiny modules for subsetting and filtering data based on user inputs, plus separate modules for Table, Plot, and Code outputs.

[www](www) contains custom CSS, Javascript, and images for Shiny apps.

[index.html](index.html) is the landing page that displays all available MEPS summary tables apps. It is generated with the R code [build.R](build.R), which creates info blocks for each app based on the *app_info.R* code in each folder under [hc_tables](hc_tables).

[template.html](template.html) is the main HTML template for all app pages, including header and footer, plus "lang='en'" attribute for web accessibility.
