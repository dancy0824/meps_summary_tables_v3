## Quick Start

To run the available app locally, run this code in Rstudio:

```r
# Install packages (only need to run this once)
install.packages(c('shiny','plotly','RColorBrewer'))

# Run app: Use, expenditures, and population
shiny:::runGitHub("meps_summary_tables_v3","e-mitchell",subdir="mepstrends/hc_tables/hc1_use")

# Run app: Accessibility and quality of care
shiny:::runGitHub("meps_summary_tables_v3","e-mitchell",subdir="mepstrends/hc_tables/hc2_care")
```

> **Note**: Relative links will not work when running locally

## Components

[mepstrends](mepstrends) is the main folder containing app code needed to run each app on Shiny server.

[build](build) contains code to calculate tabular estimates for each app, plus instructions to update tables when new data years are released.
