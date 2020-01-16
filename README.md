
# lifetime

A Southwick-only R package for lifetime pricing analysis. It includes functions for producing final analysis, as well as functions for evaluating alternative methods (in particular for estimating future years of license purchases).

## Installation

To install package lifetime and it's dependencies from the R console:

``` r
install.packages(c("tidyverse", "devtools", "mgcv"))
devtools::install_github("southwick-associates/lifetime")

# 2 packages may  be useful for preparing license data
devtools::install_github("southwick-associates/salic")
devtools::install_github("southwick-associates/salicprep")
```

## Usage

A reference project is included on the Server (E:/SA/Projects/NCWRC-19-01). This package also includes several vignettes to guide analysts:

1. [Retention Curves](github-vignettes/retention.md): Estimating retention curves (by age) using linear regression
2. [Revenue](github-vignettes/revenue.md): Estimating future agency revenue (annual vs. lifetime scenarios) for license sales & WSFR aid
3. [NC Analysis](github-vignettes/nc-analysis.md): Functions to streamline full analysis

## Documentation

Some work was done as part of the NC project in late 2019 to compare alternative estimation methods (older "years-by-age", "retain" method used in NC, vs. "logistic" which could directly incorporate avidity):

- Office 365 > NCWRC-19-01 > Documentation > [Predicting future license buying](https://southwickassociatesinc.sharepoint.com/:u:/s/NCWRC-19-01ChurnRateAssessment/EdlMJMh-fqlOo_I9YXOjNusB5EIi5VSvOXMoR0lcM_FUgg?e=ETHtxk) 
