
# lifetime

A Southwick-only R package for lifetime pricing analysis which includes functions for preparing data, estimating retention curves, revenue, and break-even metrics.

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

This package includes several vignettes to guide analysts:

1. [Retention Curves](github-vignettes/retention.md): Estimating retention curves (by age) using linear regression

2. [Revenue](github-vignettes/revenue.md): Estimating future revenue (annual vs. lifetime scenarios)

3. [Full Analysis](github-vignettes/full-analysis.md): Functions to streamline full analysis

After package installation, you can also run `?lifetime::lifetime` for a function reference.

## Methods

The methods are documented in the reference [report](https://southwickassociatesinc.sharepoint.com/:w:/s/NCWRC-19-01ChurnRateAssessment/ERa55K9RePRJp9YWiIi71YQBENPGRW76qZ0_sjX0JQR6pw?e=5gfAnX). Some background work was done to compare alternative estimation methods ("years-by-age" used previously vs. "retain" method used here vs. "logistic" which could directly incorporate avidity): [predicting retention](https://southwickassociatesinc.sharepoint.com/:u:/s/NCWRC-19-01ChurnRateAssessment/EdlMJMh-fqlOo_I9YXOjNusB5EIi5VSvOXMoR0lcM_FUgg?e=ETHtxk) 
