
# lifetime

A Southwick-only R package for assisting in analysis of lifetime license pricing, based on the NC project from late 2019. It includes functions for producing final analysis, as well as functions for evaluating altermative methods; in particular for estimating counterfactual years of license purchases (years by age, retention, logistic model retention).

## Installation

To install package lifetime and it's dependencies from the R console:

``` r
install.packages(c("dplyr", "ggplot2", "devtools"))
devtools::install_github("southwick-associates/lifetime")

# 2 packages may  be useful for preparing license data
devtools::install_github("southwick-associates/salic")
devtools::install_github("southwick-associates/salicprep")
```

## Usage

Maybe include a vignette
