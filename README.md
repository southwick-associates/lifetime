
# lifetime

A Southwick-only R package for lifetime pricing analysis. It includes functions for producing final analysis, as well as functions for evaluating alternative methods; in particular for estimating counterfactual years of license purchases (years by age, retention, logistic model retention).

## Installation

To install package lifetime and it's dependencies from the R console:

``` r
install.packages(c("dplyr", "ggplot2", "devtools", "mgcv", "scales", "tidyr"))
devtools::install_github("southwick-associates/lifetime")

# 2 packages may  be useful for preparing license data
devtools::install_github("southwick-associates/salic")
devtools::install_github("southwick-associates/salicprep")
```

## Usage

Maybe include a vignette
