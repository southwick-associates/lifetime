% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-YrsAnnual.R
\name{yrs_predict_avg}
\alias{yrs_predict_avg}
\title{Predict license buying - years by age}
\usage{
yrs_predict_avg(avg_yrs, ..., num_yrs = 5, age_cohort = function(x) {  
    cut(x, c(-Inf, 15, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, Inf)) },
  start_age = 31, end_age = 64, method = "years_by_age")
}
\arguments{
\item{avg_yrs}{data frame returned by \code{\link{yrs_calc_avg}}}

\item{...}{optional grouping variables}

\item{num_yrs}{number of years used in avg_yrs}

\item{age_cohort}{function to estimate age_cohort based on history$birth_year
in the first year to be averaged}

\item{start_age}{starting age for years_since = 0}

\item{end_age}{last age to include}

\item{method}{character name of method used (for plotting)}
}
\description{
To be run following \code{\link{yrs_calc_avg}}. Converts the output of that
function to a scale comparable to a retention curve
}
\examples{
data(all_sports)
x <- yrs_calc_avg(all_sports)
yrs_predict_avg(x)
}
\seealso{
Other functions to estimate annual license buying: \code{\link{yrs_avidity}},
  \code{\link{yrs_calc_avg}}, \code{\link{yrs_calc}},
  \code{\link{yrs_fit}}, \code{\link{yrs_lifetime}},
  \code{\link{yrs_plot}}, \code{\link{yrs_predict}},
  \code{\link{yrs_result}}, \code{\link{yrs_zero}}
}
\concept{functions to estimate annual license buying}
