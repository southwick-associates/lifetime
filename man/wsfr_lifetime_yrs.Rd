% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revenue.R
\name{wsfr_lifetime_yrs}
\alias{wsfr_lifetime_yrs}
\title{Calculate the number of years to count WSFR dollars with lifetime sales}
\usage{
wsfr_lifetime_yrs(price_lifetime, current_age, min_amount,
  age_cutoff = 80)
}
\arguments{
\item{price_lifetime}{numeric lifetime price}

\item{current_age}{numeric age}

\item{min_amount}{numeric the minimum expenditure that will count for a
certified hunter/angler (use 2 for hunt/fish and 4 for combo)}

\item{age_cutoff}{numeric final age that can be counted for lifetime
license-based WSFR dollars}
}
\description{
Take the minimum of either (a) price / min_amount, or (b) age_cutoff -
current_age, based on 2019 WSFR rules
}
\examples{
wsfr_lifetime_yrs(200, current_age = 0, min_amount = 4)
wsfr_lifetime_yrs(400, current_age = 40, min_amount = 4)
wsfr_lifetime_yrs(400, current_age = 50, min_amount = 4)
}
\seealso{
wsfr_lifetime

Other estimating revenue: \code{\link{compound_interest}},
  \code{\link{present_value_stream}}, \code{\link{wsfr}}
}
\concept{estimating revenue}
