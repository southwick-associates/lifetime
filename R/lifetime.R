# package-level documentation

#' @import dplyr ggplot2
#' @importFrom stats lm predict glm binomial
#' @importFrom utils tail
#' @importFrom mgcv gam s
NULL

# certain functions reference global variables (e.g., named variables in data frames)
# - R CMD check doesn't like this: declaring them in globalVariables() is needed
# - https://github.com/r-lib/devtools/issues/1714
if (getRversion() >= "2.15.1") {
    utils::globalVariables(
        c("method", "stream", "revenue_annual", "revenue_lifetime", "year")
    )
}

#' lifetime: Lifetime pricing analysis
#'
#' See below for a list of the core functions. Vignettes are available on
#' github: \url{https://github.com/southwick-associates/lifetime}
#'
#' @section Retention:
#' \itemize{
#'   \item Data Prep: \code{\link{yrs_zero_split}}, \code{\link{yrs_zero_filter}},
#'   \code{\link{yrs_lifetime_join}}, \code{\link{yrs_avidity}}
#'
#'   \item OLS Time-trend Model: \code{\link{yrs_calc_retain}},
#'   \code{\link{yrs_result_observe}}, \code{\link{yrs_result_retain}}
#'
#'   \item Logistic Model: \code{\link{yrs_calc_renew}},
#'   \code{\link{yrs_result_renew}}
#' }
#'
#' @section Revenue:
#' \itemize{
#'   \item License: \code{\link{lic_annual_stream}}, \code{\link{lic_lifetime_stream}},
#'   \code{\link{lic_lifetime}}
#'
#'   \item WSFR: \code{\link{wsfr_annual_stream}}, \code{\link{wsfr_lifetime_stream}}
#'
#'   \item Lifetime Fund: \code{\link{compound_interest}}, \code{\link{present_value_stream}}
#' }
#'
#' @section NC Analysis:
#' \itemize{
#'   \item Retention: \code{\link{nc_retain_all}}, \code{\link{nc_retain_youth}}
#'
#'   \item Revenue: \code{\link{nc_annual_stream}}, \code{\link{nc_lifetime}},
#'   \code{\link{nc_lifetime_stream}}, \code{\link{nc_price_lifetime_youth}}
#'
#'   \item Break-even: \code{\link{nc_break_even}},
#'   \code{\link{nc_break_even_yrs}}
#' }
#'
#' @docType package
#' @name lifetime
NULL
