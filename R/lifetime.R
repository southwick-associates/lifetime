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

#' Data: 100K customer sample
#'
#' @docType data
#' @keywords datasets
#' @name cust
#' @format data frame with 100K rows
#' @family Data
NULL

#' Data: hunt history from 100K customer sample
#'
#' @docType data
#' @keywords datasets
#' @name hunt
#' @format data frame
#' @family Data
NULL

#' Data: license types of interest for analysis
#'
#' @docType data
#' @keywords datasets
#' @name lic
#' @format data frame
#' @family Data
NULL

#' Data: hunting sales for license types of interest in sample
#'
#' @docType data
#' @keywords datasets
#' @name sale
#' @format data frame
#' @family Data
NULL

#' Data: Retention years results by age
#'
#' @docType data
#' @keywords datasets
#' @name retain
#' @format data frame
#' @family Data
NULL

#' Data: Retention years results by current & future age
#'
#' @docType data
#' @keywords datasets
#' @name retain_all
#' @format data frame
#' @family Data
NULL
