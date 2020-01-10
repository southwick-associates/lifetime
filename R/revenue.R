# estimating revenue
# - WSFR: money allocated based on USFWS certified hunters/anglers
# - Lifetime Fund: a perpetuity fund like Oklahoma or North Carolina

# WSFR --------------------------------------------------------------------

#' Calculate WSFR Aid dollars by current age
#'
#' Estimated aid dollars are determined differently for the annual vs. lifetime
#' purchasing scenarios (see functions section below). Links to a web-based
#' reference document are included in details below.
#'
#' These calculations are based on a discussion between Tom, Dan, Eric, and
#' Patty on 12/10/19. Primarily using
#' \url{https://www.law.cornell.edu/cfr/text/50/part-80} to highlight a number
#' of points about WSFR aid predictions for lifetime license sales:
#' \itemize{
#'   \item There is an age cutoff of 80 years (in lieu of a life-expectancy-based
#'   attrition estimate):
#'   \href{https://www.law.cornell.edu/cfr/text/50/80.35}{section 35} bullet H
#'   \item It looks like infants can be counted:
#'   \href{https://www.law.cornell.edu/cfr/text/50/80.33}{section 33} bullet C
#'   \item The required annual amount is $2 for hunt/fish and $4 for combo:
#'   \href{https://www.law.cornell.edu/cfr/text/50/80.34}{section 34} bullet A
#' }
#'
#' @param retain_all data frame of predicted years by age like that produced by
#' \code{\link{nc_retain_all}}
#' @param wsfr_amount numeric estimated amount for aid dollars (use SFRF for
#' fishing, WRF for hunting)
#' @param min_amount numeric the minimum expenditure that will count for a
#' certified hunter/angler (use 2 for hunt/fish and 4 for combo)
#' @param senior_price numeric price for a senior lifetime license
#' @param senior_age numeric age when a participant will be expected to buy
#' a cheap lifetime license
#' @param prices data frame of lifetime prices by age with at least
#' 2 variables: current_age and price_lifetime
#' @param age_cutoff numeric final age that can be counted for lifetime
#' license-based WSFR dollars
#' @name wsfr
#' @family estimating revenue
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(retain_all)
#'
#' # annual scenario
#' aidA <- wsfr_annual(
#'     retain_all, wsfr_amount = 16.65, min_amount = 4, senior_price = 15
#' )
#'
#' # lifetime scenario
#' prices <- tibble(current_age = 16:63, price_lifetime = rep(250, 48))
#' aidL <- wsfr_lifetime(prices, wsfr_amount = 16.65, min_amount = 4)
#'
#' # there are large predicted differences in wsfr revenue outcomes
#' aidL$wsfr_revenue - aidA$wsfr_revenue
NULL

#' @describeIn wsfr Stream of WSFR Aid for annual license scenario
#'
#' This depends largely on the estimated number of years a license buyers will
#' participate in the future. It also assumes by default that any remaining
#' participants will buy a lifetime license at senior_age. The stream calculation
#' forcasts aid for relevant future years.
#' @export
wsfr_annual_stream <- function(
    retain_all, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80
) {
    # determine how many senior years wsfr aid will be collected
    num_yrs <- wsfr_lifetime_yrs(
        senior_price, senior_age, min_amount, age_cutoff
    )
    # the senior aid years will be combined with retain_all
    fwd_one_year <- function(retain_all, yr) {
        retain_all %>%
            filter(.data$age_year == senior_age - 1) %>%
            mutate(age_year = .data$age_year + yr,
                   years_since = .data$years_since + yr)
    }
    retain_senior <- 1:num_yrs %>%
        sapply(function(i) fwd_one_year(retain_all, i), simplify = FALSE) %>%
        bind_rows()

    # calculate aid dollars for every year
    bind_rows(retain_all, retain_senior) %>%
        mutate(wsfr_revenue = .data$pct * wsfr_amount)
}

#' @describeIn wsfr Total WSFR Aid for annual license scenario
#'
#' Convenience function to aggregate stream of revenue to total across years
#' @export
wsfr_annual <- function(
    retain_all, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80
) {
    retain_all %>%
        wsfr_annual_stream(wsfr_amount, min_amount, senior_price, senior_age,
                           age_cutoff) %>%
        group_by(.data$current_age) %>%
        summarise(yrs = sum(.data$pct), wsfr_revenue = sum(.data$wsfr_revenue))
}

#' @describeIn wsfr WSFR Aid for lifetime license scenario
#' @export
wsfr_lifetime_stream <- function(
    prices, wsfr_amount, min_amount, age_cutoff = 80
) {
    # determine years of wsfr revenue remaining by current_age
    prices <- prices %>% mutate(yrs = wsfr_lifetime_yrs(
        .data$price_lifetime, .data$current_age, min_amount, age_cutoff
    ))

    # cast out to future years
    revenue_one_age <- function(prices, age = 16) {
        x <- filter(prices, .data$current_age == age)
        1:x$yrs %>%
            sapply(function(i) mutate(x, years_since = i), simplify = FALSE) %>%
            bind_rows() %>%
            mutate(age_year = age + .data$years_since, wsfr_revenue = wsfr_amount) %>%
            select(.data$current_age, .data$years_since, .data$age_year, .data$wsfr_revenue)
    }
    prices$current_age %>%
        sapply(function(i) revenue_one_age(prices, i), simplify = FALSE) %>%
        bind_rows()
}

#' @describeIn wsfr WSFR Aid for lifetime license scenario
#'
#' Convenience function to aggregate stream of revenue to total across years
#' @export
wsfr_lifetime <- function(
    prices, wsfr_amount, min_amount, age_cutoff = 80
) {
    prices %>%
        wsfr_lifetime_stream(wsfr_amount, min_amount, age_cutoff) %>%
        group_by(.data$current_age) %>%
        summarise(yrs = n(), wsfr_revenue = sum(.data$wsfr_revenue))
}

#' Calculate the number of years to count WSFR dollars with lifetime sales
#'
#' Take the minimum of either (a) price / min_amount, or (b) age_cutoff -
#' current_age, based on 2019 WSFR rules
#'
#' @inheritParams wsfr
#' @param price_lifetime numeric lifetime price
#' @param current_age numeric age
#' @family estimating revenue
#' @seealso wsfr_lifetime
#' @export
#' @examples
#' wsfr_lifetime_yrs(200, current_age = 0, min_amount = 4)
#' wsfr_lifetime_yrs(400, current_age = 40, min_amount = 4)
#' wsfr_lifetime_yrs(400, current_age = 50, min_amount = 4)
wsfr_lifetime_yrs <- function(
    price_lifetime, current_age, min_amount, age_cutoff = 80
) {
    pmin(
        floor(price_lifetime / min_amount),
        age_cutoff - current_age
    )
}

# Lifetime Fund -------------------------------------------------------------

#' Calculate final principal given a rate of return
#'
#' https://en.wikipedia.org/wiki/Compound_interest
#'
#' @param p0 original principal
#' @param r rate of return over some period (e.g., year)
#' @param t time the interest is applied (same units as r). Can be a scalar or
#' a vector
#' @param n compounding frequency. If NULL, will use continuous compounding
#' @family estimating revenue
#' @export
#' @examples
#' # a wikipedia example
#' compound_interest(p0 = 1500, r = 0.043, t = 6, n = 4)
#' compound_interest(1500, 0.043, 6) # continuous
#' compound_interest(1500, 0.043, 0:6) # over time
compound_interest <- function(p0, r, t, n = NULL) {
    if (is.null(n))  {
        # continuous compounding: P(t) = P(0)e^rt
        p0 * exp(r*t)
    } else {
        # periodic compounding
        p0 * (1 + r/n)^(n*t)
    }
}

#' Estimate present value of a fund with annual interest withdrawals
#'
#' Reproduces calculation from Eric's Excel spreadsheet. Assumes that interest
#' generated by the fund is withdrawn at the end of the year. The "_stream"
#' version returns a data frame with 3 columns: year, value = discounted
#' principal, cumulative_return =  #' sum of fund withdrawals to given year.
#'
#' Interesting that the CA lifetime license doesn't work this way, where the
#' dept. receives an allocation equal to one license fee (which means the
#' principal compounds less): https://www.wildlife.ca.gov/Licensing/Lifetime
#' In CA sporspersons also need to pick up a lifetime license, so it sounds like
#' they can directly track participation.
#'
#' Also, I believe that OK and NC could be considered perpetuities: the funds
#' pay interest every year to the agency, and the principal can never be spent.
#' There is a simple equation for valuing perpetuities: PV = A / r
#' https://en.wikipedia.org/wiki/Perpetuity
#'
#' @inheritParams compound_interest
#' @param d discount rate (i.e., inflation)
#' @family estimating revenue
#' @export
#' @examples
#' # Oklahoma fishing lifetimes
#' p0 = 225
#' r = 0.04914
#' d = 0.0219
#' life <- present_value_stream(p0, r, t = 0:49, d)
#' tail(life)
#' present_value(p0, r, 0:49, d)
#' present_value(c(250, 300, 350), r, 0:49, d)
#'
#' # note that a perpetual annuity is valued higher using the same inputs
#' # $414 using 50-year equation above vs. $505 using perpetuity equation below
#' perpetuity <- r * p0 / d
#' perpetuity
#'
#' # perpetuity convergest on the same result given a long enough time span
#' # (i.e., present_value_stream converges on perpetuity calculation)
#' library(dplyr)
#' library(ggplot2)
#' x <- present_value_stream(p0, r, t = 0:300, d)
#' x$return <- x$cumulative_return + x$value
#' ggplot(x, aes(year, return)) +
#'     geom_line() +
#'     geom_label(data = filter(x, year == 50),
#'                aes(label = paste("50-year =", round(return)))) +
#'     ggtitle(paste0("Net Present Value Stream converges on Perpetuity (",
#'                   round(perpetuity), ")"))
present_value_stream <- function(p0, r, t, d, n = 1) {
    # real value of fund, reduces at inflation rate
    # - annual compounding
    val <- compound_interest(p0, -d, t, n)

    # annual return (of inflation-adjusted principal)
    # - annual compounding
    ann_return <- val * r
    cum_return <- cumsum(val * r)

    # output in tabular format
    tibble(year = t, value = val, cumulative_return = cum_return,
           return = ann_return) %>%
        mutate(year = year + 1) # consistent with spreadsheet numbering
}

#' @describeIn present_value_stream Estimate present value in final year
#' @export
present_value <- function(p0, r, t, d) {
    sapply(p0, function(x) {
        x <- present_value_stream(x, r, t, d) %>%
            tail(1)
        x$value + x$cumulative_return
    })
}
