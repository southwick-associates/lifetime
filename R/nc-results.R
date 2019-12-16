# wrapper functions for NC results

# Retention --------------------------------------------------------------

#' Predict retention across a set of ages for NC methodology
#'
#' Convenience functions for the NC analysis, mainly wrappers for
#' \code{\link{yrs_result_retain}}
#'
#' @inheritParams yrs_result
#' @param retain_all full set of retention curve results produced by
#' nc_retain_all()
#' @param ages set of ages for which retention curves will be calculated separately
#' @family wrapper functions for NC results
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(hunt, lic, sale)
#'
#' hunt_split <- hunt %>%
#'     yrs_lifetime_join(sale, lic, "sportsman") %>%
#'     yrs_zero_split() %>%
#'     yrs_zero_filter(function(x) filter(x, life_group == "sportsman"))
#'
#' retain <- nc_retain_all(hunt_split, 40:50) %>% nc_retain()
#' retain %>%
#'     ggplot(aes(current_age, yrs)) +
#'     geom_point() +
#'     ggtitle("Predicted years of purchases by current age")
nc_retain <- function(retain_all) {
    retain_all %>%
        arrange(.data$current_age, .data$years_since) %>%
        group_by(.data$current_age) %>%
        summarise(n0 = first(.data$n), yrs = sum(.data$pct),
                  last_pct = last(.data$pct))
}

#' @describeIn nc_retain Predict full retention curves
#' @export
nc_retain_all <- function(history_split, ages) {
    retain_one <- function(history_split, age_slct) {
        history_split %>%
            yrs_zero_filter(function(x) filter(x, .data$age_year == age_slct)) %>%
            yrs_result_retain(predict_age = age_slct) %>%
            mutate(current_age = age_slct)
    }
    ages %>%
        sapply(function(x) retain_one(history_split, x),
               simplify = FALSE) %>%
        bind_rows()
}

#' Get expected annual revenue-equivalent years for youths
#'
#' This is applied as a way to include annual revenue estimates for infants and
#' youths. It essentially applies a depreciation to the annual revenue estimates,
#' providing break-even estimates that are lower for youths.
#'
#' @param retain retention results like those produced by \code{\link{nc_retain}}
#' @param rate depreciation rate
#' @param ages ages for youth (i.e., those that haven't been already estimated)
#' @family wrapper functions for NC results
#' @export
nc_retain_youth <- function(retain, rate = 0.03, ages = 0:15) {
    retain_16 <- filter(retain, .data$current_age == max(ages) + 1)
    retain_pre16 <- tibble(current_age = ages, last_pct = retain_16$last_pct) %>%
        arrange(desc(.data$current_age)) %>%
        mutate(yrs = compound_interest(retain_16$yrs, -rate, ages+1)) %>%
        arrange(.data$current_age)
    bind_rows(retain_pre16, retain)
}

# Revenue -----------------------------------------------------------------

#' Calculate revenue for annual vs lifetime scenarios
#'
#' @inheritParams wsfr
#' @param perpetuity if TRUE, use a perpetuity calculation
#' (\url{https://en.wikipedia.org/wiki/Perpetuity}) instead of
#' \code{\link{present_value}}
#' @param return_life percentage return from lifetime fund
#' @param inflation inflation rate for depreciation of lifetime fund
#' @param annual_revenue target revenue to solve lifetime break-even price
#' @param ignore_wsfr if TRUE, don't include WSFR dollars in break-even price
#' @name nc_revenue
#' @family wrapper functions for NC results
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(retain)
#'
#' # parameters
#' price_annual <- 36
#' price_lifetime <- 250
#' senior_price <- 15 # lifetime license price at age 65
#' wsfr_amount <- 16.65 # WSFR estimated aid amount per hunter
#' min_amount <- 2 # WSFR minimum revenue for certified hunter calculations
#' prices <- tibble(current_age = 16:63, price_lifetime = rep(price_lifetime, 48),
#'                 price_annual = rep(price_annual, 48))
#' return_life <- 0.05 # annual return to lifetime fund
#' inflation <- 0.02 # inflation rate for fund depreciation
#'
#' # annual
#' annual <- retain %>%
#'     nc_revenue_annual(prices, wsfr_amount, min_amount, senior_price)
#' ggplot(annual, aes(current_age, revenue_annual, fill = stream)) + geom_col()
#'
#' # lifetime
#' lifetime <- prices %>%
#'     nc_revenue_lifetime(wsfr_amount, min_amount, return_life, inflation)
#' ggplot(lifetime, aes(current_age, revenue_lifetime, fill = stream)) +
#'     geom_col()
#'
#' # compare
#' left_join(lifetime, annual, by = c("current_age", "stream")) %>%
#'     mutate(revenue_effect = revenue_lifetime - revenue_annual) %>%
#'     ggplot(aes(current_age, revenue_effect, fill = stream)) +
#'     geom_col()
#'
#' # break-even
#' x <- nc_break_even(annual, wsfr_amount, min_amount, return_life, inflation)
#' ggplot(x, aes(current_age, break_even)) + geom_line()
#'
#' x <- nc_break_even(annual, wsfr_amount, min_amount, return_life, inflation,
#'                    ignore_wsfr = FALSE)
#' ggplot(x, aes(current_age, break_even)) + geom_line()
NULL

#' @describeIn nc_revenue Revenue for annual scenario (R|A)
#' @export
nc_revenue_annual <- function(
    retain, prices, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80
) {
    retain %>%
        wsfr_annual(wsfr_amount, min_amount, senior_price,
                    senior_age, age_cutoff) %>%
        left_join(prices, by = "current_age") %>%
        mutate(lic_revenue = .data$price_annual * .data$yrs) %>%
        tidyr::gather(stream, revenue_annual, .data$wsfr_revenue, .data$lic_revenue) %>%
        select(.data$current_age, .data$stream, .data$revenue_annual)
}

#' @describeIn nc_revenue Revenue for lifetime scenario (R|L)
#' @export
nc_revenue_lifetime <- function(
    prices, wsfr_amount, min_amount, return_life, inflation,
    perpetuity = FALSE
) {
    wsfr <- prices %>%
        wsfr_lifetime(wsfr_amount, min_amount) %>%
        select(-contains("price"))

    if (perpetuity) {
        lic <- prices %>%
            mutate(lic_revenue = .data$price_lifetime * return_life / inflation)
    } else {
        # calculate with a present value
        lic <- prices %>% mutate(
            lic_revenue = present_value(.data$price_lifetime, return_life,
                                        0:50, inflation)
        )
    }
    left_join(wsfr, lic, by = c("current_age")) %>%
        tidyr::gather(stream, revenue_lifetime, .data$wsfr_revenue, .data$lic_revenue) %>%
        select(.data$current_age, .data$stream, .data$revenue_lifetime)
}

#' @describeIn nc_revenue Find price (by current_age) where R|A == R|L
#' @export
nc_break_even <- function(
    annual_revenue, wsfr_amount, min_amount, return_life, inflation,
    perpetuity = FALSE, ignore_wsfr = TRUE
) {
    revenue <- annual_revenue
    if (ignore_wsfr) {
        revenue <- filter(revenue, .data$stream == "lic_revenue")
    }
    ages <- unique(revenue$current_age)

    # define functions for solving lifetime price
    # - calculate revenue effect for a given price-age
    revenue_effect <- function(price, age) {
        annual <- filter(revenue, .data$current_age == age)
        lifetime <- tibble(current_age = age, price_lifetime = price) %>%
            nc_revenue_lifetime(wsfr_amount, min_amount, return_life, inflation,
                                perpetuity)
        if (ignore_wsfr) lifetime <- filter(lifetime, .data$stream == "lic_revenue")
        sum(lifetime$revenue_lifetime) - sum(annual$revenue_annual)
    }
    # - get break-even price for given age
    break_even <- function(age) {
        stats::uniroot(revenue_effect, interval = c(-10000, 10000), age = age)$root
    }
    # - apply across all ages
    break_even <- sapply(ages, break_even)
    tibble(
        current_age = ages, break_even = break_even
    )
}
