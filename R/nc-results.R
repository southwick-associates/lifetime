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
#' @param use_observed if TRUE, return observed (instead of predicted) retention
#' rates
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
#' observe_all <- nc_retain_all(hunt_split, 52:58, use_observed = TRUE)
#' retain_all <- nc_retain_all(hunt_split, 52:58)
#' retain <- nc_retain(retain_all)
#' retain %>%
#'     ggplot(aes(current_age, yrs)) +
#'     geom_point() +
#'     ggtitle("Predicted years of purchases by current age")
#'
#' # observed vs predicted for specified ages
#' observe <- filter(observe_all, current_age == 52)
#' retain <- filter(retain_all, current_age == 52)
#' yrs_plot(retain) + geom_point(data = observe) +
#'     ggtitle("Some noisy data points due to sample size")
#'
#' observe <- filter(observe_all, current_age == 58)
#' retain <- filter(retain_all, current_age == 58)
#' yrs_plot(retain) + geom_point(data = observe) +
#'     ggtitle("Observed rates are used for old enough buyers",
#'             "Senior lifetime artifacts emerge at age 65")
nc_retain <- function(retain_all) {
    retain_all %>%
        arrange(.data$current_age, .data$years_since) %>%
        group_by(.data$current_age) %>%
        summarise(n0 = first(.data$n), yrs = sum(.data$pct),
                  last_pct = last(.data$pct))
}

#' @describeIn nc_retain Predict full retention curves
#' @export
nc_retain_all <- function(history_split, ages, use_observed = FALSE) {
    retain_one <- function(history_split, age_slct) {
        x <- history_split %>%
            yrs_zero_filter(function(x) filter(x, .data$age_year == age_slct))
        if (use_observed) {
            x <- yrs_result_observe(x, predict_age = age_slct)
        } else {
            x <- yrs_result_retain(x, predict_age = age_slct)
        }
        mutate(x, current_age = age_slct)
    }
    ages %>%
        sapply(function(x) retain_one(history_split, x),
               simplify = FALSE) %>%
        bind_rows()
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
#' @param youth_ages if not NULL, assumes for youths that the fund is able to
#' compound until adulthood (when the agency will begin drawing revenue). See
#' \code{\link{nc_price_lifetime_youth}} for details.
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
#' return_life <- 0.05 # annual return to lifetime fund
#' inflation <- 0.02 # inflation rate for fund depreciation
#' prices <- tibble(
#'     current_age = 0:63, price_lifetime = rep(price_lifetime, 64),
#'     price_annual = rep(price_annual, 64)
#' )
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
#'
#' # years to break-even
#'
NULL

#' @describeIn nc_revenue Revenue for annual scenario (R|A)
#' @export
nc_revenue_annual <- function(
    retain, prices, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80, youth_ages = 0:15
) {
    if (!is.null(youth_ages)) {
        # for youths, assume ages match the first adult age
        first_adult <- retain %>%
            filter(.data$current_age == max(youth_ages) + 1)
        retain <- tibble(
            current_age = youth_ages,
            yrs = first_adult$yrs,
            last_pct = first_adult$last_pct
        ) %>%
            bind_rows(retain)
    }
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
    perpetuity = TRUE, youth_ages = 0:15
) {
    wsfr <- prices %>%
        wsfr_lifetime(wsfr_amount, min_amount) %>%
        select(-contains("price"))

    if (!is.null(youth_ages)) {
        # youth prices need to be compounded for valuation
        prices <- prices %>%
            nc_price_lifetime_youth(return_life, inflation, youth_ages)
    }
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

#' Calculate compounded price for youth lifetimes
#'
#' This function is based on the assumption that youth lifetime sales are
#' treated differently than those for adults. In NC, the revenue for a youth sale
#' cannot be withdrawn (from the fund) until age 16. This implies that a youth
#' lifetime license sale has greater value (since it is able to compound). Therefore
#' the effective price is higher than the sticker price for the purpose of
#' calculating present value of the lifetime license.
#'
#' @inheritParams nc_revenue
#' @family wrapper functions for NC results
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' prices <- tibble(
#'     current_age = c(0:15, 16:63),
#'     price_lifetime = c(rep(250, 64))
#' )
#' prices2 <- nc_price_lifetime_youth(prices, 0.05, 0.022)
#' ggplot(prices, aes(current_age, price_lifetime)) +
#'     geom_line() +
#'     geom_line(data = prices2, color = "blue") +
#'     scale_y_continuous(limits = c(0, 600))
nc_price_lifetime_youth <- function(
    prices, return_life, inflation, youth_ages = 0:15
) {
    adult_age <- max(youth_ages) + 1
    prices %>% mutate(
        price_lifetime = ifelse(.data$current_age %in% youth_ages, compound_interest(
                p0 = .data$price_lifetime,
                r = return_life - inflation,
                t = adult_age - .data$current_age,
                n = 1 # annual compounding
            ), .data$price_lifetime)
    )
}

# Break-even --------------------------------------------------------------

#' Find lifetime price (by current_age) where Revenue|A == Revenue|L
#'
#' This uses a solver function, \code{\link[stats]{uniroot}}, to find break-even
#' prices for lifetime licenses given predicted annual revenue.
#'
#' @inheritParams nc_revenue
#' @param annual_revenue target revenue to solve lifetime break-even price
#' @param ignore_wsfr if TRUE, don't include WSFR dollars in break-even price
#' @family wrapper functions for NC results
#' @export
#' @examples
#' # see ?nc_revenue for an example
nc_break_even <- function(
    annual_revenue, wsfr_amount, min_amount, return_life, inflation,
    perpetuity = TRUE, ignore_wsfr = TRUE, youth_ages = 0:15
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
                                perpetuity, youth_ages)
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

#' Calculate number of years it takes to break even (if ever)
#'
#' Generally it will take some number of years for cumulative revenue in
#' the lifetime scenario to catch up with cumulative revenue in the annual
#' scenario. If the lifetime scenario does catch up, the years to break even
#' represents the number of years it takes for this to happen.
#'
#' @inheritParams nc_break_even
#' @param lifetime_revenue revenue for lifetime scenario
#' @family wrapper functions for NC results
#' @export
#' @examples
#' # see ?nc_revenue for an example
nc_break_even_yrs <- function(
    annual_revenue, lifetime_revenue, ignore_wsfr = TRUE
) {
    revenue <- annual_revenue %>%
        inner_join(lifetime_revenue, by = c("current_age", "stream"))
    if (ignore_wsfr) {
        revenue <- filter(revenue, .data$stream == "lic_revenue")
    }

}
