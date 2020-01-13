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
    retain_all <- ages %>%
        sapply(function(x) retain_one(history_split, x),
               simplify = FALSE) %>%
        bind_rows()

    # add a year zero 100% (for downstream annual revenue calculations)
    sapply(ages, function(i) {
        x <- filter(retain_all, .data$current_age == i)
        filter(x, .data$years_since == 1) %>%
            mutate(years_since = 0, pct = 1, age_year = i) %>%
            bind_rows(x)
    }, simplify = FALSE) %>%
        bind_rows()
}

#' Simulate retention curves for youths
#'
#' Based on the assumption that youths won't generate revenue until age 16 (default).
#' Any youth years are to be ignored for revenue purposes (other than wsfr lifetime dollars).
#' Retention curves are unknown for youth buyers. The simple assumption is that
#' their curves will match 16-year-olds (at age 16). This likely overestimates
#' their future participation.
#'
#' @inheritParams nc_retain
#' @inheritParams nc_revenue
#' @family wrapper functions for NC results
#' @export
nc_retain_youth <- function(retain_all, youth_ages = 0:15) {
    adult_age <- max(youth_ages) + 1

    first_adult <- retain_all %>%
        filter(.data$current_age == adult_age) %>%
        select(.data$age_year, .data$years_since, .data$pct)

    youth_ages %>%
        sapply(function(i) {
            tibble(age_year = (i):(adult_age-1)) %>%
                mutate(pct = 0) %>%
                bind_rows(first_adult) %>%
                mutate(years_since = row_number()) %>%
                mutate(current_age = i)
        }, simplify = FALSE) %>%
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
#' @param fund_years range of years to cast forward for stream-based lifetime
#' fund valuation
#' @name nc_revenue
#' @family wrapper functions for NC results
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' data(retain_all)
#' data(retain)
#'
#' # parameters
#' senior_price <- 15 # lifetime license price at age 65
#' wsfr_amount <- 16.65 # WSFR estimated aid amount per sportsman
#' min_amount <- 4 # WSFR minimum revenue for certified sportsman calculations
#' return_life <- 0.05 # annual return to lifetime fund
#' inflation <- 0.02 # inflation rate for fund depreciation
#' prices <- tibble(
#'     current_age = 0:63,
#'     price_lifetime = rep(250, 64),
#'     price_annual = rep(36, 64)
#' )
#'
#' # total revenue
#' annual <- nc_annual(retain_all, prices, wsfr_amount, min_amount, senior_price)
#' ggplot(annual, aes(current_age, revenue_annual, fill = stream)) + geom_col()
#'
#' lifetime <- nc_lifetime(prices, wsfr_amount, min_amount, return_life, inflation)
#' ggplot(lifetime, aes(current_age, revenue_lifetime, fill = stream)) + geom_col()
#'
#' # break-even
#' x <- nc_break_even(annual, wsfr_amount, min_amount, return_life, inflation)
#' ggplot(x, aes(current_age, break_even)) + geom_line()
#'
#' # years to break-even (using revenue stream calculations)
#' x <- nc_break_even_yrs(retain_all, prices, senior_price, wsfr_amount,
#'                        min_amount, return_life, inflation)
#' ggplot(x, aes(current_age, yrs_to_break_even)) + geom_col()
NULL

#' @describeIn nc_revenue Stream of Revenue for annual scenario (Revenue|A)
#' @export
nc_annual_stream <- function(
    retain_all, prices, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80, youth_ages = 0:15
) {
    # calculate license & wsfr revenue
    if (!is.null(youth_ages)) {
        retain_all <- nc_retain_youth(retain_all, youth_ages) %>%
            bind_rows(retain_all)
    }
    lic <- retain_all %>%
        left_join(prices, by = "current_age") %>%
        mutate(lic_revenue = .data$price_annual * .data$pct)
    wsfr <- retain_all %>%
        wsfr_annual_stream(wsfr_amount, min_amount, senior_price, senior_age, age_cutoff)

    # combine & reshape
    full_join(wsfr, lic, by = c("current_age", "age_year")) %>%
        select(.data$current_age, .data$age_year, contains("revenue")) %>%
        tidyr::gather(stream, revenue_annual, .data$wsfr_revenue, .data$lic_revenue) %>%
        mutate(revenue_annual = ifelse(is.na(.data$revenue_annual), 0,
                                       .data$revenue_annual))
}

#' @describeIn nc_revenue Total Revenue for annual scenario (Revenue|A)
#'
#' Convenience function to pull total revenue across all years
#' @export
nc_annual <- function(
    retain_all, prices, wsfr_amount, min_amount, senior_price,
    senior_age = 65, age_cutoff = 80, youth_ages = 0:15
) {
    retain_all %>%
        nc_annual_stream(prices, wsfr_amount, min_amount, senior_price,
                         senior_age, age_cutoff, youth_ages) %>%
        group_by(.data$current_age, .data$stream) %>%
        summarise(revenue_annual = sum(.data$revenue_annual)) %>%
        ungroup()
}

#' @describeIn nc_revenue Revenue for lifetime scenario (Revenue|L)
#' @export
nc_lifetime <- function(
    prices, wsfr_amount, min_amount, return_life, inflation,
    perpetuity = TRUE, youth_ages = 0:15, age_cutoff = 80, fund_years = 0:50
) {
    wsfr <- prices %>%
        wsfr_lifetime(wsfr_amount, min_amount, age_cutoff) %>%
        select(-contains("price"))

    if (!is.null(youth_ages)) {
        # youth prices need to be compounded for fund valuation
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
                                        fund_years, inflation)
        )
    }
    full_join(wsfr, lic, by = c("current_age")) %>%
        tidyr::gather(stream, revenue_lifetime, .data$wsfr_revenue, .data$lic_revenue) %>%
        select(.data$current_age, .data$stream, .data$revenue_lifetime)
}

#' @describeIn nc_revenue Stream of Revenue for lifetime scenario (Revenue|L)
#' @export
nc_lifetime_stream <- function(
    prices, wsfr_amount, min_amount, return_life, inflation,
    fund_years = 0:500, youth_ages = 0:15, age_cutoff = 80
) {
    wsfr <- wsfr_lifetime_stream(prices, wsfr_amount, min_amount, age_cutoff)

    if (!is.null(youth_ages)) {
        # youth lifetime license prices need to be compounded for valuation
        prices <- prices %>%
            nc_price_lifetime_youth(return_life, inflation, youth_ages)
    }

    # revenue stream (by year)
    revenue_one_age <- function(row) {
        prices$price_lifetime[row] %>%
            present_value_stream(return_life, fund_years, inflation) %>%
            mutate(
                current_age = prices$current_age[row],
                age_year = .data$current_age + .data$year
            ) %>%
            select(.data$current_age, .data$age_year,
                   lic_revenue = .data$return)
    }
    fund <- 1:nrow(prices) %>%
        sapply(function(row) revenue_one_age(row), simplify = FALSE) %>%
        bind_rows()

    # - youth ages need to be correctly specified
    if (!is.null(youth_ages)) {
        first_adult <- max(youth_ages) + 1
        fund <- fund %>% mutate(age_year = case_when(
            .data$current_age >= first_adult ~ .data$age_year,
            TRUE ~ first_adult - .data$current_age + .data$age_year
        ))
    }
    full_join(wsfr, fund, by = c("current_age", "age_year")) %>%
        tidyr::gather(stream, revenue_lifetime, .data$wsfr_revenue,
                      .data$lic_revenue) %>%
        mutate(revenue_lifetime = ifelse(is.na(.data$revenue_lifetime), 0,
                                         .data$revenue_lifetime))
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
#' @param max_price Maximum lifetime license price used in solver function. A
#' lower number will allow the function to execute more quickly.
#' @family wrapper functions for NC results
#' @export
#' @examples
#' # see ?nc_revenue for an example
nc_break_even <- function(
    annual_revenue, wsfr_amount, min_amount, return_life, inflation,
    perpetuity = TRUE, ignore_wsfr = TRUE, youth_ages = 0:15, age_cutoff = 80,
    max_price = 1000
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
            nc_lifetime(wsfr_amount, min_amount, return_life, inflation,
                        perpetuity, youth_ages, age_cutoff)
        if (ignore_wsfr) lifetime <- filter(lifetime, .data$stream == "lic_revenue")
        abs(sum(lifetime$revenue_lifetime) - sum(annual$revenue_annual))
    }
    # - get break-even price for given age
    break_even <- function(slct_age) {
        stats::optimize(revenue_effect, c(0, max_price), age = slct_age)$minimum
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
#' represents the number of years it takes for this to happen. Only rows (ages)
#' where the current price is above the break-even price will be returned
#' (otherwise to lifetime return will never reach the annual return scenario).
#'
#' @inheritParams nc_break_even
#' @inheritParams nc_revenue
#' @family wrapper functions for NC results
#' @export
#' @examples
#' # see ?nc_revenue
nc_break_even_yrs <- function(
    retain_all, prices, senior_price, wsfr_amount, min_amount, return_life, inflation,
    ignore_wsfr = TRUE, youth_ages = 0:15, age_cutoff = 80, fund_years = 0:200,
    senior_age = 65
) {
    # calculate revenue streams
    annual <- nc_annual_stream(
        retain_all, prices, wsfr_amount, min_amount, senior_price,
        senior_age, age_cutoff, youth_ages
    )
    lifetime <- nc_lifetime_stream(
        prices, wsfr_amount, min_amount, return_life, inflation,
        fund_years, youth_ages, age_cutoff
    )

    # combine
    if (ignore_wsfr) {
        annual <- filter(annual, .data$stream != "wsfr_revenue")
        lifetime <- filter(lifetime, .data$stream != "wsfr_revenue")
    }
    revenue <- full_join(
        group_by(annual, .data$current_age, .data$age_year) %>%
            summarise(revenue_annual = sum(.data$revenue_annual)) %>%
            ungroup(),
        group_by(lifetime, .data$current_age, .data$age_year) %>%
            summarise(revenue_lifetime = sum(.data$revenue_lifetime)) %>%
            ungroup(),
        by = c("current_age", "age_year")
    )

    # calculate cumulative revenue
    na_to_zero <- function(x) ifelse(is.na(x), 0, x)
    revenue <- revenue %>%
        mutate_at(c("revenue_annual", "revenue_lifetime"), "na_to_zero") %>%
        arrange(.data$current_age, .data$age_year) %>%
        group_by(.data$current_age) %>%
        mutate_at(c("revenue_annual", "revenue_lifetime"), "cumsum") %>%
        ungroup()

    # determine first year where lifetime > annual (if ever)
    revenue %>%
        filter(revenue_lifetime > revenue_annual) %>%
        arrange(.data$current_age, .data$age_year) %>%
        group_by(.data$current_age) %>%
        slice(1L) %>%
        ungroup() %>%
        mutate(yrs_to_break_even = .data$age_year - .data$current_age)
}
