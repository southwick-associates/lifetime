# estimating annual license buying

## There is a nomenclature used for these functions:
# - all function names begin with "yrs"

# - followed by operation:
#   + calc = calculate relevant measure (e.g., prep training data)
#   + fit = estimate model parameters (e.g., fit training data)
#   + predict = apply model to relevant data
#   + result = functions that wrap calc, fit, predict for convenience
#   + plot = visualize results

# - followed by the relevant method:
#   + avg = years by age
#   + retain = retention rate (aggregate retention)
#   + renew = renewal identification (customer-level retention)

# Prep Data ---------------------------------------------------------------

#' Estimate a simple avidity measure
#'
#' Avidity is based on years of license purchases prior to the current year.
#'
#' @param history license history table (1 row per customer per year)
#' @param num_yrs number of years to use for defining avidity
#' @param drop_na_yrs if TRUE, records for the years in which avidity can't
#' be calculated will be dropped (e.g., drops first 3 years by default)
#' @family functions to estimate annual license buying
#' @export
#' @examples
#' data(hunt)
#' x <- yrs_avidity(hunt)
#' summary(x$num_years_held)
yrs_avidity <- function(history, num_yrs = 3, drop_na_yrs = TRUE) {
    if ("num_years_held" %in% names(history)) {
        history <- select(history, -.data$num_years_held)
    }
    # identify years to use, can't use first num_yrs years
    all_yrs <- sort(unique(history$year))
    slct_yrs <- setdiff(all_yrs, all_yrs[1:num_yrs])

    # for a license year, identify frequency based on previous num_yrs
    get_num_years_held <- function(history_yr, history, year) {
        if (!year %in% slct_yrs) {
            return(history_yr)
        }
        yrs_freq <- (year-num_yrs):(year-1)
        cnt <- history %>%
            filter(.data$year %in% yrs_freq) %>%
            semi_join(history_yr, by = "cust_id") %>%
            count(.data$cust_id) %>%
            rename(num_years_held = .data$n)
        left_join(history_yr, cnt, by = "cust_id") %>%
            mutate(num_years_held = ifelse(is.na(.data$num_years_held), 0L,
                                           .data$num_years_held))
    }

    # split/apply/combine across each license year
    x <- split(history, history$year)
    out <- names(x) %>%
        sapply(function(nm) get_num_years_held(x[[nm]], history, as.integer(nm)),
               simplify = FALSE) %>%
        bind_rows()
    if (drop_na_yrs) out <- filter(out, !is.na(.data$num_years_held))
    out
}

#' Prepare history table for retention calculation
#'
#' Splitting a history table into two data frames: (1) $year0 is a copy
#' of the input history table while (2) $history is a copy with just 2 columns:
#' cust_id and year. This data structure is intended to allow for easy filtering
#' prior to calculating retention curves (details in section below).
#'
#' The "year zero" framework identifies the characteristics of interest for
#' retention curves. For example, if we look at customers in 2008 (i.e. year0 =
#' 2008), the retention curve is represented by the percentage of these customers
#' who hold licenses in subsequent years.
#'
#' This year zero focus complicates things if we want to filter customers though.
#' Say we want to look at retention for 30-year-olds. If we applied a filter
#' directly to the history table, we wouldn't be able to calculate retention b/c
#' the future sales of these customers would be dropped. The yrs_zero_filter()
#' function makes it easy to look at specific sets of customers once yrs_zero_split()
#' has been run.
#'
#' @param history license history data frame
#' @param history_split license history list produced by yrs_zero_split()
#' @param func function to be used for subsetting customers
#' @param samp_size customer sample size passed to \code{\link[base]{sample}}
#' @param set_seed if TRUE, will run \code{\link[base]{set.seed}} for reproducibility
#' @family functions to estimate annual license buying
#' @name yrs_zero
#' @examples
#' library(dplyr)
#' data(hunt)
#'
#' # manually calculate retention for those aged 30-50 in 2010
#' # - this is a bit awkward and it's easy to make a mistake
#' #   we also need to keep in mind that only 2011-onward is relevant
#' year0 <- filter(hunt, age_year %in% 30:50, year == 2010)
#' hunt %>%
#'     semi_join(year0, by = "cust_id") %>%
#'     count(year) %>%
#'     mutate(retain_rate = n / max(n))
#'
#' # the "yrs_zero" functions make filtering more straightforward
#' hunt_split <- yrs_zero_split(hunt) %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 30:50, year == 2010))
#' hunt_split$year0
#' hunt_split$history
#'
#' # downstream calculations are consistent, irrespective of customer filter
#' yrs_calc_retain(hunt_split)
#' yrs_zero_sample(hunt_split, 1000) %>% yrs_calc_retain()
#' yrs_zero_split(hunt) %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 25:35)) %>%
#'     yrs_calc_retain()
NULL

#' @describeIn yrs_zero Split history data frame into a list
#' @export
yrs_zero_split <- function(history) {
    list(
        "year0" = history,
        "history" = select(history, .data$cust_id, .data$year)
    )
}

#' @describeIn yrs_zero Filter customers prior to retention calculation
#' @export
yrs_zero_filter <- function(history_split, func) {
    history_split$year0 <- history_split$year0 %>%
        func()
    first_zero_year <- min(unique(history_split$year0$year))

    history_split$history <- history_split$history %>%
        # ensure that irrelevant leading years are dropped (where applicable)
        filter(year >= first_zero_year) %>%
        semi_join(history_split$year0, by = "cust_id")
    history_split
}

#' @describeIn yrs_zero Sample customers prior to retention calculation
#' @export
yrs_zero_sample <- function(history_split, samp_size, set_seed = TRUE) {
    # a more focused version of yrs_cust_filter() that works in this framework
    custid <- unique(history_split$year0$cust_id)
    if (set_seed) set.seed(50)
    custid_samp <- sample(custid, samp_size)
    history_split %>%
        yrs_zero_filter(function(x) filter(x, .data$cust_id %in% custid_samp))
}

#' Prepare data for comparing lifetime vs annual buyers
#'
#' For preparing to run \code{\link{yrs_result_renew}} (logistic regression) with
#' annual vs. lifetime buyers separately.
#'
#' @inheritParams yrs_zero
#' @param sale,lic license data frames
#' @param life_slct name of group to include from lic$life_group (e.g., "sportsman").
#' Only one value should be specified.
#' @family functions to estimate annual license buying
#' @name yrs_lifetime
#' @examples
#' \dontrun{
#' library(DBI)
#' library(tidyverse)
#' f <- "E:/SA/Data-production/NCWRC-19-01/license.sqlite3"
#' con <- dbConnect(RSQLite::SQLite(), f)
#' hunt <- tbl(con, "hunt") %>% collect()
#' lic <- tbl(con, "lic") %>% select(lic_id, life_group, duration) %>% collect()
#' sale <- tbl(con, "sale") %>% select(lic_id, year, cust_id) %>% collect()
#' dbDisconnect(con)
#'
#' hunt <- hunt %>%
#'     yrs_avidity() %>%
#'     yrs_lifetime_join(sale, lic, "sportsman")
#'
#' hunt_split <- hunt %>%
#'     yrs_zero_split() %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 25:35,
#'                     life_group == "sportsman"))
#' life_split <- yrs_lifetime_split(hunt_split)
#'
#' renew_life <- yrs_result_renew(life_split$annual, life_split$lifetime) %>%
#'     mutate(method = "renew_life")
#' renew <- yrs_result_renew(life_split$annual)
#'
#' bind_rows(renew, renew_life) %>% yrs_plot() +
#'    ggtitle("Sportsman annual vs lifetime estimated years")
#' }
NULL

#' @describeIn yrs_lifetime Add lifetime sales info for selected license group
#' @export
yrs_lifetime_join <- function(history, sale, lic, life_slct) {
    lic_slct <- filter(lic, .data$life_group == life_slct) %>%
        select(.data$lic_id, .data$duration, .data$life_group)

    sale_slct <- sale %>%
        right_join(lic_slct, by = "lic_id") %>%
        group_by(.data$cust_id, .data$year) %>%
        arrange(desc(.data$duration)) %>% # give preference to lifetimes
        slice(1L) %>%
        ungroup() %>%
        select(.data$cust_id, .data$year, .data$duration, .data$life_group)

    history %>%
        left_join(sale_slct, by = c("cust_id", "year"))
}

#' @describeIn yrs_lifetime Split into lifetime vs. non-lifetime buyers
#' @export
yrs_lifetime_split <- function(history_split) {
    cust_life <- history_split$year0 %>%
        filter(.data$duration == 99)

    annual <- history_split %>%
        # exclude customers who ever bought a lifetime from annual group
        yrs_zero_filter(function(x) anti_join(x, cust_life, by = "cust_id"))

    lifetime <- history_split %>%
        # keep customers who bought a lifetime (in year0) in the lifetime group
        yrs_zero_filter(function(x) semi_join(x, cust_life, by = c("cust_id", "year")))

    list(
        "annual" = annual, "lifetime" = lifetime$year0
    )
}

# Calculate ---------------------------------------------------------------

#' Calculate license buying
#'
#' Functions to calculate retention for modelling with retain or renew methods.
#' These should only be run after preparation using \code{\link{yrs_zero_split}}
#'
#' @inheritParams yrs_zero
#' @param year0 year for defining customers for whom retention will be calculated
#' @param ... optional grouping variables
#' @family functions to estimate annual license buying
#' @name yrs_calc
#' @examples
#' library(dplyr)
#' data(hunt)
#' hunt_split <- yrs_zero_split(hunt) %>%
#'    yrs_zero_filter(function(x) filter(x, age_year %in% 30:50))
#'
#' yrs_calc_renew_one(hunt_split, 2008)
#' yrs_calc_renew(hunt_split)
#' yrs_calc_retain(hunt_split)
#'
#' library(ggplot2)
#' retain <- yrs_calc_retain(hunt_split, year0)
#' ggplot(retain, aes(years_since, pct, color = year0, size = n0)) +
#'     geom_point() +
#'     ggtitle("We see some variation in retention curves depending on year zero",
#'             "more distant future years (years_since) have fewer observations")
NULL

#' @describeIn yrs_calc Calculate license buying - renewal
#'
#' Appends a new "renew" variable (1=yes, 0=no) based on whether customers from
#' year zero held licenses in subsequent years. The customers
#' from year0 will have one record for all subsequent years.
#' @export
yrs_calc_renew <- function(history_split) {
    # define the full set of "year zeros"
    # - the final year won't be included since there is no renewal to calculate
    calc_yrs <- sort(unique(history_split$year0$year))
    calc_yrs %>%
        sapply(function(yr) yrs_calc_renew_one(history_split, yr),
               simplify = FALSE) %>%
        bind_rows()
}

#' @describeIn yrs_calc Calculate license buying - renewal year0
#'
#' Identify renewal for selected year0. Intended to be run from
#' \code{\link{yrs_calc_renew}}
#' @export
yrs_calc_renew_one <- function(history_split, year0) {
    # identify customers for renewal prediction
    cust_slct <- history_split$year0 %>%
        filter(.data$year == year0)

    # only run if selected data is available
    if (nrow(cust_slct) == 0) return(invisible())
    if (max(history_split$history$year) == year0) return(invisible())

    # get future license sales of selected customers
    future_sales <- history_split$history %>%
        filter(.data$year > year0) %>%
        semi_join(cust_slct, by = "cust_id")

    # identify whether the customer renews in each future year
    renew_one_yr <- function(history_yr, cust_slct) {
        yr <- unique(history_yr$year)
        x <- mutate(history_yr, renew = 1L)
        cust_slct %>%
            select(-.data$year) %>%
            left_join(x, by = "cust_id") %>%
            mutate(
                renew = ifelse(is.na(.data$renew), 0L, .data$renew),
                year = yr
            )
    }
    future_sales %>%
        split(future_sales$year) %>%
        lapply(function(x) renew_one_yr(x, cust_slct)) %>%
        bind_rows() %>%
        mutate(years_since = .data$year - year0, year0 = year0)
}

#' @describeIn yrs_calc Calculate license buying - retention rate
#'
#' This essentially summarizes a customer's probability of renewing in future
#' years.
#' @export
yrs_calc_retain <- function(history_split, ...) {
    group_var <- enquos(...)

    history_split %>%
        yrs_calc_renew() %>%
        group_by(.data$years_since, !!! group_var) %>%
        summarise(pct = mean(.data$renew), n0 = n()) %>%
        ungroup()
}

#' Calculate license buying - years by age
#'
#' Attempting here to replicate the approach used for the OK lifetime license
#' study for characterizing particiation over a sportsperson's lifetime. Takes the
#' average number of years (by cohort) that sportspersons held a license, conditional
#' on having held a license in at least one of the selected years.
#'
#' @inheritParams yrs_calc
#' @param history license history table with cust_id, year, age_year
#' @param yrs set of years that will be averaged. If NULL, uses the most recent
#' 5 years
#' @param ages ages to include, based on first year to be averaged
#' @param age_cohort function to estimate age_cohort based on history$birth_year
#' in the first year to be averaged
#' @family functions to estimate annual license buying
#' @export
#' @examples
#' data(hunt)
#' yrs_calc_avg(hunt)
yrs_calc_avg <- function(
    history, ..., yrs = NULL, ages = 16:64,
    age_cohort = function(x) {
        cut(x, c(-Inf, 15, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, Inf))
    }
) {
    # we only want to include selected years in summary
    if (is.null(yrs)) {
        # defaults to most recent 5 years
        all_yrs <- sort(unique(history$year))
        slct_yrs <- sort(rev(all_yrs)[1:5])
    } else {
        slct_yrs <- yrs
    }

    # identify customers to be included
    # - those who bought at some point in selected years
    cust <- history %>%
        filter(.data$year %in% slct_yrs) %>%
        mutate(birth_year = year - .data$age_year) %>%
        distinct(.data$cust_id, .data$birth_year) %>%
        mutate(
            age = slct_yrs[1] - .data$birth_year,
            cohort = age_cohort(.data$age)
        ) %>%
        filter(.data$age %in% ages)

    # filter license history to include only select customers
    history <- history %>%
        filter(.data$year %in% slct_yrs) %>%
        right_join(cust, by = "cust_id")

    # get average years by cohort
    # - for those who purchased at some point in the selected years
    group_var <- enquos(...)
    history %>%
        group_by(.data$cohort, !!! group_var, .data$cust_id) %>%
        summarise(num_yrs = n()) %>%
        summarise(avg_yrs = mean(.data$num_yrs)) %>%
        ungroup()
}

# Fit ---------------------------------------------------------------------

#' Fit license buying
#'
#' Convenience functions for running a model using selected defaults.
#'
#' @param train_df data frame to use as training dataset
#' @param model function used to model training dataset
#' @family functions to estimate annual license buying
#' @name yrs_fit
#' @examples
#' library(dplyr)
#' data(hunt)
#' hunt <- yrs_avidity(hunt, drop_na_yrs = FALSE)
#' hunt_split <- yrs_zero_split(hunt) %>%
#'     yrs_zero_filter(function(x) filter(x, age_year == 30))
#'
#' train_df <- yrs_calc_renew(hunt_split)
#' yrs_fit_renew(train_df)
#'
#' train_df <- yrs_calc_retain(hunt_split)
#' yrs_fit_retain(train_df)
#' yrs_fit_retain_gam(train_df)
NULL

#' @describeIn yrs_fit Fit license buying - renewal, logit
#' @export
yrs_fit_renew <- function(
    train_df,
    model = function(train_df) {
        glm(renew ~ num_years_held + log(years_since),
            family = binomial(link = "logit"), data = train_df)
    }
) {
    train_df %>%
        yrs_fit_renew_prep() %>%
        model()
}

#' @describeIn yrs_fit Helper function to prepare data for renewal fit
#' @export
yrs_fit_renew_prep <- function(train_df) {
    out <- train_df %>%
        # drop those where avidity wasn't coded (e.g., first 3 years)
        filter(!is.na(.data$num_years_held)) %>%
        # ensures dummy variables are used for avidity
        mutate(num_years_held = factor(.data$num_years_held))
    if (length(unique(out$num_years_held)) == 1) {
        stop("The logit model can't be run with only one num_years_held value.",
             call. = FALSE)
    }
    out
}

#' @describeIn yrs_fit Fit license buying - retention, log
#' @export
yrs_fit_retain <- function(
    train_df,
    model = function(train_df) lm(pct ~ log(years_since), data = train_df)
) {
    model(train_df)
}

#' @describeIn yrs_fit Fit license buying - retention, gam
#' @export
yrs_fit_retain_gam <- function(
    train_df,
    model = function(train_df) gam(pct ~ s(years_since, sp = 0.1), data = train_df)
) {
    model(train_df)
}

# Predict -----------------------------------------------------------------

#' Predict license buying
#'
#' Convenience functions for predicting on a dataset using a specified
#' model fit.
#'
#' @param predict_df data frame to use for prediction
#' @param model_fit model to run \code{\link[stats]{predict}}
#' @param method character name of method used (for plotting)
#' @family functions to estimate annual license buying
#' @name yrs_predict
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' data(hunt)
#'
#' hunt_split <- hunt %>%
#'     yrs_avidity() %>%
#'     yrs_zero_split() %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 30:50))
#'
#' # log trend fit of retention rates
#' train_df <- yrs_calc_retain(hunt_split)
#' model_fit <- yrs_fit_retain(train_df)
#' out <- yrs_predict_retain(data.frame(years_since = 1:40), model_fit)
#' sum(out$pct) # estimated annual license purchases over 40 years
#' ggplot(out, aes(years_since, pct)) + geom_line() + geom_point(data = train_df)
#'
#' # logistic regression based on individual-level renewal
#' train_df2 <- yrs_calc_renew(hunt_split)
#' model_fit2 <- yrs_fit_renew(train_df2)
#' predict_df2 <- filter(hunt_split$year0, year == 2011) %>% select(num_years_held)
#' out2 <- 1:40 %>%
#'     sapply(function(i) mutate(predict_df2, years_since = i), simplify = FALSE) %>%
#'     bind_rows() %>%
#'     yrs_predict_renew(model_fit2)
#' sum(out2$pct)
#' ggplot(out2, aes(years_since, pct)) + geom_line() + geom_point(data = train_df)
NULL

#' @describeIn yrs_predict Predict license buying - retention
#' @export
yrs_predict_retain <- function(predict_df, model_fit, method = "retain") {
    predict_df$pct <- predict(model_fit, predict_df)
    predict_df$method <- method
    predict_df
}

#' @describeIn yrs_predict Predict license buying - renewal
#' @export
yrs_predict_renew <- function(predict_df, model_fit, method = "renew") {
    # predict
    predict_df <- yrs_fit_renew_prep(predict_df)
    predict_df$renew <- model_fit %>%
        predict(predict_df, type = "response")

    # summarize by years_since
    predict_df %>%
        group_by(.data$years_since) %>%
        summarise(pct = mean(.data$renew), n = n()) %>%
        mutate(method = method)
}

#' Predict license buying - years by age
#'
#' To be run following \code{\link{yrs_calc_avg}}. Converts the output of that
#' function to a scale comparable to a retention curve
#'
#' @param avg_yrs data frame returned by \code{\link{yrs_calc_avg}}
#' @param num_yrs number of years used in avg_yrs
#' @param start_age starting age for years_since = 0
#' @param end_age last age to include
#' @inheritParams yrs_calc_avg
#' @inheritParams yrs_predict
#' @family functions to estimate annual license buying
#' @export
#' @examples
#' data(hunt)
#' x <- yrs_calc_avg(hunt)
#' yrs_predict_avg(x)
yrs_predict_avg <- function(
    avg_yrs, ..., num_yrs = 5,
    age_cohort = function(x) {
        cut(x, c(-Inf, 15, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, Inf))
    },
    start_age = 31, end_age = 64, method = "years_by_age"
) {
    # identify cohort age ranges
    age_per_cohort <- tibble(age_year = 0:100) %>%
        mutate(cohort = age_cohort(.data$age_year))

    # convert avg_yrs to percentage
    avg_yrs <- mutate(avg_yrs, pct = avg_yrs / num_yrs)

    # cast age cohorts to individual ages
    group_var <- enquos(...)
    inner_join(age_per_cohort, avg_yrs) %>%
        filter(.data$age_year > start_age, .data$age_year <= end_age) %>%
        group_by(!!! group_var) %>%
        mutate(years_since = row_number()) %>%
        select(.data$age_year, .data$years_since, .data$pct) %>%
        ungroup() %>%
        mutate(method = method)
}

# Result ------------------------------------------------------------------

#' Results of license buying
#'
#' These are convenience functions to wrap calculating, fitting, and predicting.
#'
#' @param history training dataset
#' @param history_predict prediction dataset. If NULL, uses the training dataset
#' @param predict_age target age for prediction (year zero)
#' @param end_age last age for prediction
#' @param model function for modelling
#' @inheritParams yrs_zero
#' @family functions to estimate annual license buying
#' @name yrs_result
#' @examples
#' library(dplyr)
#' data(hunt)
#' avg <- yrs_result_avg(hunt)
#'
#' hunt_split <- hunt %>%
#'     yrs_avidity(drop_na_yrs = FALSE) %>%
#'     yrs_zero_split() %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 25:35))
#'
#' observe <- yrs_result_observe(hunt_split)
#' retain <- yrs_result_retain(hunt_split)
#' renew <- yrs_result_renew(hunt_split)
#' max <- yrs_result_max(hunt_split)
#'
#' library(ggplot2)
#' ggplot(observe, aes(age_year, pct, color = method)) + geom_point() +
#'     geom_line(data = retain) +
#'     geom_line(data = renew) +
#'     geom_line(data = avg) +
#'     geom_line(data = max)
NULL

#' @describeIn yrs_result Results for predicting license buying - observed retention
#' @export
yrs_result_observe <- function(history_split, predict_age = 30) {
    history_split %>%
        yrs_calc_retain() %>%
        mutate(
            age_year = predict_age + .data$years_since,
            method = "observed retention"
        )
}

#' @describeIn yrs_result Results for predicting license buying - predicted retention
#' @export
yrs_result_retain <- function(
    history_split, predict_age = 30, end_age = 64,
    model = function(x) yrs_fit_retain(x)
) {
    train_df <- yrs_result_observe(history_split, predict_age)

    if (end_age %in% train_df$age_year) {
        # use observed data if no extrapolation needs to be performed
        # - this also avoids artifacts from cheap senior lifetime licenses
        predict_df <- filter(train_df, .data$age_year <= end_age) %>%
            mutate(method = "retain")
    } else {
        model_fit <- model(train_df)
        num_yrs_fwd <- end_age - predict_age
        predict_df <- tibble(years_since = 1:num_yrs_fwd) %>%
            yrs_predict_retain(model_fit)
    }
    predict_df %>%
        mutate(age_year = predict_age + .data$years_since, n = max(train_df$n0)) %>%
        select(.data$years_since, .data$pct, .data$method, .data$age_year, .data$n)
}

#' @describeIn yrs_result Results for predicting license buying - predicted renewal
#' @export
yrs_result_renew <- function(
    history_split, history_predict = NULL, predict_age = 30, end_age = 64
) {
    # train model
    num_yrs_fwd <- end_age - predict_age
    train_df <- yrs_calc_renew(history_split) %>%
        # we don't want to include ages past end_age in training data
        filter(.data$years_since <= num_yrs_fwd)
    model_fit <- yrs_fit_renew(train_df)

    # cast prediction data out to all years needed
    if (is.null(history_predict)) {
        history_predict <- history_split$year0
    }
    predict_df <- 1:num_yrs_fwd %>%
        sapply(function(i) mutate(history_predict, years_since = i),
               simplify = FALSE) %>%
        bind_rows()

    # run prediciton
    predict_df %>%
        yrs_predict_renew(model_fit) %>%
        mutate(age_year = predict_age + .data$years_since)
}

#' @describeIn yrs_result Results for predicting license buying - maximum retention
#'
#' Based on observed retention, assume the retention rate remains level after the
#' last observed year. This defines a maximum estimate because retention curves
#' never begin sloping upward.
#' @export
yrs_result_max <- function(
    history_split, predict_age = 30, end_age = 64
) {
    train_df <- yrs_result_observe(history_split)
    train_last <- tail(train_df, 1)
    num_yrs_fwd <- end_age - predict_age - train_last[["years_since"]]

    cast_fwd <- function(i) {
        train_last %>%  mutate(
            years_since = .data$years_since + i,
            age_year = .data$age_year + i
        )
    }
    sapply(1:num_yrs_fwd, cast_fwd, simplify = FALSE) %>%
        bind_rows() %>%
        bind_rows(train_df) %>%
        mutate(method = "max_possible")
}

#' @describeIn yrs_result Results for predicting license buying - predicted avg years
#'
#' Different than the renew & retain functions since it uses all available ages
#' to estimate average years purchased in five, so no train_ages
#' arguments are needed.
#' @export
yrs_result_avg <- function(history, predict_age = 30, end_age = 64) {
    train_df <- yrs_calc_avg(history, ages = 0:100)
    yrs_predict_avg(train_df, start_age = predict_age, end_age = end_age)
}

# Plotting ----------------------------------------------------------------

#' Plot results for predicting license buying
#'
#' It's useful to visualize the results with retention estimates over time,
#' especially for comparing alternative methods. This function takes the result
#' of \code{\link{yrs_result}} and plots with some useful labelling and scaling.
#'
#' @param result_df results returned from \code{\link{yrs_result}}
#' @param include_label if TRUE, displays the method name and years estimate
#' next to the line
#' @param xlim maximum x-axis limit for plot
#' @param grp unquoted name of grouping variable
#' @family functions to estimate annual license buying
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' data(hunt)
#' avg <- yrs_result_avg(hunt)
#'
#' hunt_split <- hunt %>%
#'     yrs_avidity(drop_na_yrs = FALSE) %>%
#'     yrs_zero_split() %>%
#'     yrs_zero_filter(function(x) filter(x, age_year %in% 25:35))
#'
#' # comparing retention-based vs avg-years-based methods
#' observe <- yrs_result_observe(hunt_split)
#' retain <- yrs_result_retain(hunt_split)
#' max <- yrs_result_max(hunt_split)
#' bind_rows(retain, max, avg) %>% yrs_plot() + geom_point(data = observe)
#'
#' # comparing retain & renew methods
#' renew <- yrs_result_renew(hunt_split)
#' bind_rows(retain, renew) %>% yrs_plot() + geom_point(data = observe)
#'
#' # using a GAM for retain
#' retain <- hunt_split %>%
#'     yrs_result_retain(model = function(x) yrs_fit_retain_gam(x))
#' bind_rows(retain, max, avg) %>% yrs_plot() + geom_point(data = observe)
yrs_plot <- function(
    result_df, include_label = TRUE, xlim = 74, grp = method
) {
    grp <- enquo(grp)

    p <- result_df %>%
        ggplot(aes(.data$age_year, .data$pct, color = !! grp)) +
        geom_line() +
        scale_y_continuous(
            limits = c(0, max(result_df$pct) + 0.02),
            labels = scales::percent
        ) +
        theme(axis.title.y = element_blank())

    if (include_label) {
        est <- result_df %>%
            group_by(!! grp) %>%
            summarise(est_yrs = sum(.data$pct), age_year = max(.data$age_year),
                      pct = last(.data$pct)) %>%
            mutate(text_label = paste0(
                "yrs (", !! grp, ") = ", round(.data$est_yrs, 1))
            )
        p <- p +
            geom_text(aes(label = .data$text_label, hjust = -0.05), data = est) +
            scale_x_continuous(limits = c(min(result_df$age_year), xlim)) +
            theme(legend.position = "none")
    }
    p
}
