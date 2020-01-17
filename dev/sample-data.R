# create sample data

library(tidyverse)
library(lifetime)
library(DBI)

db_license <- "E:/SA/Data-production/NCWRC-19-01/license.sqlite3"
con <- dbConnect(RSQLite::SQLite(), db_license)
lic <- tbl(con, "lic") %>% collect()
cust <- tbl(con, "cust") %>% collect()
sale <- tbl(con, "sale") %>% select(cust_id:year, res) %>% collect()
all_sports <- tbl(con, "all_sports") %>% collect()
dbDisconnect(con)

# pull customer sample & filter
# - only include the license types of interest
lic <- filter(lic, life_group == "sportsman") %>% select(lic_id, duration, life_group)
sale <- semi_join(sale, lic)
cust <- semi_join(cust, sale)

cust <- sample_n(cust, 50000) %>% select(cust_id, birth_year)
sale <- semi_join(sale, cust) %>% select(cust_id, lic_id, year)
all_sports <- semi_join(all_sports, cust) %>% select(cust_id, year, res, age_year)

# calculate sportsman retention
df_split <- all_sports %>%
    yrs_lifetime_join(sale, lic, "sportsman") %>%
    yrs_zero_split() %>%
    yrs_zero_filter(function(x) filter(x, life_group == "sportsman", res == 1))
retain_all <- nc_retain_all(df_split, age = 16:63)
retain <- nc_retain(retain_all)

filter(retain_all, current_age %in% c(20,30,40), years_since > 0) %>%
    ggplot(aes(years_since, pct, color = factor(current_age))) +
    geom_line()

ggplot(retain, aes(current_age, yrs)) +
    geom_point() +
    ggtitle("Estimated future participation years by age")

# write to package data
devtools::use_data(cust, overwrite = TRUE)
devtools::use_data(lic, overwrite = TRUE)
devtools::use_data(sale, overwrite = TRUE)
devtools::use_data(all_sports, overwrite = TRUE)
devtools::use_data(retain_all, overwrite = TRUE)
devtools::use_data(retain, overwrite = TRUE)
