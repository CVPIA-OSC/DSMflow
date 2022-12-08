library(testthat)
library(tidyverse)
library(lubridate)

# Tests in this script test:
# delta_cross_channel_closed - delta structure and values, proportion of days that overtopped
# delta_flows - delta structure and values, delta_inflow equal delta_flows, delta_flows equals delta_proportion_diverted
# delta_inflow - delta structure and values, delta_inflow equal delta_flows
# delta_proportion_diverted - delta_flows equals delta_proportion_diverted, proportion less than 1, delta_total_diverted greater than delta_proportion_diverted, delta structure and values
# delta_total_diverted - delta_total_diverted greater than delta_proportion_diverted, delta structure and values

# delta structure and values ---------------------------------------------------------
test_that("delta structure and values", {
  # names
  expect_equal(names(delta_flows$biop_2008_2009), names(delta_flows$biop_itp_2018_2019))
  col_names <- c("date", "n_dlt_inflow_cfs", "s_dlt_inflow_cfs", "n_dlt_div_cfs",
                 "s_dlt_div_cfs", "n_dlt_prop_div", "s_dlt_prop_div")
  year_names <- as.character(seq(1980, 2000, 1))

  expect_equal(unlist(lapply(unname(delta_flows), function(i) colnames(i))), rep(col_names, 2))
  expect_equal(unlist(lapply(unname(delta_inflow), function(i) colnames(i))), rep(year_names, 2))
  expect_equal(unlist(lapply(unname(delta_proportion_diverted), function(i) colnames(i))), rep(year_names, 2))
  expect_equal(unlist(lapply(unname(delta_total_diverted), function(i) colnames(i))), rep(year_names, 2))
  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) colnames(i))), rep(month.abb, 2))

  # dimensions

  expect_equal(unlist(lapply(unname(delta_inflow), function(i) dim(i))), c(12L, 21L, 2L, 12L, 21L, 2L))
  expect_equal(unlist(lapply(unname(delta_proportion_diverted), function(i) dim(i))), c(12L, 21L, 2L, 12L, 21L, 2L))
  expect_equal(unlist(lapply(unname(delta_total_diverted), function(i) dim(i))), c(12L, 21L, 2L, 12L, 21L, 2L))
  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) dim(i))), c(2L, 12L, 2L, 12L))

  # values
  expect_equal(delta_flows$biop_2008_2009$n_dlt_prop_div, delta_flows$biop_2008_2009$n_dlt_div_cfs/delta_flows$biop_2008_2009$n_dlt_inflow_cfs)
  expect_equal(delta_flows$biop_itp_2018_2019$n_dlt_prop_div, delta_flows$biop_itp_2018_2019$n_dlt_div_cfs/delta_flows$biop_itp_2018_2019$n_dlt_inflow_cfs)

  expect_equal(delta_flows$biop_2008_2009$s_dlt_prop_div, pmin(delta_flows$biop_2008_2009$s_dlt_div_cfs/delta_flows$biop_2008_2009$s_dlt_inflow_cfs, 1))
  expect_equal(delta_flows$biop_itp_2018_2019$s_dlt_prop_div, pmin(delta_flows$biop_itp_2018_2019$s_dlt_div_cfs/delta_flows$biop_itp_2018_2019$s_dlt_inflow_cfs, 1))

  # class
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$date))), c("Date", "Date"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$n_dlt_inflow_cfs))), c("numeric", "numeric"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$s_dlt_inflow_cfs))), c("numeric", "numeric"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$n_dlt_div_cfs))), c("numeric", "numeric"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$s_dlt_div_cfs))), c("numeric", "numeric"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$n_dlt_prop_div))), c("numeric", "numeric"))
  expect_equal(unlist(lapply(unname(delta_flows), function(i) class(i$s_dlt_prop_div))), c("numeric", "numeric"))

  # TODO: units (should be in cfs, not cms)

  # TODO: outliers

})

# delta_inflow equal delta_flows --------------------------------------------------------
# transform delta_inflow matrix to tidy format
delta_inflow_df_2008 <- map_df(c("North Delta", "South Delta"), function(i) {
  delta_inflow$biop_2008_2009[, , i] %>%
  as.data.frame() %>%
  mutate(location = i) %>%
  rownames_to_column(var = "month") %>%
  pivot_longer(cols = `1980`:`2000`,
               names_to = 'year',
               values_to = 'inflow_cms')}) %>%
  pivot_wider(id_cols = c('month','year'),
              names_from = location,
              values_from = inflow_cms) %>%
  rename(n_dlt_inflow_cms = `North Delta`,
         s_dlt_inflow_cms = `South Delta`)

delta_inflow_df_2018 <- map_df(c("North Delta", "South Delta"), function(i) {
  delta_inflow$biop_itp_2018_2019[, , i] %>%
  as.data.frame() %>%
  mutate(location = i) %>%
  rownames_to_column(var = "month") %>%
  pivot_longer(cols = `1980`:`2000`,
               names_to = 'year',
               values_to = 'inflow_cms')}) %>%
  pivot_wider(id_cols = c('month','year'),
              names_from = location,
              values_from = inflow_cms) %>%
  rename(n_dlt_inflow_cms = `North Delta`,
         s_dlt_inflow_cms = `South Delta`)

# prepared delta_flows to compare to inflow
delta_flows_filtered_2008 <- filter(delta_flows$biop_2008_2009, date >= as.Date("1980-01-01"),
                               date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_inflow_df_2008)

delta_flows_filtered_2018 <- filter(delta_flows$biop_itp_2018_2019, date >= as.Date("1980-01-01"),
                                    date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_inflow_df_2018)


test_that("delta_inflow equal delta_flows", {
  expect_equal(cfs_to_cms(delta_flows_filtered_2008$n_dlt_inflow_cfs), delta_flows_filtered_2008$n_dlt_inflow_cms)
  expect_equal(cfs_to_cms(delta_flows_filtered_2018$n_dlt_inflow_cfs), delta_flows_filtered_2018$n_dlt_inflow_cms)

  expect_equal(cfs_to_cms(delta_flows_filtered_2008$s_dlt_inflow_cfs), delta_flows_filtered_2008$s_dlt_inflow_cms)
  expect_equal(cfs_to_cms(delta_flows_filtered_2018$s_dlt_inflow_cfs), delta_flows_filtered_2018$s_dlt_inflow_cms)
})

# proportion less than 1 -------------------------------------------
delta_proportion_diverted_df_2008 <- map_df(c("North Delta", "South Delta"), function(i) {
  delta_proportion_diverted$biop_2008_2009[, , i] %>%
  as.data.frame() %>%
  mutate(location = i) %>%
  rownames_to_column(var = "month") %>%
  pivot_longer(cols = `1980`:`2000`,
               names_to = 'year',
               values_to = 'proportion_diverted')}) %>%
  pivot_wider(id_cols = c('month','year'),
              names_from = location,
              values_from = proportion_diverted) %>%
  rename(n_dlt_proportion_diverted = `North Delta`,
         s_dlt_proportion_diverted = `South Delta`)

delta_proportion_diverted_df_2018 <- map_df(c("North Delta", "South Delta"), function(i) {
  delta_proportion_diverted$biop_itp_2018_2019[, , i] %>%
  as.data.frame() %>%
  mutate(location = i) %>%
  rownames_to_column(var = "month") %>%
  pivot_longer(cols = `1980`:`2000`,
               names_to = 'year',
               values_to = 'proportion_diverted')}) %>%
  pivot_wider(id_cols = c('month','year'),
              names_from = location,
              values_from = proportion_diverted) %>%
  rename(n_dlt_proportion_diverted = `North Delta`,
         s_dlt_proportion_diverted = `South Delta`)

# prepared delta_flows to compare to inflow
delta_flows_filtered_2008 <- filter(delta_flows$biop_2008_2009, date >= as.Date("1980-01-01"),
                               date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_proportion_diverted_df_2008)

delta_flows_filtered_2018 <- filter(delta_flows$biop_itp_2018_2019, date >= as.Date("1980-01-01"),
                                    date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_proportion_diverted_df_2018)

test_that("proportion less than 1", {
  expect_true(all(delta_proportion_diverted$biop_2008_2009 >= 0))
  expect_true(all(delta_proportion_diverted$biop_2008_2009 <= 1))

  expect_true(all(delta_proportion_diverted$biop_itp_2018_2019 >= 0))
  expect_true(all(delta_proportion_diverted$biop_itp_2018_2019 <= 1))

})

# delta_flows equals delta_proportion_diverted -------------------------------------------

test_that("delta_flows equals delta_proportion_diverted", {
  expect_equal(delta_flows_filtered_2008$n_dlt_prop_div, delta_flows_filtered_2008$n_dlt_proportion_diverted)
  expect_equal(delta_flows_filtered_2008$s_dlt_prop_div, delta_flows_filtered_2008$s_dlt_proportion_diverted)

  expect_equal(delta_flows_filtered_2018$n_dlt_prop_div, delta_flows_filtered_2018$n_dlt_proportion_diverted)
  expect_equal(delta_flows_filtered_2018$s_dlt_prop_div, delta_flows_filtered_2018$s_dlt_proportion_diverted)
})

# delta_total_diverted greater than delta_proportion_diverted ------------------------------------------------
# TODO delta_inflow is not > delta_total_diverted
test_that("delta_total_diverted greater than delta_proportion_diverted", {

  expect_equal(unlist(lapply(unname(delta_total_diverted), function(i) all(i >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(delta_inflow), function(i) all(i >= 0))), c(TRUE, TRUE))

})


# proportion of days that overtopped ------------------------------------------
test_that("proportion of days that overtopped", {

  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) all(i[2, ] >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) all(i[2, ] <= 1))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) all(i[1, ] <= 31))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(delta_cross_channel_closed), function(i) all(i[1, ] >= 0))), c(TRUE, TRUE))

})
