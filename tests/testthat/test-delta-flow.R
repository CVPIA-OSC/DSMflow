library(testthat)
library(tidyverse)
library(lubridate)
# DSMflow::delta_flows ---------------------------------------------------------
test_that("test delta flow structure and values", {
  col_names <- c("date", "n_dlt_inflow_cfs", "s_dlt_inflow_cfs", "n_dlt_div_cfs",
                 "s_dlt_div_cfs", "n_dlt_prop_div", "s_dlt_prop_div")
  expect_equal(delta_flows$n_dlt_prop_div, delta_flows$n_dlt_div_cfs/delta_flows$n_dlt_inflow_cfs)
  expect_equal(delta_flows$s_dlt_prop_div, pmin(delta_flows$s_dlt_div_cfs/delta_flows$s_dlt_inflow_cfs, 1))
  expect_equal(class(delta_flows$date), "Date")
  expect_equal(class(delta_flows$n_dlt_inflow_cfs), "integer")
  expect_equal(class(delta_flows$s_dlt_inflow_cfs), "numeric")
  expect_equal(class(delta_flows$n_dlt_div_cfs), "numeric")
  expect_equal(class(delta_flows$s_dlt_div_cfs), "numeric")
  expect_equal(class(delta_flows$n_dlt_prop_div), "numeric")
  expect_equal(class(delta_flows$n_dlt_prop_div), "numeric")
  expect_equal(colnames(delta_flows), col_names)
  # think about testing to check that units seem right (not too small because that would be an indicator of cms)
  # Think about tests to catch outliers
})

# DSMflow::delta_inflow --------------------------------------------------------
# transform delta_inflow matrix to tidy format
delta_inflow_df <- map_df(c("North Delta", "South Delta"), function(i) {
  DSMflow::delta_inflow[, , i] %>%
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
delta_flows_filtered <- filter(DSMflow::delta_flows, date >= as.Date("1980-01-01"),
                               date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_inflow_df)


test_that("test delta inflow structure and values", {
  expect_equal(dim(delta_inflow), c(12L, 21L, 2L))
  expect_equal(cfs_to_cms(delta_flows_filtered$n_dlt_inflow_cfs), delta_flows_filtered$n_dlt_inflow_cms)
  expect_equal(cfs_to_cms(delta_flows_filtered$s_dlt_inflow_cfs), delta_flows_filtered$s_dlt_inflow_cms)
})

# DSMflow::delta_proportion_diverted -------------------------------------------
delta_proportion_diverted_df <- map_df(c("North Delta", "South Delta"), function(i) {
  DSMflow::delta_proportion_diverted[, , i] %>%
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
delta_flows_filtered <- filter(DSMflow::delta_flows, date >= as.Date("1980-01-01"),
                               date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  full_join(delta_proportion_diverted_df)

test_that("Proportion calculated from delta flow and between 0 and 1; delta_flows equals delta_proportion_diverted", {
  expect_equal(dim(delta_proportion_diverted), c(12L, 21L, 2L))
  expect_true(all(delta_proportion_diverted >= 0))
  expect_true(all(delta_proportion_diverted <= 1))
  expect_equal(delta_flows_filtered$n_dlt_prop_div, delta_flows_filtered$n_dlt_proportion_diverted)
  expect_equal(delta_flows_filtered$s_dlt_prop_div, delta_flows_filtered$s_dlt_proportion_diverted)
})

# DSMflow::delta_total_diverted ------------------------------------------------
test_that("Total diverted is greater than proportion diverted", {
  expect_equal(dim(delta_total_diverted), c(12L, 21L, 2L))
  expect_true(all(delta_total_diverted > delta_proportion_diverted))
})

# DSMflow::delta_cross_channel_closed ------------------------------------------
test_that("Proportion of days that overtopped", {
  expect_true(all(delta_cross_channel_closed[2, ] >= 0))
  expect_true(all(delta_cross_channel_closed[2, ] <= 1))
  expect_true(all(delta_cross_channel_closed[1, ] <= 31))
  expect_true(all(delta_cross_channel_closed[1, ] >= 0))
  expect_equal(dim(delta_cross_channel_closed), c(2L, 12L))
})
