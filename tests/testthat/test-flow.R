library(testthat)
library(tidyverse)
library(lubridate)

# Tests in this script test:
# flows_cfs - flow structure, flow_cfs equal mean_flow
# mean_flow - flow_cfs equal mean_flow, total_diverted less than mean_flow
# freeport_flow - flow structure
# proportion_flow_natal - proportions less than 1
# proportion_pulse_flows - proportions less than 1
# stockon_flow - flow structure
# upper_sacramento_flows - flow structure
# vernalis_flow - flow structure
# wilkins_flow - flow structure
# proportion_diverted - proportions less than 1
# total_diverted - total_diverted less than mean_flow

# flow structure ------------------------------------------------------------

test_that("flow structure",  {
  expect_equal(dim(flows_cfs$biop_2008_2009), c(972, 31))
  expect_equal(dim(flows_cfs$biop_itp_2018_2019), c(972, 31))
  expect_equal(names(flows_cfs), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(dim(freeport_flow$biop_2008_2009), c(12, 21))
  expect_equal(dim(freeport_flow$biop_itp_2018_2019), c(12, 21))
  expect_equal(names(freeport_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(dim(vernalis_flow$biop_2008_2009), c(12, 21))
  expect_equal(dim(vernalis_flow$biop_itp_2018_2019), c(12, 21))
  expect_equal(names(vernalis_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(dim(wilkins_flow$biop_2008_2009), c(12, 21))
  expect_equal(dim(wilkins_flow$biop_itp_2018_2019), c(12, 21))
  expect_equal(names(wilkins_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(dim(stockton_flow$biop_2008_2009), c(12, 21))
  expect_equal(dim(stockton_flow$biop_itp_2018_2019), c(12, 21))
  expect_equal(names(stockton_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(dim(upper_sacramento_flows$biop_2008_2009), c(12, 21))
  expect_equal(dim(upper_sacramento_flows$biop_itp_2018_2019), c(12, 21))
  expect_equal(names(upper_sacramento_flows), c("biop_2008_2009","biop_itp_2018_2019"))

})

# TODO
# Dimensions of flow_cfs vary from what is described in documentation

# flow_cfs equals mean_flow ----------------------------------------------------
# List of tests to include:
# Could include a test that checks the equation but that would be more involved
# Test that mean_flow equals mean flow calculated using cfs_to_cms(flow_cfs)

years <- seq(1980,2000)
years <- as.character(years)
mean_flow_df <- map_df(years, function(i) {
  DSMflow::mean_flow[, , i] %>%
    as.data.frame() %>%
    mutate(year = i) %>%
    rownames_to_column(var = "watershed") %>%
    pivot_longer(cols = -c(watershed, year), names_to = "month", values_to = "flow_cms")
  })

total_diverted_df <- map_df(years, function(i) {
  DSMflow::total_diverted[, , i] %>%
    as.data.frame() %>%
    mutate(year = i) %>%
    rownames_to_column(var = "watershed") %>%
    pivot_longer(cols = -c(watershed, year), names_to = "month", values_to = "diverted_cms")
})


flows_filtered_df <- filter(flows_cfs, date >= as.Date("1980-01-01"),
                            date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  pivot_longer(cols = -c(date, year, month), names_to = "watershed", values_to = "flow_cfs") %>%
  full_join(mean_flow_df) %>%
  full_join(total_diverted_df)

test_that("flow_cfs equals mean_flow", {
  expect_equal(cfs_to_cms(flows_filtered_df$flow_cfs), flows_filtered_df$flow_cms)
})

# TODO Lower-mid Sacramento River1/2 doesn't exist in mean_flow but Lower-mid Sacramento River does.
# TODO Sutter Bypass, Lower-mid Sacramento River, Yolo Bypass do not exist in flows_cfs

# total_diverted less than mean_flow -------------------------------------------
test_that("total_diverted less than mean_flow", {
  expect_true(all(flows_filtered_df$diverted_cms <= flows_filtered_df$flow_cms))
})

# TODO some cases where diverted is greater than mean flow

# proportions less than 1 ------------------------------------------------------
test_that("proportions less than 1", {
  expect_true(all(proportion_diverted >= 0))
  expect_true(all(proportion_diverted <= 1))
  expect_true(all(proportion_flow_natal >= 0))
  expect_true(all(proportion_flow_natal <= 1))
  expect_true(all(proportion_pulse_flows >= 0))
  expect_true(all(proportion_pulse_flows <= 1))
})
