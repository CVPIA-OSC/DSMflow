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
  expect_equal(unlist(lapply(unname(flows_cfs), function(i) dim(i))), c(972, 31, 972, 31))
  expect_equal(names(flows_cfs), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(unlist(lapply(unname(freeport_flow), function(i) dim(i))), c(12, 21, 12, 21))
  expect_equal(names(freeport_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(unlist(lapply(unname(vernalis_flow), function(i) dim(i))), c(12, 21, 12, 21))
  expect_equal(names(vernalis_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(unlist(lapply(unname(wilkins_flow), function(i) dim(i))), c(12, 21, 12, 21))
  expect_equal(names(wilkins_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(unlist(lapply(unname(stockton_flow), function(i) dim(i))), c(12, 21, 12, 21))
  expect_equal(names(stockton_flow), c("biop_2008_2009","biop_itp_2018_2019"))

  expect_equal(unlist(lapply(unname(upper_sacramento_flows), function(i) dim(i))), c(12, 21, 12, 21))
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
mean_flow_df_2008 <- map_df(years, function(i) {
    mean_flow$biop_2008_2009[, , i] %>%
    as.data.frame() %>%
    mutate(year = i) %>%
    rownames_to_column(var = "watershed") %>%
    pivot_longer(cols = -c(watershed, year), names_to = "month", values_to = "flow_cms")
  })

total_diverted_df_2008 <- map_df(years, function(i) {
    total_diverted$biop_2008_2009[, , i] %>%
    as.data.frame() %>%
    mutate(year = i) %>%
    rownames_to_column(var = "watershed") %>%
    pivot_longer(cols = -c(watershed, year), names_to = "month", values_to = "diverted_cms")
})


flows_filtered_df_2008 <- filter(flows_cfs$biop_2008_2009, date >= as.Date("1980-01-01"),
                            date <= as.Date("2000-12-31")) %>%
  mutate(year = as.character(year(date)),
         month = as.character(month(date, label = T, abbr = T))) %>%
  pivot_longer(cols = -c(date, year, month), names_to = "watershed", values_to = "flow_cfs") %>%
  filter(!(watershed %in% c("Lower-mid Sacramento River1", "Lower-mid Sacramento River2"))) |>
  full_join(mean_flow_df_2008 |>
              filter(!(watershed %in% c("Sutter Bypass", "Yolo Bypass", "Lower-mid Sacramento River")))) |>
  full_join(total_diverted_df_2008 |>
              filter(!(watershed %in% c("Sutter Bypass", "Yolo Bypass", "Lower-mid Sacramento River")))) |>
  glimpse()

test_that("flow_cfs equals mean_flow", {
  expect_equal(cfs_to_cms(flows_filtered_df_2008$flow_cfs), flows_filtered_df_2008$flow_cms)
})

# TODO Lower-mid Sacramento River1/2 doesn't exist in mean_flow but Lower-mid Sacramento River does.
# TODO Sutter Bypass, Lower-mid Sacramento River, Yolo Bypass do not exist in flows_cfs
# TODO just filtered these out in the join - need more elegant solution

# total_diverted less than mean_flow -------------------------------------------
test_that("total_diverted less than mean_flow", {
  expect_true(all(flows_filtered_df_2008$diverted_cms <= flows_filtered_df_2008$flow_cms))
})

# TODO some cases where diverted is greater than mean flow

# proportions less than 1 ------------------------------------------------------
test_that("proportions less than 1", {

  expect_equal(unlist(lapply(unname(proportion_diverted), function(i) all(i >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(proportion_diverted), function(i) all(i <= 1))), c(TRUE, TRUE))

  expect_equal(unlist(lapply(unname(proportion_flow_natal), function(i) all(i >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(proportion_flow_natal), function(i) all(i <= 1))), c(TRUE, TRUE))

  expect_equal(unlist(lapply(unname(proportion_pulse_flows), function(i) all(i >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(proportion_pulse_flows), function(i) all(i <= 1))), c(TRUE, TRUE))

})
# TODO: some proportions are greater than 1
# which(proportion_pulse_flows$biop_2008_2009 > 1, arr.ind=T)
