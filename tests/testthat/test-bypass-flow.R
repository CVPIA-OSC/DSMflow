library(testthat)
library(tidyverse)
library(lubridate)
# Tests in this script test:
# bypass_flows - structure

# proportion_flow_bypasses - structure, proportion less than 1

# gates_overtopped - structure

# NOTE: using lapply on unnamed list to get dimensions of biop 2008 and biop 2018 list elements

# structure --------------------------------------------------------------------

test_that("structure", {
  expect_equal(unlist(lapply(unname(bypass_flows), function(i) dim(i))), c(972, 7, 972, 7))

  expect_equal(unlist(lapply(unname(proportion_flow_bypasses), function(i) dim(i))), c(12, 21, 2, 12, 21, 2))

  expect_equal(unlist(lapply(unname(gates_overtopped), function(i) dim(i))), c(12, 21, 2, 12, 21, 2))

  expect_equal(unlist(lapply(unname(gates_overtopped), function(i) class(all(i)))), c("logical", "logical"))
})

# proportion less than 1 -------------------------------------------------------

test_that("proportion less than 1", {
  expect_equal(unlist(lapply(unname(proportion_flow_bypasses), function(i) all(i >= 0))), c(TRUE, TRUE))
  expect_equal(unlist(lapply(unname(proportion_flow_bypasses), function(i) all(i <= 1))), c(TRUE, TRUE))
})
