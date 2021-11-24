library(testthat)
library(tidyverse)
library(lubridate)
# Tests in this script test:
# bypass_flows - structure

# proportion_flow_bypasses - structure, proportion less than 1

# gates_overtopped - structure

# structure --------------------------------------------------------------------

test_that("structure", {
  expect_equal(dim(bypass_flows), c(972, 7))
  expect_equal(dim(proportion_flow_bypasses), c(12, 21, 2))
  expect_equal(dim(gates_overtopped), c(12, 21, 2))
  expect_equal(class(all(gates_overtopped)), "logical")
})

# proportion less than 1 -------------------------------------------------------

test_that("proportion less than 1", {
  expect_true(all(proportion_flow_bypasses >= 0))
  expect_true(all(proportion_flow_bypasses <= 1))
})
