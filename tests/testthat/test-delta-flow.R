library(testthat)
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
test_that("", {
  expect_equal(dim(delta_inflow), c(12L, 21L, 2L))

  # TODO come back to this and figure out if useful and if so convert matrix/df to be compatible
  # currently only testing full value, want to test full one
  # filtered_delta <- filter(delta_flows, date >= as.Date("1980-01-01") & date <= as.Date("2000-12-30"))
  # expect_equal(delta_inflow[1,1,1], cfs_to_cms(filtered_delta$n_dlt_inflow_cfs)[1])
})

# DSMflow::delta_proportion_diverted -------------------------------------------
test_that("Proportion calculated from delta flow and between 0 and 1", {
  expect_equal(dim(delta_proportion_diverted), c(12L, 21L, 2L))
  expect_true(all(delta_proportion_diverted >= 0))
  expect_true(all(delta_proportion_diverted <= 1))
  # think about testing that it equals prop_diverted from delta_flows
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

