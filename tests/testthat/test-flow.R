test_that("proportion diverted are between 0 and 1",  {
  expect_true(all(proportion_diverted >= 0))
  expect_true(all(proportion_diverted <= 1))
})
