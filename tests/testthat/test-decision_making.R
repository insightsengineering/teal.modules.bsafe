local({
param_approx <- matrix(c(1, 2, 0.1), nrow = 3)

test_that("calc_log_hazard_area returns correct values", {
  expected <- c(1.8, 2.2)
  result <- calc_log_hazard_area(param_approx)
  expect_equal(result, expected)
})

test_that("calc_param_approx_boundaries returns correct lower and upper boundaries", {
  expected <- c(1.9, 2.1)
  result <- calc_param_approx_boundaries(param_approx)
  expect_equal(result, expected)
})
})