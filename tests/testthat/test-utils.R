test_that("'%||%' returns 'a' when 'a' is not NULL, not NA, and has length > 0", {
  result <- 1 %||% 2
  expect_equal(result, 1)
})

test_that("'%||%' returns 'b' when 'a' is NULL", {
  result <- NULL %||% 2
  expect_equal(result, 2)
})

test_that("'%||%' returns 'b' when 'a' is NA", {
  result <- NA %||% 2
  expect_equal(result, 2)
})

test_that("'%||%' returns 'b' when 'a' is an empty vector", {
  result <- integer() %||% 2
  expect_equal(result, 2)
})

test_that("'%||%' returns 'b' when 'a' is a vector with length 0", {
  result <- numeric(0) %||% 2
  expect_equal(result, 2)
})
