local({
    
test_that("preface_prior_txt returns correct preface for MAP approach", {
  sel_analysis <- BSAFE_CHOICES$SEL_ANALYSIS[1]
  expected <- "Using a MAP approach, the prior approximated as the Beta mixture distribution:"
  result <- preface_prior_txt(sel_analysis)
  expect_equal(result, expected)
})

test_that("preface_prior_txt returns correct preface for log scale approach", {
  sel_analysis <- BSAFE_CHOICES$SEL_ANALYSIS[2]
  expected <- "Using a MAP approach, the log scale of the prior approximated as the Normal mixture distribution:"
  result <- preface_prior_txt(sel_analysis)
  expect_equal(result, expected)
})

test_that("preface_prior_txt returns NULL for unknown selection analysis", {
  sel_analysis <- "Unknown"
  expected <- NULL
  result <- preface_prior_txt(sel_analysis)
  expect_equal(result, expected)
})

})