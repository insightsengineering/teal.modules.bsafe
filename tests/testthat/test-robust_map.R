local({    
test_that("preface_rob_txt returns correct preface for weakly informative conjugate component", {
  sel_analysis <- BSAFE_CHOICES$SEL_ANALYSIS[1]
  rob_weight <- 0.5
  rob_mean <- 0.25
  expected <- "Based on a weakly informative conjugate component with weight 0.5 and mean 0.25 the robust MAP prior is approximated as w * (MAP Prior) + (1 - w) * (weakly informative prior): "
  result <- preface_rob_txt(sel_analysis, rob_weight, rob_mean)
  expect_equal(result, expected)
})

test_that("preface_rob_txt returns correct preface for EX-NEX approach", {
  sel_analysis <- BSAFE_CHOICES$SEL_ANALYSIS[2]
  rob_weight <- 0.75
  rob_mean <- 0.35
  expected <- "Based on a EX-NEX approach with non-exchangeable probability P(nex) =  0.75 and mean 0.35 on the log scale of the robust MAP prior is approximated as (1 - P(nex)) * (MAP Prior) + P(nex) * (weakly informative prior): "
  result <- preface_rob_txt(sel_analysis, rob_weight, rob_mean)
  expect_equal(result, expected)
})

test_that("preface_rob_txt returns NULL for unknown selection analysis", {
  sel_analysis <- "Unknown"
  rob_weight <- 0.5
  rob_mean <- 0.25
  expected <- NULL
  result <- preface_rob_txt(sel_analysis, rob_weight, rob_mean)
  expect_equal(result, expected)
})
})