local({
    # Sample data for testing
data <- data.frame(
  ARM = c("A", "A", "B", "B", "C"),
  SAF_TOPIC = c("X", "Y", "X", "Z", "Z")
)

# Unit tests for get_arm_choices_and_selection function
test_that("get_arm_choices_and_selection returns correct choices when previous selection is empty", {
  choices_selected <- get_arm_choices_and_selection(data, "")
  expect_equal(choices_selected[[1]], c("A", "B", "C"))
})

test_that("get_arm_choices_and_selection returns correct selected value when previous selection is empty", {
  choices_selected <- get_arm_choices_and_selection(data, "")
  expect_equal(choices_selected[[2]], "A")
})

test_that("get_arm_choices_and_selection returns correct choices when previous selection is not empty", {
  choices_selected <- get_arm_choices_and_selection(data, "B")
  expect_equal(choices_selected[[1]], c("A", "B", "C"))
})

test_that("get_arm_choices_and_selection returns correct selected value when previous selection is not empty", {
  choices_selected <- get_arm_choices_and_selection(data, "B")
  expect_equal(choices_selected[[2]], "B")
})

# Unit tests for get_safety_topic_choices_and_selection function
test_that("get_safety_topic_choices_and_selection returns correct choices when there are choices", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "", c("A", "B"))
  expect_equal(choices_selected[[1]], c("X", "Y", "Z"))
})

test_that("get_safety_topic_choices_and_selection returns correct selected value when there are choices", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "", c("A", "B"))
  expect_equal(choices_selected[[2]], "X")
})

test_that("get_safety_topic_choices_and_selection returns correct choices when previous selection is not empty", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "Z", c("A", "B"))
  expect_equal(choices_selected[[1]], c("X", "Y", "Z"))
})

test_that("get_safety_topic_choices_and_selection returns correct selected value when previous selection is not empty", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "Z", c("A", "B"))
  expect_equal(choices_selected[[2]], "Z")
})

test_that("get_safety_topic_choices_and_selection returns NULL choices when there are no choices", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "", c("D"))
  expect_identical(choices_selected[[1]], character(0))
})

test_that("get_safety_topic_choices_and_selection returns NULL selected value when there are no choices", {
  choices_selected <- get_safety_topic_choices_and_selection(data, "", c("D"))
  expect_null(choices_selected[[2]])
})
})


