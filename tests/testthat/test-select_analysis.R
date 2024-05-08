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

# app tests ----
local({
tns <- tns_factory("mock")

app <- start_app_driver(teal.modules.bsafe:::mock_select_analysis_mod())
on.exit(if ("stop" %in% names(app)) app$stop())

fail_if_app_not_started <- function() {
  if (is.null(app)) rlang::abort("App could not be started")
}

test_that("can read and change treatment", {
  expected_value <- "Treatment"
  app$set_inputs(!!tns(BSAFE_ID$SEL_TRT) := expected_value)
  current_value <- shiny::isolate(app$get_values()[["export"]][["r"]][["treatment"]]())
  expect_equal(current_value, expected_value)
})

test_that("can read and change change analysis_type", {
  expected_value <- "Exposure-adjusted AE rate"
  app$set_inputs(!!tns(BSAFE_ID$SEL_ANALYSIS) := expected_value)
  current_value <- shiny::isolate(app$get_values()[["export"]][["r"]][["analysis_type"]]())
  expect_equal(current_value, expected_value)
})

test_that("can read and change safety_topic", {
  expected_value <- "Vomitting"
  app$set_inputs(!!tns(BSAFE_ID$SEL_SAF_TOPIC) := expected_value)
  current_value <- shiny::isolate(app$get_values()[["export"]][["r"]][["safety_topic"]]())
  expect_equal(current_value, expected_value)
})

test_that("can read and change seed", {
  expected_value <- 1
  app$set_inputs(!!tns(BSAFE_ID$SET_SEED) := expected_value)
  current_value <- as.numeric(shiny::isolate(app$get_values()[["export"]][["r"]][["seed"]]()))
  expect_equal(current_value, expected_value)
})

test_that("returned data is a data.frame",{
  current_value <- shiny::isolate(app$get_values()[["export"]][["r"]][["data"]]())
  checkmate::expect_data_frame(current_value)
})

test_that("exported code reproduces the output",{
  expected_val <- shiny::isolate(app$get_values()[["export"]][["r"]][["data"]]())
  current_val <- eval(shinymeta::expandChain(shiny::isolate(app$get_values()[["export"]][["r"]][["data"]]())))
  expect_identical(current_val, expected_val)
})



# SMOKE TESTING
# TESTING THE REPORT AND THE GENERATED CODE PER MODULE?

})