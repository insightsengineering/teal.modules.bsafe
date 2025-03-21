chromote::local_chrome_version(binary = "chrome-headless-shell", quiet = FALSE)

is_CI <- isTRUE(as.logical(Sys.getenv("CI"))) # nolint
is_shiny_test <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | is_CI

run_shiny_tests <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) || isTRUE(as.logical(Sys.getenv("CI")))
suspect_check <- any(names(Sys.getenv()) == "_R_CHECK_CRAN_INCOMING_")

skip_if_not_running_shiny_tests <- function() testthat::skip_if_not(run_shiny_tests, message = "Not CI or TEST_LOCAL")
skip_if_suspect_check <- function() testthat::skip_if(suspect_check, message = "Suspected check")

tns_factory <- function(id) function(...) paste0(c(id, as.character(list(...))), collapse = "-")

wait_value_idle <- function(app, ...) {
  # Worst case scenario false positive
  app$wait_for_value(...)
  app$wait_for_idle()
}

# Code helpers ----

expect_code_val_match <- function(n, app) {
  export <- app$get_values()[["export"]]
  expected_val <- shiny::isolate(export[["r"]][[n]]())
  current_val <- rlang::eval_tidy(shinymeta::expandChain(shiny::isolate(export[["r"]][[n]]())))
  expect_equal(current_val, expected_val)
}

expect_code_print_match <- function(n, app) {
  # Objects may contain environments that make the expect_equal unusable.
  # Here we compare the print method object may be less robust but enough for our purpose
  # Akin to plot match where we do not compare the plot code representation but the SVG representation
  export <- app$get_values()[["export"]]
  expected_val <- capture.output(print(shiny::isolate(export[["r"]][[n]]())))
  current_val <- capture.output(print(rlang::eval_tidy(shinymeta::expandChain(shiny::isolate(export[["r"]][[n]]())))))
  expect_equal(current_val, expected_val)
}

expect_code_plot_match <- function(n, app) {
  export <- app$get_values()[["export"]]
  tf1 <- tempfile()
  tf2 <- tempfile()
  fig1 <- shiny::isolate(export[["r"]][[n]]())
  vdiffr::write_svg(fig1, tf1)
  fig2 <- rlang::eval_tidy(shinymeta::expandChain(shiny::isolate(export[["r"]][[n]]())))
  vdiffr::write_svg(fig2, tf2)
  expect_identical(readLines(tf1), readLines(tf2))
}
