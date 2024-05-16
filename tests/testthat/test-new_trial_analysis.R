local({

})


# app ----

local({
  tns <- tns_factory("mock")

  app <- shinytest2::AppDriver$new(teal.modules.bsafe:::mock_new_trial_analysis_mod())
  wait_value_idle(app, input = tns(BSAFE_ID$SLDR_N_PAT))
  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  # Check interaction

  test_that("ae div side and main is hidden when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[1]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_AE)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))

    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_AE_MAIN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))
  })

  test_that("inci div side and main is visible when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[1]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_INCI)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))

    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_INCI_MAIN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))
  })

  test_that("can read and change number of patients", {
    expected_value <- c(20)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_N_PAT) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_N_PAT)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change number of patients with AE", {
    expected_value <- c(10)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_N_AE) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_N_AE)]])
    expect_equal(current_value, expected_value)
  })

  test_that("number of patients with AE slider is updated when number of patients change", {
    expected_value <- c(5)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_N_PAT) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_N_PAT)]])
    expect_equal(current_value, expected_value)

    app$set_inputs(!!tns(BSAFE_ID$SLDR_N_AE) := 6)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_N_AE)]])
    expect_equal(current_value, 5)
  })

  test_that("mix density plot is present", {
    plot <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_COMPARE_PLT)]]
    expect_true("src" %in% names(plot))
  })

  test_that("summary table is present", {
    tbl <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_COMPARE_SUM_TBL)]]
    checkmate::expect_string(tbl, fixed = "<table")
  })

  test_that("new_trial_analysis is a string", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["new_trial_analysis"]]())
    checkmate::expect_data_frame(v)
  })

  test_that("post_dist is a string", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["post_dist"]]())
    checkmate::expect_class(v, "betaMix")
  })

  test_that("current_trial_data is a string", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["current_trial_data"]]())
    checkmate::expect_list(v)
  })

  test_that("compare_plot is a ggplot", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["compare_plot"]]())
    checkmate::expect_class(v, "ggplot")
  })

  test_that("map_summary_table is a data.frame", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["compare_summary_table"]]())
    checkmate::expect_data_frame(v)
  })

  test_that("compare_plot code eval matches plot", {
    expect_code_plot_match("compare_plot", app)
  })

  test_that("compare_summary_table code eval matches value", {
    expect_code_val_match("compare_summary_table", app)
  })



  app <- shinytest2::AppDriver$new(teal.modules.bsafe:::mock_new_trial_analysis_mod(BSAFE_CHOICES$SEL_ANALYSIS[2]))
  wait_value_idle(app, input = tns(BSAFE_ID$SLDR_AE_FIRST_OCCURENCE))
  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("ae div side and main is visible when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[2]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_AE)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))

    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_AE_MAIN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))
  })

  test_that("inci div side and main is hidden when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[2]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_INCI)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))

    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_NTA_INCI_MAIN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))
  })

  test_that("can read and change first occurence", {
    expected_value <- c(11)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_AE_FIRST_OCCURENCE) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_AE_FIRST_OCCURENCE)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change cummulative time", {
    expected_value <- c(21)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_CUMM_TIME_FIRST_AE) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_CUMM_TIME_FIRST_AE)]])
    expect_equal(current_value, expected_value)
  })
})
