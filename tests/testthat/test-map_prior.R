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

  test_that("preface_prior_txt returns an error for unknown selection analysis", {
    sel_analysis <- "Unknown"
    expect_error(preface_prior_txt(sel_analysis), regexp = "has additional elements", fixed = TRUE)
  })
})

# app tests ----
local({
  tns <- tns_factory("mock")

  app <- shinytest2::AppDriver$new(teal.modules.bsafe:::mock_map_prior_mod())
  wait_value_idle(app, input = tns(BSAFE_ID$SEL_TAU))
  
  
  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("map prior is calculated when button is pressed", {
    app$click(tns("submit"))
    app$wait_for_idle()
    checkmate::expect_class(shiny::isolate(app$get_values()[["export"]][["r"]][["map_mcmc"]]()), "gMAP")
  })

  test_that("forest plot is present", {
    plot <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_FOREST_PLT)]]
    expect_true("src" %in% names(plot))
  })

  test_that("mix density plot is present", {
    plot <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_MIX_DENSITY_PLT)]]
    expect_true("src" %in% names(plot))
  })

  test_that("preface prior text is present", {
    txt <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_PREFACE_PRIOR_TXT)]][["html"]]
    checkmate::expect_string(txt, min.chars = 1)
  })

  test_that("density function is present", {
    txt <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_DENSITY_FCT)]][["html"]]
    checkmate::expect_string(txt, min.chars = 1)
  })

  test_that("summary table is present", {
    tbl <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL)]]
    checkmate::expect_string(tbl, fixed = "<table")
  })

  test_that("map_mcmc returned values is a gMAP object", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["map_mcmc"]]())
    checkmate::expect_class(v, "gMAP")
  })

  test_that("param_approx returned values is a EM object", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["param_approx"]]())
    checkmate::expect_class(v, "EM")
  })

  test_that("adj_tau returned values is a numeric", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["adj_tau"]]())
    checkmate::expect_numeric(v)
  })

  test_that("ess_method is a subset from BSAFE_CHOICES$SEL_ESS_METHOD", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["ess_method"]]())
    checkmate::expect_subset(v, BSAFE_CHOICES$SEL_ESS_METHOD)
  })

  test_that("forest_plot is a ggplot", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["forest_plot"]]())
    checkmate::expect_class(v, "ggplot")
  })

  test_that("map_summary_table is a data.frame", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["map_summary_table"]]())
    checkmate::expect_data_frame(v)
  })

  test_that("map_mcmc code eval matches print", {
    expect_code_print_match("map_mcmc", app)
  })

  test_that("param_approx code eval matches print", {
    expect_code_print_match("param_approx", app)
  })

  test_that("adj_tau code eval matches value", {
    expect_code_val_match("adj_tau", app)
  })

  test_that("forest plot code eval matches plot", {
    expect_code_plot_match("forest_plot", app)
  })

  test_that("map_summary_table code eval matches value", {
    expect_code_val_match("map_summary_table", app)
  })

  test_that("can read and change tau", {
    expected_value <- "HalfNormal"
    app$set_inputs(!!tns(BSAFE_ID$SEL_TAU) := "Half-normal")
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SEL_TAU)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change hist borrow", {
    expected_value <- "Small"
    app$set_inputs(!!tns(BSAFE_ID$SEL_HIST_BORROW) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SEL_HIST_BORROW)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change effective sampling method", {
    expected_value <- "elir"
    app$set_inputs(!!tns(BSAFE_ID$SEL_ESS_METHOD) := "Expected Local Information Ratio")
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SEL_ESS_METHOD)]])
    expect_equal(current_value, expected_value)
  })

  test_that("menu prior is invalidated when any change in the input values to map prior happens", {
    button_id <- c(
      "invalidate_data",
      "invalidate_analysis",
      "invalidate_seed"
    )

    purrr::walk(button_id, function(id) {
      app$click(id)
      app$wait_for_idle()
      expect_error(class = "shiny.silent.error", shiny::isolate(app$get_values()[["export"]][["r"]][["map_mcmc"]]()))
      app$click(tns("submit"))
      app$wait_for_idle()
    })

    # adj_tau trough history borrow

    app$set_inputs(!!tns(BSAFE_ID$SEL_HIST_BORROW) := "Large")
    app$wait_for_idle()
    expect_error(class = "shiny.silent.error", shiny::isolate(app$get_values()[["export"]][["r"]][["map_mcmc"]]()))

    # sel_tau
    # Only one value in sel tau is available
    # app$set_inputs(!!tns(BSAFE_ID$SEL_TAU) := "Half-normal")
    # app$wait_for_idle()
    # expect_error(class = "shiny.silent.error", shiny::isolate(app$get_values()[["export"]][["r"]][["map_mcmc"]]()))
  })
})
