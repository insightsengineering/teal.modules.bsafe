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

# app ----

# Prepare testing data ✓
# Adapt mock ✓
# Check interaction
# Check hidden elements
# Check presence
# Check returned values
# Check code generation

local({
  tns <- tns_factory("mock")

  app <- start_app_driver(teal.modules.bsafe:::mock_robust_map_mod())
  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("robust mean div is hidden when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[1]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_ROB_MEAN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))
  })

  test_that("can read and informative prior weight", {
    expected_value <- c(.3)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_ROB_WEIGHT) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_ROB_WEIGHT)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change ess method", {
    expected_value <- "elir"
    app$set_inputs(!!tns(BSAFE_ID$SEL_ROB_ESS_METHOD) := "Expected Local Information Ratio")
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SEL_ROB_ESS_METHOD)]])
    expect_equal(current_value, expected_value)
  })

  test_that("robust_map_mcmc is data.frame", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["robust_map_mcmc"]]())
    checkmate::expect_class(v, "betaMix")
  })

  test_that("robust_plot is ggplot", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["robust_plot"]]())
    checkmate::expect_class(v, "ggplot")
  })

  test_that("robust_summary is data.frame", {
    v <- shiny::isolate(app$get_values()[["export"]][[tns("r")]][["robust_summary"]]())
    checkmate::expect_data_frame(v)
  })

  test_that("preface text is present", {
    txt <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_PREFACE_ROB_TXT)]][["html"]]
    checkmate::expect_string(txt, min.chars = 1)
  })

  test_that("density function is present", {
    txt <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_ROB_DENSITY_FCT)]][["html"]]
    checkmate::expect_string(txt, min.chars = 1)
  })

  test_that("robust map plot is present", {
    plot <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_ROB_MAP_PLT)]]
    expect_true("src" %in% names(plot))
  })

  test_that("table is present", {
    tbl <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_ROB_SUM_TBL)]]
    checkmate::expect_string(tbl, fixed = "<table")
  })

  test_that("robust_map_mcmc code eval matches value", {
    expect_code_val_match("robust_map_mcmc", app)
  })

  test_that("robust_plot code eval matches value", {
    expect_code_plot_match("robust_plot", app)
  })

  test_that("robust_summary code eval matches value", {
    expect_code_val_match("robust_summary", app)
  })

  app <- start_app_driver(teal.modules.bsafe:::mock_robust_map_mod(BSAFE_CHOICES$SEL_ANALYSIS[2]))
  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("robust mean div is visible when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[2]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_ROB_MEAN)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))
  })

  test_that("can read and change informative prior mean", {
    expected_value <- c(2)
    app$set_inputs(!!tns(BSAFE_ID$SLDR_ROB_MEAN) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SLDR_ROB_MEAN)]])
    expect_equal(current_value, expected_value)
  })
})
