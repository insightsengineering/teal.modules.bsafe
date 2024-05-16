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

# app tests ----
local({
  tns <- tns_factory("mock")

  app <- shinytest2::AppDriver$new(teal.modules.bsafe:::mock_decision_making_mod())
  # Wait for the app to be in place
  wait_value_idle(app, input = tns(BSAFE_ID$SEL_DIST))

  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("can read and change distribution", {
    expected_value <- "Likelihood"
    app$set_inputs(!!tns(BSAFE_ID$SEL_DIST) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$SEL_DIST)]])
    expect_equal(current_value, expected_value)
  })

  test_that("can read and change non ae perc slider", {
    expected_value <- c(5, 10)
    app$set_inputs(!!tns(BSAFE_ID$OUT_PERC_SLDR) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$OUT_PERC_SLDR)]])
    expect_equal(current_value, expected_value)
  })

  test_that("ae div is hidden when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[1]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_DM_AE)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))
  })

  test_that("inci div is visible when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[1]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_DM_INCI)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))
  })

  test_that("header element is present", {
    header <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_DM_HEADER_TXT)]][["html"]]
    checkmate::expect_string(header, min.chars = 1, null.ok = FALSE)
  })

  test_that("preface element is present", {
    preface <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_DM_PREFACE_TXT)]][["html"]]
    checkmate::expect_string(preface, min.chars = 1, null.ok = FALSE)
  })

  test_that("plot is present", {
    plot <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_STAT_INF_DENSITY_PLT)]]
    expect_true("src" %in% names(plot))
  })

  test_that("auc text element is present", {
    auc_text <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_AREA_UNDER_CURVE)]]
    checkmate::expect_string(auc_text, min.chars = 1, null.ok = FALSE)
  })

  test_that("preset_statements table is present", {
    pe_table <- app$get_values()[["output"]][[tns(BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL)]]
    opening_tag <- regmatches(pe_table, regexpr("<table[^>]*>", pe_table))
    expect_length(opening_tag, 1)
  })

  test_that("exported preset statements code matches output", {
    expected_val <- shiny::isolate(app$get_values()[["export"]][["r"]][["preset_statements"]]())
    current_val <- rlang::eval_tidy(shinymeta::expandChain(shiny::isolate(app$get_values()[["export"]][["r"]][["preset_statements"]]())))
    expect_equal(current_val, expected_val)
  })

  test_that("exported plot code matches output", {
    tf1 <- tempfile()
    tf2 <- tempfile()
    fig1 <- shiny::isolate(app$get_values()[["export"]][["r"]][["stat_inf_plot"]]())
    vdiffr::write_svg(fig1, tf1)
    fig2 <- rlang::eval_tidy(shinymeta::expandChain(shiny::isolate(app$get_values()[["export"]][["r"]][["stat_inf_plot"]]())))
    vdiffr::write_svg(fig2, tf2)
    expect_identical(readLines(tf1), readLines(tf2))
  })
})

# app tests ----
local({
  tns <- tns_factory("mock")

  app <- shinytest2::AppDriver$new(teal.modules.bsafe:::mock_decision_making_mod(BSAFE_CHOICES$SEL_ANALYSIS[2]))

  # Wait for the app to be in place
  wait_value_idle(app, input = tns(BSAFE_ID$OUT_AE_PERC_SLDR))

  on.exit(if ("stop" %in% names(app)) app$stop())

  fail_if_app_not_started <- function() {
    if (is.null(app)) rlang::abort("App could not be started")
  }

  test_that("can read and change ae perc slider", {
    expected_value <- c(0, 2)
    app$set_inputs(!!tns(BSAFE_ID$OUT_AE_PERC_SLDR) := expected_value)
    app$wait_for_idle()
    current_value <- shiny::isolate(app$get_values()[["input"]][[tns(BSAFE_ID$OUT_AE_PERC_SLDR)]])
    expect_equal(current_value, expected_value)
  })

  test_that("ae div is hidden when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[2]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_DM_AE)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_false(grepl("display: none", opening_tag))
  })

  test_that("inci div is visible when analysis_type is BSAFE_CHOICES$SEL_ANALYSIS[2]", {
    div <- app$get_html(paste0("#", tns(BSAFE_ID$DIV_DM_INCI)))
    opening_tag <- regmatches(div, regexpr("<div[^>]*>", div))
    expect_true(grepl("display: none", opening_tag))
  })
})
