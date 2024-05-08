is_CI <- isTRUE(as.logical(Sys.getenv("CI"))) # nolint
is_shiny_test <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | is_CI

run_shiny_tests <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) || isTRUE(as.logical(Sys.getenv("CI")))
suspect_check <- any(names(Sys.getenv()) == "_R_CHECK_CRAN_INCOMING_")

skip_if_not_running_shiny_tests <- function() testthat::skip_if_not(run_shiny_tests, message = "Not CI or TEST_LOCAL")
skip_if_suspect_check <- function() testthat::skip_if(suspect_check, message = "Suspected check")

tns_factory <- function(id) function(...) paste0(c(id, as.character(list(...))), collapse = "-")

# `expr` must be a quosure or a regular call, in both cases they must be self-contained as they will be deparsed
# and run in another process
start_app_driver <- function(expr) {
  root_app <- if (run_shiny_tests && !suspect_check) {
    app_dir <- if (testthat::is_testing()) {
      "app/app.R"
    } else {
      "tests/testthat/app/app.R"
    }

    call <- if (rlang::is_quosure(expr)) rlang::get_expr(expr) else substitute(expr)

    # tryCatch to avoid snapshots being deleted when the app cannot be started
    tryCatch(
      { 
        app <- shinytest2::AppDriver$new(
          app_dir = app_dir,
          seed = 1,
          options = list(
            "__test_fn_expr" = deparse1(call),
            "__use_load_all" = isTRUE(as.logical(Sys.getenv("TEST_LOCAL")))
          )
        )
        app$wait_for_idle()
        app
      },
      condition = function(e) {
        if (exists("app") && "stop" %in% names(app)) app$stop()
        print(e)
        NULL
      }
    )
  } else {
    NULL
  }
}
