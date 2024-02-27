# ui function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
ui_dv_poc_example <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    poc_UI(ns("bsafe"), header = shiny::tagList(
      teal.reporter::add_card_button_ui(ns("add")),
      teal.reporter::download_report_button_ui(ns("download")),
      teal.reporter::reset_report_button_ui(ns("resets"))
    ))
  )
}

# server function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
srv_dv_poc_example <- function(id, data, reporter, filter_panel_api, dataset_name) {
  shiny::moduleServer(id, function(input, output, session) {
    dataset <- shiny::reactive({
      d <- data()[[dataset_name]]
      attr(d, "code") <- teal.data::get_code(data(), datanames = dataset_name)
      d
    })

    to_report <- poc_server(
      "bsafe",
      dataset = dataset,
      filter_panel_api = filter_panel_api,
      reporter = reporter
    )

    generic_card_function <- function(card = teal.reporter::ReportCard$new()) {
      report_creators <- list(
        map = function(card, contents) {
          card$set_name(contents[["name"]])

          card$append_text("Forest", "header2")
          card$append_text(contents[["forest"]][["code"]], "verbatim")
          card$append_text(contents[["forest"]][["prior_txt"]], "verbatim")
          card$append_plot(contents[["forest"]][["plot"]])

          card$append_text("MAP Prior", "header2")
          card$append_text(contents[["map"]][["code"]], "verbatim")
          card$append_plot(contents[["map"]][["plot"]])

          card$append_text("Summary Table", "header2")
          card$append_text(contents[["summary"]][["code"]], "verbatim")
          card$append_table(contents[["summary"]][["table"]])
          card
        },
        robust = function(card, contents) {
          card$set_name(contents[["name"]])

          card$append_text("Robust", "header2")
          card$append_text(contents[["plot"]][["code"]], "verbatim")
          # Mathjax is not supported by card reporter
          # card$append_text(contents[["plot"]][["prior_txt"]], "verbatim")
          # Mathjax is not supported by card reporter
          # card$append_text(contents[["plot"]][["formula"]], "verbatim")
          card$append_plot(contents[["plot"]][["plot"]])

          card$append_text("Summary Table", "header2")
          card$append_text(contents[["summary"]][["code"]], "verbatim")
          card$append_table(contents[["summary"]][["table"]])
          card
        }
      )

      if (to_report[["active_tab"]]() == "MAP Prior") {
        return(report_creators[["map"]](card, to_report[["map"]]()))
      }
      if (to_report[["active_tab"]]() == "Robust MAP Prior") {
        return(report_creators[["robust"]](card, to_report[["robust"]]()))
      }
    }

    teal.reporter::add_card_button_srv("add", reporter = reporter, card_fun = generic_card_function)
    teal.reporter::download_report_button_srv("download", reporter = reporter)
    teal.reporter::reset_report_button_srv("reset", reporter)
  })
}

# the function which creates the teal module for users
tm_dv_poc_example <- function(label = "BSAFE", dataset_name) {
  checkmate::assert_character(label)


  teal::module(
    label = label,
    server = srv_dv_poc_example,
    server_args = list(dataset_name = dataset_name),
    ui = ui_dv_poc_example,
    datanames = "all"
  )
}


mock_teal <- function() {
  data <- teal.data::teal_data(
    bsafe_data = teal.modules.bsafe::bsafe_data,
    code = expression(bsafe_data <- teal.modules.bsafe::bsafe_data)
  ) |>
    teal.data::verify()

  app <- teal::init(
    data = data,
    modules = list(
      tm_dv_poc_example(
        label = "teal.modules.bsafe",
        dataset_name = "bsafe_data"
      )
    ),
    header = "DaVinci test of a bsafe as teal module"
  )
  shiny::shinyApp(app$ui, app$server)
}
