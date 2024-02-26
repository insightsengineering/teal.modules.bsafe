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
    dataset <- shiny::reactive({data()[[dataset_name]]})

    to_report <- poc_server(
      "bsafe",
      dataset = dataset,
      filter_panel_api = filter_panel_api,
      reporter = reporter
    )

    map_card_fun <- function(card = teal.reporter::ReportCard$new(), comment) {
      card$set_name(to_report[["map"]]()[["name"]])
      #   # card$append_text(filter_panel_api$get_filter_state(), "verbatim") # nolint

      card$append_text("Forest", "header")
      card$append_text(to_report[["map"]]()[["forest"]][["code"]], "verbatim")
      card$append_text(to_report[["map"]]()[["forest"]][["prior_txt"]], "verbatim")
      card$append_plot(to_report[["map"]]()[["forest"]][["plot"]])

      card$append_text("MAP Prior", "header")
      card$append_text(to_report[["map"]]()[["map"]][["code"]], "verbatim")
      card$append_plot(to_report[["map"]]()[["map"]][["plot"]])

      card$append_text("Summary Table", "header")
      card$append_text(to_report[["map"]]()[["summary"]][["code"]], "verbatim")
      card$append_plot(to_report[["map"]]()[["summary"]][["plot"]])
    }

    teal.reporter::add_card_button_srv("add", reporter = reporter, card_fun = map_card_fun)
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
  app <- teal::init(
    data = teal.data::teal_data(bsafe_data = teal.modules.bsafe::bsafe_data),
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
