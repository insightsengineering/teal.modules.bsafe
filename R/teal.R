# ui function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
ui_dv_poc_example <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code"),
    poc_UI(ns("bsafe"))
  )
}

# server function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
srv_dv_poc_example <- function(id, data, reporter, filter_panel_api, dataset_name) {
  checkmate::assert_class(data, "tdata")
  shiny::moduleServer(id, function(input, output, session) {
    dataset <- shiny::reactive(data[[dataset_name]]())

    poc_server(
      "bsafe",
      dataset = dataset,
      dataset_tdata = data,
      reporter = reporter,
      filter_panel_api = filter_panel_api
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = "Non Functional",
      title = "R Code"
    )
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
    filters = "all"
  )
}


mock_teal <- function() {
  app <- teal::init(
    data = list(bsafe_data = teal.modules.bsafe::bsafe_data),
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
