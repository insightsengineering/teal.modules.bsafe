# ui function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
ui_bsafe <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bsafe_UI(ns("bsafe"), header = shiny::tagList(
      teal.reporter::add_card_button_ui(ns("add")),
      teal.reporter::download_report_button_ui(ns("download")),
      teal.reporter::reset_report_button_ui(ns("resets"))
    ))
  )
}

# server function for the module
# histogram_var is a teal.transform::data_extract_spec object
# specifying which columns of which datasets users can choose
srv_bsafe <- function(id, data, reporter, filter_panel_api, dataset_name) {
  shiny::moduleServer(id, function(input, output, session) {
    dataset <- shiny::reactive({
      d <- data()[[dataset_name]]
      attr(d, "code") <- rlang::parse_expr(paste0("{", teal.data::get_code(data(), datanames = dataset_name), "}"))
      d
    })

    to_report <- bsafe_server(
      "bsafe",
      dataset = dataset
    )

    generic_card_function <- function(card = teal.reporter::ReportCard$new()) {
      card$append_text("Code", "header2")
      code <- shinymeta::deparseCode(to_report()[["code"]]) |>
        shinymeta::formatCode(, formatter = styler::style_text) |>
        paste(collapse = "\n")
      card$append_text(code, "verbatim")
      card$append_text("MAP prior", "header2")
      card$append_text("Forest plot", "header3")
      card$append_plot(to_report()[["forest_plot"]])
      card$append_text("Map Summary table", "header3")
      card$append_table(to_report()[["map_summary_table"]])
      card$append_text("Robust MAP", "header2")
      card$append_text("Robust plot", "header3")
      card$append_plot(to_report()[["robust_plot"]])
      card$append_text("Robust Summary table", "header3")
      card$append_table(to_report()[["robust_summary"]])
      card$append_text("New trial analysis", "header2")
      card$append_text("Compare plot", "header3")
      card$append_plot(to_report()[["compare_plot"]])
      card$append_text("Compare Summary table", "header3")
      card$append_table(to_report()[["compare_summary_table"]])
      card$append_text("Decision Making", "header2")
      card$append_text(to_report()[["dm_header"]], "header3")
      card$append_text(to_report()[["dm_preface"]])
      card$append_plot(to_report()[["stat_inf_plot"]])
      card$append_text(to_report()[["auc"]])
      card$append_table(to_report()[["preset_statements"]])
      card
    }

    teal.reporter::add_card_button_srv("add", reporter = reporter, card_fun = generic_card_function)
    teal.reporter::download_report_button_srv("download", reporter = reporter)
    teal.reporter::reset_report_button_srv("reset", reporter)
  })
}

# the function which creates the teal module for users
tm_bsafe <- function(label = "BSAFE", dataset_name) {
  checkmate::assert_character(label)


  teal::module(
    label = label,
    server = srv_bsafe,
    server_args = list(dataset_name = dataset_name),
    ui = ui_bsafe,
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
      tm_bsafe(
        label = "teal.modules.bsafe",
        dataset_name = "bsafe_data"
      )
    ),
    header = "DaVinci test of a bsafe as teal module"
  )
  shiny::shinyApp(app$ui, app$server)
}
