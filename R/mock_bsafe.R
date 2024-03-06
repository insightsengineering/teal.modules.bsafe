#' @title mock_poc_bsafe_app
#'
#' @description dummy mock app function
#'
#' @export
mock_poc_bsafe_app <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      poc_UI(
        id = "bsafe"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    x <- poc_server(
      id = "bsafe",
      dataset = shiny::reactive(as.data.frame(teal.modules.bsafe::bsafe_data))
    )

    output[["out"]] <- shiny::renderPrint({
      # utils::str(x[["map"]]())
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
