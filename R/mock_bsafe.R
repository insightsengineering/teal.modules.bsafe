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
      )
    )
  }

  server <- function(input, output, session) {
    poc_server(
      id = "bsafe",
      dataset = shiny::reactive(as.data.frame(bsafe_data))
    )
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
