# nocov start

#' @title mock_poc_bsafe_app
#'
#' @description dummy mock app function
#'
#' @export
mock_bsafe <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      bsafe_UI(
        id = "bsafe"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    x <- bsafe_server(
      id = "bsafe",
      dataset = shiny::reactive(
        structure(
          as.data.frame(
            teal.modules.bsafe::bsafe_data
          ),
          code = "teal.modules.bsafe::bsafe_data"
        )
      )
    )

    output[["out"]] <- shiny::renderPrint({
      x()$code
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}

# nocov end
