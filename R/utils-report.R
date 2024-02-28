reactive_snapshot <- function(expr) {
  q <- rlang::enquo(expr)
  v <- shiny::reactiveVal()
  r <- shiny::reactive(q, quoted = TRUE)
  shiny::observe({
    v(r())
  })
  v
}
