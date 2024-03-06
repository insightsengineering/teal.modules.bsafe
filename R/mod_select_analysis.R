mod_select_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tags$hr(),
      shiny::selectInput(ns(BSAFE_ID$SEL_TRT),
        "Select patients with the respective treatment",
        choices = c(""),
        multiple = FALSE
      ),
      shiny::selectInput(ns(BSAFE_ID$SEL_ANALYSIS),
        "Select safety analysis",
        choices = BSAFE_CHOICES$SEL_ANALYSIS
      ),
      shiny::selectInput(ns(BSAFE_ID$SEL_SAF_TOPIC),
        "Select safety topic",
        choices = c(
          ""
        )
      ),
      shiny::numericInput(ns(BSAFE_ID$SET_SEED), "Used seed:",
        min = 0,
        value = round(as.numeric(Sys.time()), 0)
      ),
      shiny::checkboxInput(ns(BSAFE_ID$CB_POOLED), "Pool by study", value = TRUE)
    ),
    shiny::mainPanel(
      shiny::htmlOutput(ns(BSAFE_ID$OUT_FILE_TABLE)), # historical trial table
    ),
  )
}

mod_select_analysis_server <- function(id, data) {
  module <- function(input, output, session) {

    shiny::observe({
      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_TRT,
        choices = unique(data()[, "ARM"])
      )
    })

    shiny::observe({
      safety_topics <- as.character(unlist(data()[, "SAF_TOPIC"]))
      choices_ae <- safety_topics[as.character(unlist(data()[, "ARM"])) == input[[BSAFE_ID$SEL_TRT]]]

      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_SAF_TOPIC,
        choices = choices_ae
      )
    })

    # Data table preparation
    prepared_data <- shinymeta::metaReactive({
      bsafe::data_table_prep(
        input_data = ..(data()),
        select_analysis = ..(input[[BSAFE_ID$SEL_ANALYSIS]]),
        saf_topic = ..(input[[BSAFE_ID$SEL_SAF_TOPIC]]),
        select_btrt = ..(input[[BSAFE_ID$SEL_TRT]]),
        bool_pooled = ..(input[[BSAFE_ID$CB_POOLED]])
      )
    })

    return(prepared_data)
  }

  shiny::moduleServer(id, module)
}

mock_select_analysis_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_select_analysis_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    x <- mod_select_analysis_server(
      id = "mock",
      data = shiny::reactive(as.data.frame(teal.modules.bsafe::bsafe_data))
    )

    output[["out"]] <- shiny::renderPrint({
      utils::str(x())
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}