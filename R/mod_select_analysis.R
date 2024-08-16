mod_select_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
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
  )

  main <- shiny::tableOutput(ns(BSAFE_ID$OUT_FILE_TABLE))
  list(side = side, main = main)
}

mod_select_analysis_server <- function(id, data) {
  module <- function(input, output, session) {
    shiny::observe({
      cs <- get_arm_choices_and_selection(data(), input[[BSAFE_ID$SEL_TRT]])
      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_TRT,
        choices = cs[["choices"]],
        selected = cs[["selected"]]
      )
    })

    shiny::observe({
      cs <- get_sfty_choices_select(data(), input[[BSAFE_ID$SEL_SAF_TOPIC]], input[[BSAFE_ID$SEL_TRT]])

      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_SAF_TOPIC,
        choices = cs[["choices"]],
        selected = cs[["selected"]]
      )
    })

    # Data table preparation
    prepared_data <- shinymeta::metaReactive(
      {
        bsafe::data_table_prep(
          input_data = ..(data()),
          select_analysis = ..(input[[BSAFE_ID$SEL_ANALYSIS]]),
          saf_topic = ..(input[[BSAFE_ID$SEL_SAF_TOPIC]]),
          select_btrt = ..(input[[BSAFE_ID$SEL_TRT]]),
          bool_pooled = ..(input[[BSAFE_ID$CB_POOLED]])
        )
      },
      varname = "prepared_data"
    )

    output[[BSAFE_ID$OUT_FILE_TABLE]] <- shiny::renderTable({
      bsafe::data_table_prep(
        input_data = data(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]],
        select_btrt = input[[BSAFE_ID$SEL_TRT]],
        bool_pooled = input[[BSAFE_ID$CB_POOLED]]
      )
    })

    r <- list(
      data = prepared_data,
      analysis_type = shiny::reactive(input[[BSAFE_ID$SEL_ANALYSIS]]),
      safety_topic = shiny::reactive(input[[BSAFE_ID$SEL_SAF_TOPIC]]),
      treatment = shiny::reactive(input[[BSAFE_ID$SEL_TRT]]),
      seed = shiny::reactive(input[[BSAFE_ID$SET_SEED]])
    )

    do.call(shiny::exportTestValues, as.list(environment()))

    return(r)
  }

  shiny::moduleServer(id, module)
}

get_arm_choices_and_selection <- function(data, previous_selection) {
  choices <- unique(data[["ARM"]])
  if (length(choices) > 0) {
    if (!identical(previous_selection, "")) {
      selected <- previous_selection
    } else {
      selected <- choices[[1]]
    }
  } else {
    selected <- NULL
  }
  list(choices = choices, selected = selected)
}

get_sfty_choices_select <- function(data, previous_selection, arm_selection) {
  choices <- unique(data[["SAF_TOPIC"]][data[["ARM"]] %in% arm_selection])
  if (length(choices) > 0) {
    if (!identical(previous_selection, "")) {
      selected <- previous_selection
    } else {
      selected <- choices[[1]]
    }
  } else {
    selected <- NULL
  }
  list(choices = choices, selected = selected)
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
    r <- mod_select_analysis_server(
      id = "mock",
      data = shinymeta::metaReactive(as.data.frame(teal.modules.bsafe::bsafe_data), varname = "data")
    )

    output[["out"]] <- shiny::renderPrint({
      r[["data"]]()
      utils::str(r)
    })

    do.call(shiny::exportTestValues, as.list(environment()))
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
