mod_data_preparation_ui <- function(id) {
  ns <- shiny::NS(id)
  side <- list(
    shinyjs::useShinyjs(),
    shiny::selectInput(
      ns(BSAFE_ID$SEL_COLUMN),
      "Select the columns",
      choices = "",
      selected = "",
      multiple = TRUE
    ),
    shiny::uiOutput(ns(BSAFE_ID$OUT_SEL_VAR)),
    shiny::actionButton(
      ns(BSAFE_ID$BUT_ADD_ARM),
      "add arm"
    )
  )
  main <- list(
    shiny::tableOutput(ns(BSAFE_ID$OUT_ARM_SEL))
  )

  return(
    list(side = side, main = main)
  )
}



mod_data_preparation_server <- function(id, data) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    # calculations/functions --------------------------------------------------
    get_names <- function(name, length) {
      helper <- paste0(name, 1)
      for (i in 2:length) {
        helper <- c(helper, paste0(name, i))
      }
      return(helper)
    }

    down_filtering <- function(data, column_names) {
      for (i in seq_along(column_names)) {
        selector <- paste0("SEL_", i)
        elements <- input[[selector]]
        data <- data %>%
          dplyr::filter(.data[[column_names[i]]] %in% {{ elements }})
      }
      if (length(data[, "ARM"]) > 0) {
        data[, "ARM"] <- input$MODAL_INPUT
      }
      return(data)
    }

    shiny::observeEvent(input[["MODAL_ARM_CREATION"]], {
      name <- input$MODAL_INPUT
      selectors <- get_names("SEL_", length(input[[BSAFE_ID$SEL_COLUMN]]))
      param_list <- purrr::map(selectors, function(x) {
        return(input[[x]])
      })
      names(param_list) <- input[[BSAFE_ID$SEL_COLUMN]]
      if (!param_list$ARM %in% rv$arm_list) {
        rv$arm_list[[name]] <- param_list$ARM
        print(rv$arm_list)
      } else {
        warning("The selected arm is already available, please select a different one")
      }
      filtered_data <- down_filtering(data(), input[[BSAFE_ID$SEL_COLUMN]])
      if (length(filtered_data) > 0) {
        if (length(rv[["data"]] > 0)) {
          rv[["data"]] <- dplyr::full_join(rv[["data"]], filtered_data)
        } else {
          rv[["data"]] <- filtered_data
        }
      } else {
        warning("The arm you created has no rows, please select a different combination")
      }
      full_join_data()
      shiny::removeModal()
    })


    # reactive Values Object
    rv <- shiny::reactiveValues(arm_list = list(), data = NULL)

    full_join_data <- function() {
      rv[["data"]] <- dplyr::full_join(rv[["data"]], data())
    }

    # ui element updates ------------------------------------------------------


    # slider input dependent on the new_n slider input in the New Trial Tab
    # Update label of Number of Patients with AE




    shiny::observe({
      choices_helper <- setdiff(names(data()), c("STUDYID", "DOSE", "FREQ", "LENGTH", "TREAT"))
      shiny::updateSelectInput(session,
        inputId = BSAFE_ID$SEL_COLUMN,
        choices = choices_helper
      )
    })

    output[[BSAFE_ID$OUT_SEL_VAR]] <- shiny::renderUI({
      shiny::req(input[[BSAFE_ID$SEL_COLUMN]])
      lapply(seq_along(input[[BSAFE_ID$SEL_COLUMN]]), function(i) {
        shiny::selectInput(ns(paste0("SEL_", i)),
          label = paste0(
            "Select the forms of ",
            input[[BSAFE_ID$SEL_COLUMN]][i]
          ),
          choices = levels(
            factor(
              unique(
                data()[, input[[BSAFE_ID$SEL_COLUMN]][i]]
              )
            )
          ),
          multiple = TRUE
        )
      })
    })

    shiny::outputOptions(output, BSAFE_ID$OUT_SEL_VAR, suspendWhenHidden = FALSE)


    shiny::observeEvent(input[[BSAFE_ID$BUT_ADD_ARM]], {
      shiny::showModal(shiny::modalDialog(
        title = "Name the arm you just created",
        shiny::textInput(ns("MODAL_INPUT"), "Name", ""),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton(ns("Cancel")),
          shiny::actionButton(ns("MODAL_ARM_CREATION"), "OK")
        )
      ))
    })

    output[[BSAFE_ID$OUT_ARM_SEL]] <- shiny::renderTable({
      if (is.null(rv[["data"]])) {
        data()
      } else {
        rv[["data"]]
      }
    })


    return(
      shiny::reactive(
        if (is.null(rv[["data"]])) {
          data()
        } else {
          rv[["data"]]
        }

      )
    )
  }

  shiny::moduleServer(id, mod)
}

mock_data_preparation_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_data_preparation_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    x <- mod_data_preparation_server(
      id = "mock",
      data = shiny::reactive(as.data.frame(teal.modules.bsafe::bsafe_data))
    )

    output[["out"]] <- shiny::renderPrint({
      x[["data"]]()
      utils::str(x)
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
