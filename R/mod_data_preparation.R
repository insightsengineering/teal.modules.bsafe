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
    shiny::div(
      style = "display: flex; align-items: baseline; column-gap: 10px",
      shiny::textInput(inputId = ns("new_arm_name"), label = NULL, placeholder = "New arm name"),
      shiny::actionButton(ns(BSAFE_ID$BUT_ADD_ARM), "+")
    ),
    shiny::uiOutput(ns("current_arms"))
  )

  main <- list(
    shiny::tableOutput(ns(BSAFE_ID$OUT_ARM_SEL))
  )

  return(
    list(side = side, main = main)
  )
}

#' New arm creation
#'
#' @param data original data
#'
#' @param new_arm new arm name
#'
#' @param col_vals filtering criteria to create the new arm
#'
#' @export
#'
create_arm <- function(data, new_arm, col_vals) {
  mask <- TRUE
  for (idx in seq_along(col_vals)) {
    col_name <- names(col_vals)[[idx]]
    col_val <- col_vals[[col_name]]
    mask <- mask & data[[col_name]] %in% col_val
  }

  data <- data[mask, , drop = FALSE]

  if (nrow(data) > 0) data$ARM <- new_arm

  return(data)
}

mod_data_preparation_server <- function(id, data) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    current_arms <- shiny::reactiveVal(list())

    shiny::onBookmark(function(state) {
      state$values$current_arms <- current_arms()
    })

    shiny::onRestore(function(state) {
      current_arms(state$values$current_arms)
    })

    shiny::setBookmarkExclude(names = BSAFE_ID$BUT_ADD_ARM)



    get_names <- function(name, length) {
      helper <- paste0(name, 1)
      for (i in 2:length) {
        helper <- c(helper, paste0(name, i))
      }
      return(helper)
    }

    # Initial menus

    shiny::observe({
      choices_helper <- names(data())
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
          choices = sort(unique(data()[[input[[BSAFE_ID$SEL_COLUMN]][i]]])),
          multiple = TRUE
        )
      })
    })

    output[["current_arms"]] <- shiny::renderUI({
      purrr::imap(
        current_arms(),
        function(args, arm_name) {
          shiny::div(
            style = "display: flex; align-items: baseline; column-gap: 10px",
            arm_name,
            shiny::tags$button(
              shiny::icon("delete-left"),
              type = "button",
              class = "btn btn-default",
              onClick = paste0(
                "Shiny.setInputValue('", ns("delete_arm"),
                "', '", arm_name, "', {priority: \"event\"})"
              )
            ),
          )
        }
      )
    })

    shiny::observeEvent(input[["delete_arm"]], {
      .current_arms <- current_arms()
      .current_arms <- .current_arms[!names(.current_arms) %in% input[["delete_arm"]]]
      current_arms(.current_arms)
    })

    shiny::observeEvent(input[[BSAFE_ID$BUT_ADD_ARM]], {
      new_name <- input[["new_arm_name"]]
      current_arm_names <- union(names(current_arms()), data()[["ARM"]])

      if (new_name %in% current_arm_names) {
        shiny::showNotification("Arm name already exists", type = "error")
        shiny::req(FALSE)
      }

      selector_cols <- input[[BSAFE_ID$SEL_COLUMN]]
      if (length(selector_cols) == 0) {
        shiny::showNotification("No columns selected")
        shiny::req(FALSE)
      }

      selector_ids <- paste0("SEL_", seq_along(selector_cols))
      selector_vals <- purrr::map(selector_ids, ~ input[[.x]])
      selector_vals <- stats::setNames(selector_vals, selector_cols)

      if (nrow(create_arm(data(), new_name, selector_vals)) == 0) {
        shiny::showNotification("Arm has no rows")
        shiny::req(FALSE)
      }

      new_el <- list()
      new_el[[new_name]] <- selector_vals
      current_arms(c(current_arms(), new_el))
    })

    shiny::outputOptions(output, BSAFE_ID$OUT_SEL_VAR, suspendWhenHidden = FALSE)

    data_with_arms <- shinymeta::metaReactive(
      {
        new_arms <- shinymeta::..(current_arms())

        if (length(new_arms) > 0) {
          d <- shinymeta::..(data())
          new_arms_df <- list()
          for (idx in seq_along(new_arms)) {
            new_name <- names(new_arms)[[idx]]
            new_sel_vals <- new_arms[[idx]]
            new_arms_df[[idx]] <- create_arm(d, new_name, new_sel_vals)
          }
          dplyr::bind_rows(d, new_arms_df)
        } else {
          shinymeta::..(data())
        }
      },
      varname = "data_with_arms"
    )

    output[[BSAFE_ID$OUT_ARM_SEL]] <- shiny::renderTable({
      data_with_arms()
    })


    return(data_with_arms)
  }

  shiny::moduleServer(id, mod)
}

mock_data_preparation_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      shiny::bookmarkButton(),
      mod_data_preparation_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out_data"),
      shiny::verbatimTextOutput("out_code")
    )
  }

  server <- function(input, output, session) {
    x <- mod_data_preparation_server(
      id = "mock",
      data = shiny::reactive(as.data.frame(teal.modules.bsafe::bsafe_data))
    )

    output[["out_data"]] <- shiny::renderPrint({
      x()
    })

    output[["out_code"]] <- shiny::renderPrint({
      shinymeta::expandChain(x())
    })
  }


  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}
