mod_simulation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(ns(BSAFE_ID$SLDR_NUM_COMP),
          "Number of comparisons",
          min = 1,
          max = 5,
          value = 3,
          step = 1
        ),
        shiny::uiOutput(ns(BSAFE_ID$OUT_COMP_CB)),
        shiny::actionButton(
          ns(BSAFE_ID$BUT_COMP_SUBMIT),
          "Submit"
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns(BSAFE_ID$OUT_DWNLD_PLTS)),
        shiny::br(),
        shiny::downloadButton(
          ns(BSAFE_ID$BUT_DWNLD_SUM_TBLS),
          "Download All AE Summary Tables"
        ),
        shiny::h5(""),
        shiny::downloadButton(
          ns(BSAFE_ID$BUT_DWNLD_EXCEL),
          "Download AE Summary Tables as Excel files"
        ),
        shiny::downloadButton(
          ns(BSAFE_ID$BUT_DWNLD_LOG),
          "Download log file"
        ),
        shiny::textOutput(ns(BSAFE_ID$OUT_EXCEL_PATH_TXT)),
        shiny::h5(""),
        shiny::h5("Simulating all tables might take a while."),
        shiny::h5("You have to press the submit button prior to download."),
        shiny::h5("The tables can be downloaded as soon as the chosen comparisons are displayed."),
        shiny::h5("Chosen comparisons:"),
        shiny::uiOutput(ns(BSAFE_ID$OUT_COMP_DISPLAY))
      )
    )
  )
}

mod_simulation_server <- function(id, data) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    output[[BSAFE_ID$OUT_COMP_CB]] <- shiny::renderUI({
      el <- list()
      for (i in seq_len(input[[BSAFE_ID$SLDR_NUM_COMP]])) {
        el[[i]] <- shiny::div(
          shiny::checkboxInput(ns(paste0("show_", i)), label = shiny::h4("Show comparison", i)),
          shiny::div(
            style = "border: solid gray; background: white; margin-bottom: 1rem; padding-left: 1rem",
            shiny::conditionalPanel(
              condition = paste0("input.show_", i),
              ns = ns,
              shiny::checkboxGroupInput(ns(paste0("download_boxes_trt_", i)),
                "Select the treatment arms",
                choices = unique(data()[["ARM"]])
              ),
              shiny::checkboxGroupInput(ns(paste0("download_boxes_ctrl_", i)),
                "Select the control arms",
                choices = unique(data()[["ARM"]])
              )
            )
          )
        )
      }

      el
    })

    shiny::outputOptions(output, BSAFE_ID$OUT_COMP_CB, suspendWhenHidden = FALSE)

    output[[BSAFE_ID$OUT_COMP_DISPLAY]] <- shiny::renderUI({
      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        shiny::verbatimTextOutput(ns(paste0("comparison_", i)))
      })
    })

    ae_summary_data <- NULL

    shiny::observeEvent(input[[BSAFE_ID$BUT_COMP_SUBMIT]], {
      DOWNLOAD_BOOL <- TRUE # nolint: object_name_linter

      for (i in seq_len(input[[BSAFE_ID$SLDR_NUM_COMP]])) {
        # checking whether at least one treatment AND control arm checkbox is ticked in the corresponding comparison
        if (length(input[[paste0("download_boxes_trt_", i)]]) == 0 |
          length(input[[paste0("download_boxes_ctrl_", i)]]) == 0) { # nolint: indentation_linter
          shiny::showNotification(
            paste0("You have to enter both a Treatment arm as well as a Control arm in Table ", i),
            type = "error"
          )
          return()
        }
      }


      # TODO: Remove assign usage
      for (i in seq_len(input[[BSAFE_ID$SLDR_NUM_COMP]])) {
        # assign values of the ticked checkboxes to the corresponding variables
        assign(
          paste0("selected_trt_boxes_", i),
          input[[paste0("download_boxes_trt_", i)]]
        )
        assign(
          paste0("selected_ctrl_boxes_", i),
          input[[paste0("download_boxes_ctrl_", i)]]
        )

        output[[paste0("comparison_", i)]] <- shiny::renderText(
          # pasting together the displayed comparisons from the corresponding checkbox variables
          # containing the values of the ticked boxes
          paste0(
            paste(unlist(get(paste0("selected_trt_boxes_", i))),
              collapse = ", "
            ),
            " vs. ",
            paste(unlist(get(paste0("selected_ctrl_boxes_", i))),
              collapse = ", "
            )
          )
        )
      }

      cb_list_trt <- vector(
        mode = "list",
        length = input[[BSAFE_ID$SLDR_NUM_COMP]]
      )
      names(cb_list_trt) <- paste0("grp", 1:input[[BSAFE_ID$SLDR_NUM_COMP]])
      cb_list_ctrl <- vector(
        mode = "list",
        length = input[[BSAFE_ID$SLDR_NUM_COMP]]
      )
      names(cb_list_ctrl) <- paste0("grp", 1:input[[BSAFE_ID$SLDR_NUM_COMP]])
      for (i in seq_len(input[[BSAFE_ID$SLDR_NUM_COMP]])) {
        cb_list_trt[[i]] <- input[[paste0("download_boxes_trt_", i)]]
        cb_list_ctrl[[i]] <- input[[paste0("download_boxes_ctrl_", i)]]
      }

      pgrs <- shiny::showNotification("Running simulations", duration = NULL)
      ae_summary_data <<- bsafe::ae_summary_table(
        data(),
        cb_list_ctrl,
        cb_list_trt,
        unique(data()[["SAF_TOPIC"]]),
        input[[BSAFE_ID$SET_SEED]]
      )

      shiny::removeNotification(pgrs)


      tmp_dir <- tempdir()
      # TODO: Asses usage of this part and how to improve this, otherwise we will polute the www directory

      # # create PDF-file from markdown document to show in popup window
      rmarkdown::render(
        input = system.file("template_ae_summary_table.Rmd",
          package = "teal.modules.bsafe",
          mustWork = TRUE
        ),
        # directory where the pdf file will be stored, works on the Docker container
        output_dir = tmp_dir,
        clean = TRUE,
        # parameters needed for markdown file
        params = list(
          ae_summary_Rmd = ae_summary_data(),
          date = format(Sys.time(), "%d %B, %Y"),
          bsafe_version = utils::packageVersion("teal.modules.bsafe"),
          pwemap_version = utils::packageVersion("bsafe"),
          seed = input[[BSAFE_ID$SET_SEED]]
        )
      )

      # TODO: Asses usage of this part and how to improve this, otherwise we will polute the www directory
      # open popup window with specified dimensions and display pdf file in iframe
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$head(
            shiny::tags$style(".modal-dialog{ min-width:1000px}")
          ),
          shiny::tags$head(shiny::tags$style(".modal-body{min-height:700px}")),
          shiny::tags$iframe(
            style = "height:700px; width:100%; scrolling=yes",
            src = "template_ae_summary_table.pdf"
          )
        )
      )
    })

    output[[BSAFE_ID$BUT_DWNLD_SUM_TBLS]] <- shiny::downloadHandler(
      filename = "ae_summary_table.pdf",
      content = function(file) {
        rmarkdown::render(
          input = system.file("template_ae_summary_table.Rmd",
            package = "teal.modules.bsafe",
            mustWork = TRUE
          ),
          output_file = file,
          clean = TRUE,
          params = list(
            ae_summary_Rmd = ae_summary_data(),
            date = format(Sys.time(), "%d %B, %Y"),
            bsafe_version = utils::packageVersion("teal.modules.bsafe"),
            pwemap_version = utils::packageVersion("bsafe"),
            seed = input[[BSAFE_ID$SET_SEED]]
          )
        )
      }
    )


    # logging -----------------------------------------------------------------
    # needs work TODO
    output[[BSAFE_ID$BUT_DWNLD_LOG]] <- shiny::downloadHandler(
      filename = "BSAFE_log.txt",
      content = function(file) {
        write(
          system2(
            command = "more",
            args = c("../../../../var/log/shiny-server/*.log"),
            stdout = TRUE
          ),
          file = file
        )
      }
    )



    return(
      list()
    )
  }

  shiny::moduleServer(id, mod)
}

mock_simulation_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_simulation_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    x <- mod_simulation_server(
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
