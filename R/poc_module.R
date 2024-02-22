# ui ----------------------------------------------------------------------

#' @title quic.bsafe's ui function
#' @description  implements the UI for the quic.bsafe shiny app
#'
#' @param id the id
#'
#' @return the UI
#' @export
poc_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Getting started",
        shiny::includeMarkdown(system.file("gettingStarted_bsafe.Rmd",
          package = "teal.modules.bsafe",
          mustWork = TRUE
        )),
        shiny::h5("User Manual:"),
        # shiny::a("open manual", href = "bsafe_manual.pdf")
      ),
      shiny::tabPanel(
        "Data preparation",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectInput(ns(BSAFE_ID$SEL_COLUMN),
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
          ),
          shiny::mainPanel(
            shiny::tableOutput(ns(BSAFE_ID$OUT_ARM_SEL))
          ),
        )
      ),
      shiny::tabPanel(
        "Select Analysis",
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
      ),
      shiny::tabPanel(
        "MAP Prior",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            teal.reporter::add_card_button_ui(ns(REPORT_IDS$MAP$ADD)),
            teal.reporter::download_report_button_ui(ns(REPORT_IDS$MAP$DOWNLOAD)),
            teal.reporter::reset_report_button_ui(ns(REPORT_IDS$MAP$RESET)),
            shiny::selectInput(ns(BSAFE_ID$SEL_TAU),
              "Between-Trial Heterogeneity Prior Distribution",
              choices = BSAFE_CHOICES$SEL_TAU,
              selected = BSAFE_DEFAULTS$SEL_TAU
            ),
            shiny::selectInput(
              inputId = ns(BSAFE_ID$SEL_HIST_BORROW),
              label = shiny::withMathJax(
                paste(
                  "\\(\\frac{\\tau}{\\sigma}\\)",
                  "controls the amount of historical borrowing",
                  "and is a ratio of the between-trial heterogeneity \\(\\tau\\)",
                  "and standard deviation \\(\\sigma\\):"
                )
              ),
              choices = BSAFE_CHOICES$SEL_HIST_BORROW,
              selected = BSAFE_DEFAULTS$SEL_HIST_BORROW
            ),
            shiny::selectInput(BSAFE_ID$SEL_ESS_METHOD,
              "Effective Sample Size Method",
              choices = BSAFE_CHOICES$SEL_ESS_METHOD,
              selected = BSAFE_DEFAULTS$SEL_ESS_METHOD
            ),
            shiny::actionButton(ns(BSAFE_ID$BUT_UPDATE_MAP), "Update")
          ),
          shiny::mainPanel(
            # shinyjs::hidden(
            #   shiny::div(
            #     id = BSAFE_ID$DIV_INCI,
            shiny::h2("Model Estimates"),
            shiny::h6("Displayed are the point estimates for the mean (dots) and their respective 95% frequentistic confidence intervals.
                      For a stratified (dashed light blue line) and meta (solid dark blue line) analysis.
                      The blue highlighted part displays the 95% credible interval (CrI) for the mean and the MAP Prior."),
            shiny::plotOutput(ns(BSAFE_ID$OUT_FOREST_PLT)),
            #   )
            # ),
            # shinyjs::hidden(
            #   shiny::div(
            #     id = BSAFE_ID$DIV_AE,
            shiny::h2("MAP Prior"),
            shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_PRIOR_TXT)),
            shiny::uiOutput(ns(BSAFE_ID$OUT_DENSITY_FCT)),
            shiny::plotOutput(ns(BSAFE_ID$OUT_MIX_DENSITY_PLT)), # spinner MAP prior distribution
            shiny::tableOutput(ns(BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL)) # MAP prior distribution summary table
            #   )
            # )
          )
        )
      ),
      shiny::tabPanel(
        "Robust MAP Prior",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            teal.reporter::add_card_button_ui(ns(REPORT_IDS$ROBUST_MAP$ADD)),
            teal.reporter::download_report_button_ui(ns(REPORT_IDS$ROBUST_MAP$DOWNLOAD)),
            teal.reporter::reset_report_button_ui(ns(REPORT_IDS$ROBUST_MAP$RESET)),
            shiny::sliderInput(ns(BSAFE_ID$SLDR_ROB_WEIGHT),
              "Weakly-informative Prior Weight (recommended to be between 0.1 and 0.5)",
              value = 0.2,
              min = 0.01,
              max = 0.99,
              step = 0.01
            ),
            shinyjs::hidden(
              shiny::div(
                id = ns(BSAFE_ID$DIV_ROB_MEAN),
                shiny::sliderInput(ns(BSAFE_ID$SLDR_ROB_MEAN),
                  "Weakly-informative Prior Mean on the exp scale",
                  value = 0.5, # default starting value
                  min = 0.01,
                  max = 3,
                  step = 0.01
                )
              )
            ),
            shiny::selectInput(ns(BSAFE_ID$SEL_ROB_ESS_METHOD),
              "Effective Sample Size Method",
              choices = BSAFE_CHOICES$SEL_ESS_METHOD,
              selected = BSAFE_DEFAULTS$SEL_ESS_METHOD
            ),
            shiny::actionButton(ns(BSAFE_ID$BUT_UPDATE_ROB), "Update")
          ),
          shiny::mainPanel(
            shiny::h2("Robust MAP Prior"),
            shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_ROB_TXT)),
            shiny::uiOutput(ns(BSAFE_ID$OUT_ROB_DENSITY_FCT)),
            shiny::plotOutput(ns(BSAFE_ID$OUT_ROB_MAP_PLT)), # spinner
            shiny::tableOutput(ns(BSAFE_ID$OUT_ROB_SUM_TBL))
          )
        )
      ),
      shiny::tabPanel(
        "New Trial Analysis",
        # shinyjs::hidden(
        shiny::div(
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              "Input data from the new trial",
              shiny::tags$hr(),
              shinyjs::hidden(shiny::div(
                id = ns(BSAFE_ID$DIV_NTA_INCI),
                shiny::sliderInput(ns(BSAFE_ID$SLDR_N_PAT),
                  "Number of Patients in selected Arm ",
                  min = 1,
                  max = 200,
                  value = 10,
                  step = 1
                ),
                shiny::sliderInput(ns(BSAFE_ID$SLDR_N_AE),
                  "Number of Patients with AE ",
                  min = 0,
                  max = 200,
                  value = 0,
                  step = 1
                )
              )),
              shinyjs::hidden(shiny::div(
                id = ns(BSAFE_ID$DIV_NTA_AE),
                shiny::sliderInput(ns(BSAFE_ID$SLDR_AE_FIRST_OCCURENCE),
                  "Number of first occurence of the event ",
                  min = 1,
                  max = 200,
                  value = 10,
                  step = 1
                ),
                shiny::sliderInput(ns(BSAFE_ID$SLDR_CUMM_TIME_FIRST_AE),
                  "Cummulative time to occurence of the first events",
                  min = 1,
                  max = 1000,
                  value = 200,
                  step = 1
                )
              )),
              shiny::actionButton(ns(BSAFE_ID$BUT_UPDATE_PRIOR), "Update"),
            ),
            shiny::mainPanel(
              shinyjs::hidden(shiny::div(
                id = ns(BSAFE_ID$DIV_NTA_INCI_MAIN),
                shiny::h2("Prior Data Conflict Assessment"),
                "To assess prior data conflict, compare the prior and posterior probability density function, and the likelihood of the observed data.",
              )),
              shinyjs::hidden(shiny::div(
                id = ns(BSAFE_ID$DIV_NTA_AE_MAIN),
                shiny::h2("Prior Data Conflict Assessment"),
                "To assess prior data conflict, compare the prior and posterior probability density function, and the log likelihood of the observed data on the log scale.",
              )),
              shiny::plotOutput(ns(BSAFE_ID$OUT_COMPARE_PLT)), # spinner %>% shinycssloaders::withSpinner(color = "#0dc5c1"), test
              shiny::tableOutput(ns(BSAFE_ID$OUT_COMPARE_SUM_TBL))
            )
          )
        )
      ),
      shiny::tabPanel(
        "Decision Making",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shinyjs::hidden(shiny::div(
              id = ns(BSAFE_ID$DIV_DM_INCI),
              shiny::selectInput(ns(BSAFE_ID$SEL_DIST),
                "Make statistical inference about the",
                choices = BSAFE_CHOICES$SEL_DIST,
                selected = BSAFE_DEFAULTS$SEL_DIST
              ),
              shiny::withMathJax("Adjust sliders for \\(P(LB_{AE} < p_{AE} < UB_{AE})\\),"),
              htmltools::HTML("<br/>"),
              shiny::uiOutput(ns(BSAFE_ID$OUT_PERC_SLDR)),
            )),
            shinyjs::hidden(
              shiny::div(
                id = ns(BSAFE_ID$DIV_DM_AE),
                shiny::selectInput(ns(BSAFE_ID$SEL_DIST_AE),
                  "Make statistical inference about the",
                  choices = BSAFE_CHOICES$SEL_DIST,
                  selected = BSAFE_DEFAULTS$SEL_DIST
                ),
                shiny::withMathJax("Adjust sliders for \\(P(LB_{AE} < p_{AE} < UB_{AE})\\),"),
                htmltools::HTML("<br/>"),
                shiny::uiOutput(ns(BSAFE_ID$OUT_AE_PERC_SLDR))
              )
            ),
            shiny::actionButton(ns(BSAFE_ID$BUT_UPDATE_STAT_INF), "Update")
          ),
          shiny::mainPanel(
            shiny::uiOutput(ns(BSAFE_ID$OUT_DM_HEADER_TXT)),
            shiny::uiOutput(ns(BSAFE_ID$OUT_DM_PREFACE_TXT)),
            shiny::plotOutput(ns(BSAFE_ID$OUT_STAT_INF_DENSITY_PLT)), # spinner
            shiny::uiOutput(ns(BSAFE_ID$OUT_AREA_UNDER_CURVE)),
            shiny::h2("Inference"),
            shiny::tableOutput(ns(BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL))
          )
        ),
      ),
      shiny::tabPanel(
        "Download Results",
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
    )
  )
}

# server ------------------------------------------------------------------


poc_server <- function(
    id,
    dataset, # Must be reactive
    dataset_tdata,
    dataset_name, # Ignore for now
    reporter,
    filter_panel_api) {
  module <- function(input, output, session) {
    # global variables --------------------------------------------------------


    ns <- session[["ns"]]

    ae_summary_data <- NULL

    # reactive Values Object
    rv <- shiny::reactiveValues(arm_list = list(), data = NULL)

    receive_data_qenv <- shiny::reactive({
      message("receive")
      teal.code::new_qenv(
        env = teal::tdata2env(dataset_tdata),
        code = teal::get_code_tdata(dataset_tdata)
      )
    })
    # data input/checks/transformation ----------------------------------------
    receive_data <- shiny::reactive({
      dataset()
    })

    full_join_data <- function() {
      rv[["data"]] <- dplyr::full_join(rv[["data"]], receive_data())
    }

    trt_data_wrangler <- function(input_data) {
      choices_trt <- unique(input_data[, "ARM"])
      return(choices_trt)
    }

    ae_events_wrangler <- function(input_data, selected_trt) {
      safety_topics <- as.character(unlist(input_data[, "SAF_TOPIC"]))
      choices_ae <- safety_topics[as.character(unlist(input_data[, "ARM"])) == selected_trt]
      return(choices_ae)
    }

    # # needs rework TODO
    # # Read input data
    # input_data <- shiny::reactive({
    #   # shiny::req(input[[BSAFE_ID$FILE_IN]])
    #   # data <- data_reader(filename = input[[BSAFE_ID$FILE_IN]])
    #   # shiny::validate(
    #   #  shiny::need(
    #   #    is.character(unlist(data[, "STUDYID"])),
    #   #    "STUDYID needs to be a character"),
    #   #  shiny::need(
    #   #    is.numeric(unlist(data[, "N"])),
    #   #    "N needs to be a number"),
    #   #  shiny::need(
    #   #    is.numeric(unlist(data[, "N_WITH_AE"])),
    #   #    "N_with_AE needs to be a number"),
    #   #  shiny::need(
    #   #    is.numeric(unlist(data[, "TOT_EXP"])),
    #   #    "TOT_EXP needs to be a number"),
    #   #  shiny::need(
    #   #    is.character(unlist(data[, "SAF_TOPIC"])),
    #   #    "SAF_TOPIC needs to be a character"),
    #   #  shiny::need(
    #   #    is.character(unlist(data[, "ARM"])),
    #   #    "ARM needs to be a character"),
    #   #  shiny::need(
    #   #    is.numeric(unlist(data[, "HIST"])),
    #   #    "HIST needs to be a number")
    #   # )
    #   # data <- data_saf_topic_char_limiter(data)
    #   # data
    #   dataset
    # })

    # input_data <- shiny::reactive




    # ui element updates ------------------------------------------------------


    # slider input dependent on the new_n slider input in the New Trial Tab
    # Update label of Number of Patients with AE
    shiny::observe({
      shiny::updateSliderInput(session,
        BSAFE_ID$SLDR_N_AE,
        label = paste0(
          "Number of Patients with AE ",
          input[[BSAFE_ID$SEL_SAF_TOPIC]]
        ),
        min = 0,
        max = input[[BSAFE_ID$SLDR_N_PAT]],
        value = 0,
        step = 1
      )
    })

    output[[BSAFE_ID$OUT_COMP_CB]] <- shiny::renderUI({
      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        shiny::p(
          shiny::a(
            id = ns(paste0("compare_", i)),
            paste0("show/hide Comparison ", i), href = "#"
          ),
          shinyjs::hidden(
            shiny::div(
              id = ns(paste0("table_", i)),
              shiny::checkboxGroupInput(ns(paste0("download_boxes_trt_", i)),
                "Select the treatment arms",
                choices = unique(receive_data()[["ARM"]])
              ),
              shiny::checkboxGroupInput(ns(paste0("download_boxes_ctrl_", i)),
                "Select the control arms",
                choices = unique(receive_data()[["ARM"]])
              )
            )
          )
        )
      })
    })
    output[[BSAFE_ID$OUT_COMP_DISPLAY]] <- shiny::renderUI({
      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        shiny::verbatimTextOutput(ns(paste0("comparison_", i)))
      })
    })

    output[[BSAFE_ID$OUT_PERC_SLDR]] <- shiny::renderUI({
      shiny::sliderInput(
        ns(BSAFE_ID$OUT_PERC_SLDR),
        shiny::withMathJax(
          paste0(
            "where \\(p_{AE}\\) = percentage of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]]
          )
        ),
        min = 0,
        max = 100,
        value = c(30, 100), # TODO define this
        post = "%"
      )
    })

    output[[BSAFE_ID$OUT_AE_PERC_SLDR]] <- shiny::renderUI({
      val <- calc_log_hazard_area(param_approx = param_approx())
      shiny::sliderInput(
        ns(BSAFE_ID$OUT_AE_PERC_SLDR),
        shiny::withMathJax(
          paste0(
            "where \\(p_{AE}\\) = area of log(hazard) for patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]]
          )
        ),
        min = min(val),
        max = max(val),
        value = calc_param_approx_boundaries(param_approx())
      )
    })


    output[[BSAFE_ID$OUT_TXT_SELECTED_ANALYSIS]] <- shiny::renderText({
      input[[BSAFE_ID$SEL_ANALYSIS]]
    })


    shiny::observe({
      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_TRT,
        choices = trt_data_wrangler(receive_data())
      )
    })

    shiny::observe({
      choices_helper <- main_sel_arm_creation_update(
        data = receive_data(),
        col_names_to_be_excluded = c("STUDYID", "DOSE", "FREQ", "LENGTH", "TREAT")
      )
      shiny::updateSelectInput(session,
        inputId = BSAFE_ID$SEL_COLUMN,
        choices = choices_helper
      )
    })

    output[[BSAFE_ID$OUT_SEL_VAR]] <- shiny::renderUI({
      shiny::req(input[[BSAFE_ID$SEL_COLUMN]])
      lapply(1:length(input[[BSAFE_ID$SEL_COLUMN]]), function(i) {
        shiny::selectInput(ns(paste0("SEL_", i)),
          label = paste0(
            "Select the forms of ",
            input[[BSAFE_ID$SEL_COLUMN]][i]
          ),
          choices = levels(
            factor(
              unique(
                receive_data()[, input[[BSAFE_ID$SEL_COLUMN]][i]]
              )
            )
          ),
          multiple = TRUE
        )
      })
    })


    shiny::observeEvent(input[[BSAFE_ID$BUT_ADD_ARM]], {
      shiny::showModal(shiny::modalDialog(
        title = "Name the arm you just created",
        shiny::textInput("MODAL_INPUT", "Name", ""),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("MODAL_ARM_CREATION"), "OK")
        )
      ))
    })

    output[[BSAFE_ID$OUT_ARM_SEL]] <- shiny::renderTable({
      receive_data()
    })

    shiny::observeEvent(input[[BSAFE_ID$SET_SEED]], {
      shiny::updateNumericInput(session,
        inputId = BSAFE_ID$SET_SEED,
        value = input[[BSAFE_ID$SET_SEED]]
      )
    })

    selected_arm <- shiny::eventReactive(input[[BSAFE_ID$SEL_TRT]], {
      selected_arm <- input[[BSAFE_ID$SEL_TRT]]
    })

    shiny::observe({
      shiny::updateSelectInput(
        session,
        inputId = BSAFE_ID$SEL_SAF_TOPIC,
        choices = ae_events_wrangler(receive_data(), selected_arm())
      )
    })


    # Display input data
    output[[BSAFE_ID$OUT_FILE_TABLE]] <- function() {
      bsafe::input_data_display(
        data = my_data(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]]
      )
    }

    # Data table preparation
    my_data <- shiny::reactive({
      bsafe::data_table_prep(
        input_data = receive_data(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]],
        select_btrt = input[[BSAFE_ID$SEL_TRT]],
        bool_pooled = input[[BSAFE_ID$CB_POOLED]]
      )
    })

    my_data_qenv <- shiny::reactive({
      x <- rlang::expr(
        d <- bsafe::data_table_prep(
          input_data = bsafe_data,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          saf_topic = !!input[[BSAFE_ID$SEL_SAF_TOPIC]],
          select_btrt = !!input[[BSAFE_ID$SEL_TRT]],
          bool_pooled = !!input[[BSAFE_ID$CB_POOLED]]
        )
      )

      teal.code::eval_code(
        receive_data_qenv(),
        x
      )
    })


    # Historical Borrowing
    # sigma set to 2, so adjust tau to change amount of historical borrowing
    # Formula: tau/2
    adj_tau <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      bsafe::tau_adjust(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        hist_borrow = input[[BSAFE_ID$SEL_HIST_BORROW]]
      )
    })

    adj_tau_qenv <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      x <- rlang::expr(
        adj_tau <- bsafe::tau_adjust(
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          hist_borrow = !!input[[BSAFE_ID$SEL_HIST_BORROW]]
        )
      )

      teal.code::eval_code(
        teal.code::new_qenv(),
        x
      )
    })

    # MAP object
    map_mcmc <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      shiny::req(input[[BSAFE_ID$SET_SEED]])
      bsafe::map_prior_func(
        input_data = my_data(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        tau_dist = input[[BSAFE_ID$SEL_TAU]],
        adj_tau = adj_tau(),
        seed = input[[BSAFE_ID$SET_SEED]]
      )
    })

    # Robust MAP prior
    robust_map_mcmc <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_ROB]], {
      # rob weight in function and return that

      shiny::req(input[[BSAFE_ID$SEL_ANALYSIS]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])

      bsafe::robust_map(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        param_approx = param_approx(),
        input_data = my_data(),
        robust_weight = input[[BSAFE_ID$SLDR_ROB_WEIGHT]],
        robust_mean = input[[BSAFE_ID$SLDR_ROB_MEAN]],
        adj_tau = adj_tau(),
        seed = input[[BSAFE_ID$SET_SEED]]
      )
    })

    robust_map_mcmc_qenv <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_ROB]], {
      # rob weight in function and return that
      shiny::req(input[[BSAFE_ID$SEL_ANALYSIS]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])

      x <- rlang::expr(
        robust_map_mcmc <- bsafe::robust_map(
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          param_approx = param_approx,
          input_data = my_data_qenv()[["d"]],
          robust_weight = !!input[[BSAFE_ID$SLDR_ROB_WEIGHT]],
          robust_mean = !!input[[BSAFE_ID$SLDR_ROB_MEAN]],
          adj_tau = adj_tau,
          seed = !!input[[BSAFE_ID$SET_SEED]]
        )
      )

      teal.code::eval_code(
        teal.code::join(my_data_qenv(), adj_tau_qenv()) |> teal.code::join(param_approx_qenv()),
        x
      )
    })

    # Data from current trial
    current_trial_data <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_PRIOR]], {
      shiny::req(input[[BSAFE_ID$SEL_ANALYSIS]])
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        list(
          new_v1 = input[[BSAFE_ID$SLDR_N_PAT]],
          new_v2 = input[[BSAFE_ID$SLDR_N_AE]]
        )
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        list(
          new_v1 = input[[BSAFE_ID$SLDR_AE_FIRST_OCCURENCE]],
          new_v2 = input[[BSAFE_ID$SLDR_CUMM_TIME_FIRST_AE]]
        )
      }
    })


    # Posterior distribution
    post_dist <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_PRIOR]], {
      bsafe::posterior_dist(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        input_data = my_data(),
        robust_map_prior = robust_map_mcmc(),
        explore = TRUE,
        new_v1 = current_trial_data()[["new_v1"]],
        new_v2 = current_trial_data()[["new_v2"]],
        seed = input[[BSAFE_ID$SET_SEED]]
      )
    })

    # Compare robust MAP prior to MAP prior
    rob_comp <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_ROB]], {
      bsafe::robust_compare(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        robust_map_prior = robust_map_mcmc(),
        param_approx = param_approx()
      )
    })

    rob_comp_qenv <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_ROB]], {
      x <- rlang::expr(
        rob_comp <- bsafe::robust_compare(
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          robust_map_prior = robust_map_mcmc,
          param_approx = param_approx
        )
      )

      teal.code::eval_code(
        teal.code::join(robust_map_mcmc_qenv(), param_approx_qenv()),
        x
      )
    })

    # New trial analysis
    new_trial_analysis <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_PRIOR]], {
      bsafe::new_trial_compare(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        robust_map_prior = robust_map_mcmc(),
        new_v1 = current_trial_data()[["new_v1"]],
        new_v2 = current_trial_data()[["new_v2"]],
        post_dist = post_dist()
      )
    })

    # Mixture distributions for MAP Prior, Robust MAP Prior, Likelihood, or Posterior Distribution
    mix <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_STAT_INF]], {
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shiny::req(input[[BSAFE_ID$SEL_DIST]])
        bsafe::mix_distribution_all(
          current_trial_data = current_trial_data(),
          select_dist = input[[BSAFE_ID$SEL_DIST]],
          select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
          param_approx = param_approx(),
          robust_map_object = robust_map_mcmc(),
          post_dist = post_dist()
        )
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shiny::req(input[[BSAFE_ID$SEL_DIST_AE]])
        bsafe::mix_distribution_all(
          current_trial_data = current_trial_data(),
          select_dist = input[[BSAFE_ID$SEL_DIST_AE]],
          select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
          param_approx = param_approx(),
          robust_map_object = robust_map_mcmc(),
          post_dist = post_dist()
        )
      }
    })


    # MAP PRIOR ----

    map_mcmc_qenv <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      shiny::req(input[[BSAFE_ID$SET_SEED]])
      message("map_mcmc_qenv")

      x <- rlang::expr(
        map_mcmc <- bsafe::map_prior_func(
          input_data = d,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          tau_dist = !!input[[BSAFE_ID$SEL_TAU]],
          adj_tau = bsafe::tau_adjust(
            select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
            hist_borrow = !!input[[BSAFE_ID$SEL_HIST_BORROW]]
          ),
          seed = !!input[[BSAFE_ID$SET_SEED]]
        )
      )

      teal.code::eval_code(
        teal.code::join(my_data_qenv(), adj_tau_qenv()),
        x
      )
    })

    forest_plot_qenv <- shiny::reactive({
      message("forest")
      x <- rlang::expr(
        forest_plot <- bsafe::forest_plot_display(
          map_object = map_mcmc,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          saf_topic = !!input[[BSAFE_ID$SEL_SAF_TOPIC]],
          select_btrt = !!input[[BSAFE_ID$SEL_TRT]]
        )
      )

      teal.code::eval_code(
        map_mcmc_qenv(),
        x
      )
    })

    # Parametric approximation object
    param_approx <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      bsafe::parametric_approx(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        map_prior = map_mcmc()
      )
    })

    param_approx_qenv <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_MAP]], {
      message("param_approx")
      x <- rlang::expr(
        param_approx <- bsafe::parametric_approx(
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          map_prior = map_mcmc
        )
      )

      teal.code::eval_code(
        map_mcmc_qenv(),
        x
      )
    })

    map_mix_density_qenv <- shiny::reactive({
      message("param_approx")
      x <- rlang::expr(
        mix_density_plot <- bsafe::param_mix_density_display(
          param_approx = param_approx,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          saf_topic = !!input[[BSAFE_ID$SEL_SAF_TOPIC]],
          select_btrt = !!input[[BSAFE_ID$SEL_TRT]]
        )
      )

      teal.code::eval_code(
        param_approx_qenv(),
        x
      )
    })

    map_summary_table_qenv <- shiny::reactive({
      message("param_approx")
      x <- rlang::expr(
        summary_table <- bsafe::model_summary_display(
          map_object = map_mcmc,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          param_approx = param_approx,
          ess_method = !!input[[BSAFE_ID$SEL_ESS_METHOD]]
        )
      )

      teal.code::eval_code(
        param_approx_qenv(),
        x
      )
    })

    # Display forest plot
    output[[BSAFE_ID$OUT_FOREST_PLT]] <- shiny::renderPlot({
      forest_plot_qenv()[["forest_plot"]]
    })

    # Preface the MAP prior distribution
    output[[BSAFE_ID$OUT_PREFACE_PRIOR_TXT]] <- shiny::renderUI({
      shiny::h6(preface_prior_txt(input[[BSAFE_ID$SEL_ANALYSIS]]))
    })

    # Display parametric approximation mixture density function
    # TODO: Cannot be included in the reporter MATHJAX not supported
    output[[BSAFE_ID$OUT_DENSITY_FCT]] <- shiny::renderUI({
      bsafe::map_prior_function_display(
        param_approx = param_approx(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]]
      )
    })

    # Display parametric mixture density
    output[[BSAFE_ID$OUT_MIX_DENSITY_PLT]] <- shiny::renderPlot({
      map_mix_density_qenv()[["mix_density_plot"]]
    })

    # Display model summary output
    output[[BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL]] <- function() {
      ({
        map_summary_table_qenv()[["summary_table"]] %>%
          knitr::kable("html") %>%
          kableExtra::kable_styling("striped")
      })
    }

    ### REPORTER
    map_card_fun <- function(card = teal.reporter::ReportCard$new(), comment) {
      card$set_name("Forest Plot")
      # card$append_text(filter_panel_api$get_filter_state(), "verbatim")
      card$append_text(paste(teal.code::get_code(forest_plot_qenv()), collapse = "\n"), "verbatim")
      card$append_text(preface_prior_txt(input[[BSAFE_ID$SEL_ANALYSIS]]))
      card$append_plot(forest_plot_qenv()[["forest_plot"]])
      card$append_text(paste(teal.code::get_code(map_mix_density_qenv()), collapse = "\n"), "verbatim")
      card$append_text("CANNOT INCLUDE DENSITY FUNCTION MATHJAX IS NOT SUPPORTED BY TEAL REPORTER")
      card$append_plot(map_mix_density_qenv()[["mix_density_plot"]])
      card$append_text(paste(teal.code::get_code(map_summary_table_qenv()), collapse = "\n"), "verbatim")
      card$append_table(map_summary_table_qenv()[["summary_table"]])
    }

    teal.reporter::add_card_button_srv(REPORT_IDS$MAP$ADD, reporter = reporter, card_fun = map_card_fun)
    teal.reporter::download_report_button_srv(REPORT_IDS$MAP$DOWNLOAD, reporter = reporter)
    teal.reporter::reset_report_button_srv(REPORT_IDS$MAP$RESET, reporter)
    ###

    # ROBUST MAP PRIOR ----

    # Preface robust MAP prior output
    # UNREPORTABLE
    output[[BSAFE_ID$OUT_PREFACE_ROB_TXT]] <- shiny::renderUI({
      shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])
      shiny::withMathJax(
        shiny::h6(
          preface_rob_txt(
            sel_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
            rob_weight = input[[BSAFE_ID$SLDR_ROB_WEIGHT]],
            rob_mean = input[[BSAFE_ID$SLDR_ROB_MEAN]]
          )
        )
      )
    })

    # Display robust MAP prior mixture density function
    # UNREPORTABLE
    output[[BSAFE_ID$OUT_ROB_DENSITY_FCT]] <- shiny::renderUI({
      bsafe::robust_map_prior_mix_dens_display(
        robust_map_object = robust_map_mcmc(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]]
      )
    })

    robust_map_plot_qenv <- shiny::reactive({
      x <- rlang::expr(
        robust_map_plot <- bsafe::robust_map_prior_plot(
          rob_comp = rob_comp,
          saf_topic = !!input[[BSAFE_ID$SEL_SAF_TOPIC]],
          select_btrt = !!input[[BSAFE_ID$SEL_TRT]],
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]]
        )
      )

      teal.code::eval_code(
        rob_comp_qenv(),
        x
      )
    })

    # Compare robust MAP prior to MAP prior
    output[[BSAFE_ID$OUT_ROB_MAP_PLT]] <- shiny::renderPlot({
      robust_map_plot_qenv()[["robust_map_plot"]]
    })

    robust_map_sum_tbl_qenv <- shiny::reactive({
      shiny::req(robust_map_mcmc_qenv()[["robust_map_mcmc"]])
      x <- rlang::expr(
        robust_map_sum_tbl <- bsafe::summary_stats_robust_map_prior_display(
          map_object = map_mcmc,
          select_analysis = !!input[[BSAFE_ID$SEL_ANALYSIS]],
          param_approx = param_approx,
          ess_method = !!input[[BSAFE_ID$SEL_ESS_METHOD]],
          robust_map_object = robust_map_mcmc,
          rob_ess_method = !!input[[BSAFE_ID$SEL_ROB_ESS_METHOD]],
          download = FALSE
        )
      )

      teal.code::eval_code(
        teal.code::join(map_mcmc_qenv(), param_approx_qenv()) |> teal.code::join(robust_map_mcmc_qenv()),
        x
      )
    })

    # Display summary stats of robust MAP prior and MAP prior
    output[[BSAFE_ID$OUT_ROB_SUM_TBL]] <- function() {
      robust_map_sum_tbl_qenv()[["robust_map_sum_tbl"]] %>%
        knitr::kable("html") %>%
        kableExtra::kable_styling("striped")
    }

    ### REPORTER
    robust_map_card_fun <- function(card = teal.reporter::ReportCard$new(), comment) {
      card$set_name("Robust Prior Map")
      # card$append_text(filter_panel_api$get_filter_state(), "verbatim")
      card$append_text("CANNOT INCLUDE FUNCTION MATHJAX IS NOT SUPPORTED BY TEAL REPORTER")
      card$append_text(paste(teal.code::get_code(robust_map_plot_qenv()), collapse = "\n"), "verbatim")
      card$append_plot(robust_map_plot_qenv()[["robust_map_plot"]])
      card$append_text(paste(teal.code::get_code(robust_map_sum_tbl_qenv()), collapse = "\n"), "verbatim")
      card$append_table(robust_map_sum_tbl_qenv()[["robust_map_sum_tbl"]])
    }

    teal.reporter::add_card_button_srv(REPORT_IDS$ROBUST_MAP$ADD, reporter = reporter, card_fun = robust_map_card_fun)
    teal.reporter::download_report_button_srv(REPORT_IDS$ROBUST_MAP$DOWNLOAD, reporter = reporter)
    teal.reporter::reset_report_button_srv(REPORT_IDS$ROBUST_MAP$RESET, reporter)
    ###

    # NEW TRIAL ANALYSIS ----

    # Prior data conflict assessment - compare prior, likelihood, and posterior
    output[[BSAFE_ID$OUT_COMPARE_PLT]] <- shiny::renderPlot({
      shiny::req(new_trial_analysis())
      bsafe::nta_data_conflict_assassment_plot(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        new_trial_analysis = new_trial_analysis(),
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]],
        select_btrt = input[[BSAFE_ID$SEL_TRT]]
      )
    })

    # Summary statistics for prior, likelihood, and posterior
    output[[BSAFE_ID$OUT_COMPARE_SUM_TBL]] <- function() {
      bsafe::summary_stat_all_display(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        robust_map_object = robust_map_mcmc(),
        ess_method = input[[BSAFE_ID$SEL_ESS_METHOD]],
        current_trial_data = current_trial_data(),
        post_dist = post_dist(),
        download = FALSE
      )
    }

    # MAP Prior, Robust MAP Prior, Likelihood, or Posterior Distribution Samples
    stat_inf_dist <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_STAT_INF]], {
      select_dist <- select_dist_selector(
        sel_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        sel_dist = input[[BSAFE_ID$SEL_DIST]],
        sel_dist_ae = input[[BSAFE_ID$SEL_DIST_AE]]
      )
      bsafe::sampling_all_plot(
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        select_dist = select_dist,
        param_approx = param_approx(),
        new_trial_analysis = new_trial_analysis()
      )
    })

    # Slider input for proportion of adverse event (quantiles of distribution)
    ae_prop <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_STAT_INF]], {
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        input[[BSAFE_ID$OUT_PERC_SLDR]] / 100
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        input[[BSAFE_ID$OUT_AE_PERC_SLDR]] / 100
      }
    })
    # DECISION MAKING ----
    # Header text
    output[[BSAFE_ID$OUT_DM_HEADER_TXT]] <- shiny::renderUI({
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shiny::h2(input[[BSAFE_ID$SEL_DIST]])
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shiny::h2(input[[BSAFE_ID$SEL_DIST_AE]])
      }
    })

    # Preface text for each distribution
    output[[BSAFE_ID$OUT_DM_PREFACE_TXT]] <- shiny::renderUI({
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        switch(input[[BSAFE_ID$SEL_DIST]],
          "Likelihood" = paste0(
            "The likelihood represents information about the proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial that is contained in the observed data."
          ),
          "MAP Prior" = paste0(""),
          "Robust MAP Prior" = paste0(
            "The robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial if they were to receive placebo."
          ),
          "Posterior" = paste0(
            "The posterior distribution represents information about the proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial after combining the prior (historical data) and the likelihood (new trial)."
          )
        )
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        switch(input[[BSAFE_ID$SEL_DIST]],
          "Likelihood" = paste0(
            "The log scale of the likelihood represents information about the proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial that is contained in the observed data."
          ),
          "MAP Prior" = paste0(""),
          "Robust MAP Prior" = paste0(
            "The log scale of the robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial if they were to receive placebo."
          ),
          "Posterior" = paste0(
            "The log scale of the posterior distribution represents information about the proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial after combining the prior (historical data) and the likelihood (new trial)."
          )
        )
      }
    })

    # Plot density
    output[[BSAFE_ID$OUT_STAT_INF_DENSITY_PLT]] <- shiny::renderPlot({
      bsafe::decision_making_density_plot(
        stat_inf_dist = stat_inf_dist(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        ae_prop = ae_prop(),
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]],
        select_btrt = input[[BSAFE_ID$SEL_TRT]]
      )
    })

    # Interpret area under the curve
    output[[BSAFE_ID$OUT_AREA_UNDER_CURVE]] <- shiny::renderUI({
      bsafe::area_under_the_curve(
        ae_prop = ae_prop(),
        mix = mix(),
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]]
      )
    })

    # Table of preset statistical inference statements
    output[[BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL]] <- function() {
      bsafe::preset_stat_table(
        mix = mix(),
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]],
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]]
      )
    }

    # REST OF MODULE ----

    # shinyjs -----------------------------------------------------------------

    # needs rework TODO
    shiny::observeEvent(input[[BSAFE_ID$SEL_ANALYSIS]], {
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::show(BSAFE_ID$DIV_INCI)
        shinyjs::show(BSAFE_ID$DIV_AE)
        shinyjs::hide(BSAFE_ID$DIV_ROB_MEAN)
        shinyjs::show(BSAFE_ID$DIV_NTA_INCI)
        shinyjs::show(BSAFE_ID$DIV_NTA_INCI_MAIN)
        shinyjs::show(BSAFE_ID$DIV_DM_INCI)
        shinyjs::hide(BSAFE_ID$DIV_NTA_AE)
        shinyjs::hide(BSAFE_ID$DIV_NTA_AE_MAIN)
        shinyjs::hide(BSAFE_ID$DIV_DM_AE)
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::hide(BSAFE_ID$DIV_INCI)
        shinyjs::show(BSAFE_ID$DIV_AE)
        shinyjs::show(BSAFE_ID$DIV_ROB_MEAN)
        shinyjs::hide(BSAFE_ID$DIV_NTA_INCI)
        shinyjs::hide(BSAFE_ID$DIV_NTA_INCI_MAIN)
        shinyjs::hide(BSAFE_ID$DIV_DM_INCI)
        shinyjs::show(BSAFE_ID$DIV_NTA_AE)
        shinyjs::show(BSAFE_ID$DIV_NTA_AE_MAIN)
        shinyjs::show(BSAFE_ID$DIV_DM_AE)
      }
    })

    shiny::observe({
      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        shinyjs::onclick(
          paste0("compare_", i),
          shinyjs::toggle(id = paste0("table_", i), anim = TRUE)
        )
      })
    })





    # calculations/functions --------------------------------------------------




    getNames <- function(name, length) {
      helper <- paste0(name, 1)
      for (i in 2:length) {
        helper <- c(helper, paste0(name, i))
      }
      return(helper)
    }

    # column_names = input[[BSAFE_ID$SEL_COLUMN]]
    down_filtering <- function(data, column_names) {
      # name <- input$MODAL_INPUT
      for (i in 1:length(column_names)) {
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
      selectors <- getNames("SEL_", length(input[[BSAFE_ID$SEL_COLUMN]]))
      param_list <- purrr::map(selectors, function(x) {
        return(input[[x]])
      })
      names(param_list) <- input[[BSAFE_ID$SEL_COLUMN]]
      if (!param_list %in% rv$arm_list) {
        rv$arm_list[[name]] <- param_list
        print(rv$arm_list)
      } else {
        warning("The selected arm is already available, please select a different one")
      }
      filtered_data <- down_filtering(receive_data(), input[[BSAFE_ID$SEL_COLUMN]])
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



    # report generation/simulation --------------------------------------------

    shiny::observeEvent(input[[BSAFE_ID$BUT_COMP_SUBMIT]], {
      DOWNLOAD_BOOL <- TRUE

      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        # checking whether at least one treatment AND control arm checkbox is ticked in the corresponding comparison
        if (length(input[[paste0("download_boxes_trt_", i)]]) == 0 |
          length(input[[paste0("download_boxes_ctrl_", i)]]) == 0) {
          shinyjs::alert(
            paste0(
              "You have to enter both a Treatment arm as well as a Control arm in Table ",
              i
            )
          )
        }
      })
      for (i in 1:input[[BSAFE_ID$SLDR_NUM_COMP]]) {
        # disabling download functionality if not both treatment AND control arm checkboxes at least contain one ticked box each in the corresponding comparison
        if (length(input[[paste0("download_boxes_trt_", i)]]) == 0 |
          length(input[[paste0("download_boxes_ctrl_", i)]]) == 0) {
          DOWNLOAD_BOOL <- FALSE
        }
      }
      # needs rework TODO
      if (DOWNLOAD_BOOL) {
        lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
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
            # pasting together the displayed comparisons from the corresponding checkbox variables containing the values of the ticked boxes
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
        })
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
        for (i in 1:input[[BSAFE_ID$SLDR_NUM_COMP]]) {
          cb_list_trt[[i]] <- input[[paste0("download_boxes_trt_", i)]]
          cb_list_ctrl[[i]] <- input[[paste0("download_boxes_ctrl_", i)]]
        }
        shiny::withProgress(
          message = "Running simulations for the download",
          value = 0,
          {
            shiny::incProgress(1 / 10)
            Sys.sleep(1)
            shiny::incProgress(5 / 10)
            # reactive outsourcing
            ae_summary_data <<- bsafe::ae_summary_table(
              receive_data(),
              cb_list_ctrl,
              cb_list_trt,
              unique(receive_data()[["SAF_TOPIC"]]),
              input[[BSAFE_ID$SET_SEED]]
            )
          }
        )
      }

      # create PDF-file from markdown document to show in popup window
      rmarkdown::render(
        input = system.file("template_ae_summary_table.Rmd",
          package = "teal.modules.bsafe",
          mustWork = TRUE
        ),
        # directory where the pdf file will be stored, works on the Docker container
        output_dir = "./www/",
        clean = TRUE,
        # parameters needed for markdown file
        params = list(
          ae_summary_Rmd = ae_summary_data,
          date = format(Sys.time(), "%d %B, %Y"),
          bsafe_version = utils::packageVersion("teal.modules.bsafe"),
          pwemap_version = utils::packageVersion("bsafe"),
          seed = input[[BSAFE_ID$SET_SEED]]
        )
      )
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
        ae_summary_data

        rmarkdown::render(
          input = system.file("template_ae_summary_table.Rmd",
            package = "teal.modules.bsafe",
            mustWork = TRUE
          ),
          output_file = file,
          clean = TRUE,
          params = list(
            ae_summary_Rmd = ae_summary_data,
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
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}
