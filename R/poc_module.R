# ui ----------------------------------------------------------------------

#' @title quic.bsafe's ui function
#' @description  implements the UI for the quic.bsafe shiny app
#'
#' @param id the id
#' @param header elements to be included in the header of the tabset of the module
#'
#' @return the UI
#' @export
poc_UI <- function(id, header = NULL) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tabsetPanel(
      id = ns("tab_panel"),
      header = header,
      shiny::tabPanel(
        "Getting started",
        shiny::includeMarkdown(system.file("gettingStarted_bsafe.Rmd",
          package = "teal.modules.bsafe",
          mustWork = TRUE
        )),
        shiny::h5("User Manual:"),
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
        mod_select_analysis_ui(ns("sel_analysis"))
      ),
      shiny::tabPanel(
        "MAP Prior",
        mod_map_prior_ui(ns("map_prior"))
      ),
      shiny::tabPanel(
        "Robust MAP Prior",
        mod_robust_map_ui(ns("robust_map"))
      ),
      shiny::tabPanel(
        "New Trial Analysis",
        mod_new_trial_analysis_UI(ns("new_trial"))
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
    reporter,
    filter_panel_api) {
  module <- function(input, output, session) {
    # global variables --------------------------------------------------------


    ns <- session[["ns"]]

    ae_summary_data <- NULL

    # reactive Values Object
    rv <- shiny::reactiveValues(arm_list = list(), data = NULL)

    # return

    to_report <- list()


    # data input/checks/transformation ----------------------------------------
    receive_data <- shinymeta::metaReactive({
      # Here is where we should include the dataset calculation steps
      ..(dataset())
    })

    full_join_data <- function() {
      rv[["data"]] <- dplyr::full_join(rv[["data"]], receive_data())
    }



 


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
      lapply(seq_along(input[[BSAFE_ID$SEL_COLUMN]]), function(i) {
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

    
    # Display input data
    output[[BSAFE_ID$OUT_FILE_TABLE]] <- function() {
      bsafe::input_data_display(
        data = my_data(),
        select_analysis = input[[BSAFE_ID$SEL_ANALYSIS]],
        saf_topic = input[[BSAFE_ID$SEL_SAF_TOPIC]]
      )
    }

    # Data table preparation
    sel_analysis <- mod_select_analysis_server("sel_analysis", receive_data)
    selected_data <- sel_analysis[["data"]] # TODO: Careful with reactive overlap once poc is ready
    seed <- sel_analysis[["seed"]]

    map_prior <- mod_map_prior_server(
      "map_prior",
      data = selected_data,
      analysis_type = sel_analysis[["analysis_type"]],
      safety_topic = sel_analysis[["safety_topic"]],
      treatment = sel_analysis[["treatment"]],
      seed = sel_analysis[["seed"]]
    )

    robust_map <- mod_robust_map_server(
      "robust_map",
      data = selected_data,
      map_mcmc = map_prior[["map_mcmc"]],
      param_approx = map_prior[["param_approx"]],
      adj_tau = map_prior[["adj_tau"]],
      analysis_type = sel_analysis[["analysis_type"]],
      safety_topic = sel_analysis[["safety_topic"]],
      ess_method = map_prior[["ess_method"]],
      treatment = sel_analysis[["treatment"]],
      seed = sel_analysis[["seed"]]
    )

    new_trial <- mod_new_trial_analysis_server(
      "new_trial",
      data = selected_data,
      robust_map_mcmc = robust_map[["robust_map_mcmc"]],
      analysis_type = sel_analysis[["analysis_type"]],
      safety_topic = sel_analysis[["safety_topic"]],
      ess_method = map_prior[["ess_method"]],
      treatment = sel_analysis[["treatment"]],
      seed = sel_analysis[["seed"]]
    )
      

 
   



    # Posterior distribution




    # Mixture distributions for MAP Prior, Robust MAP Prior, Likelihood, or Posterior Distribution
    mix <- shinymeta::metaReactive2({
      shiny::req(input[[BSAFE_ID$BUT_UPDATE_STAT_INF]])

      shiny::isolate({
        if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
          shiny::req(input[[BSAFE_ID$SEL_DIST]])
          shinymeta::metaExpr(
            bsafe::mix_distribution_all(
              current_trial_data = ..(current_trial_data()),
              select_dist = ..(input[[BSAFE_ID$SEL_DIST]]),
              select_analysis = ..(input[[BSAFE_ID$SEL_ANALYSIS]]),
              param_approx = ..(param_approx()),
              robust_map_object = ..(robust_map_mcmc()),
              post_dist = ..(post_dist())
            )
          )
        } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
          shiny::req(input[[BSAFE_ID$SEL_DIST_AE]])
          shinymeta::metaExpr(bsafe::mix_distribution_all(
            current_trial_data = ..(current_trial_data()),
            select_dist = ..(input[[BSAFE_ID$SEL_DIST_AE]]),
            select_analysis = ..(input[[BSAFE_ID$SEL_ANALYSIS]]),
            param_approx = ..(param_approx()),
            robust_map_object = ..(robust_map_mcmc()),
            post_dist = ..(post_dist())
          ))
        }
      })
    })
   
    # NEW TRIAL ANALYSIS ----



    # DECISION MAKING ----
    # Slider input for proportion of adverse event (quantiles of distribution)
    ae_prop <- shiny::eventReactive(input[[BSAFE_ID$BUT_UPDATE_STAT_INF]], {
      if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        input[[BSAFE_ID$OUT_PERC_SLDR]] / 100
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        input[[BSAFE_ID$OUT_AE_PERC_SLDR]] / 100
      }
    })
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
            " in the population of the new trial after combining the prior (historical data)",
            " and the likelihood (new trial)."
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
            "The log scale of the robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ", # nolint: line_length_linter
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial if they were to receive placebo."
          ),
          "Posterior" = paste0(
            "The log scale of the posterior distribution represents information about the proportion of patients with ",
            input[[BSAFE_ID$SEL_SAF_TOPIC]],
            " in the population of the new trial after combining the prior (historical data) and the likelihood (new trial)." # nolint: line_length_linter
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
      DOWNLOAD_BOOL <- TRUE # nolint: object_name_linter

      lapply(1:input[[BSAFE_ID$SLDR_NUM_COMP]], function(i) {
        # checking whether at least one treatment AND control arm checkbox is ticked in the corresponding comparison
        if (length(input[[paste0("download_boxes_trt_", i)]]) == 0 |
          length(input[[paste0("download_boxes_ctrl_", i)]]) == 0) { # nolint: indentation_linter
          shinyjs::alert(
            paste0(
              "You have to enter both a Treatment arm as well as a Control arm in Table ",
              i
            )
          )
        }
      })
      for (i in 1:input[[BSAFE_ID$SLDR_NUM_COMP]]) {
        # disabling download functionality if not both treatment AND control arm checkboxes at least
        # contain one ticked box each in the corresponding comparison
        if (length(input[[paste0("download_boxes_trt_", i)]]) == 0 |
          length(input[[paste0("download_boxes_ctrl_", i)]]) == 0) { # nolint: indentation_linter
          DOWNLOAD_BOOL <- FALSE # nolint: object_name_linter
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

    # return ----

    to_report <- shiny::reactive({

      code <- local({
        ec <- shinymeta::newExpansionContext()
      shinymeta::expandChain(
        "# Data selection",
        sel_analysis[["data"]](),
        "# Map prior",
        "## Forest Plot",
        map_prior[["forest_plot"]](),
        "## Map Summary table",
        map_prior[["map_summary_table"]](),
        "# Robust Map Prior",
        "## Robust plot",
        robust_map[["robust_plot"]](),
        "## Robust summary",
        robust_map[["robust_summary"]](),
        "# New trial",
        "## Compare plot",
        new_trial[["compare_plot"]](),
        "## Compare table summary",
        new_trial[["compare_summary_table"]]()
      )
      })

      list(
        code = code,
        forest_plot = map_prior[["forest_plot"]](),
        map_summary_table = map_prior[["map_summary_table"]](),
        robust_plot = robust_map[["robust_plot"]](),
        robust_summary = robust_map[["robust_summary"]](),
        compare_plot = new_trial[["compare_plot"]](),
        compare_summary_table = new_trial[["compare_summary_table"]]()
      )
      
    })
    return(to_report)
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}

