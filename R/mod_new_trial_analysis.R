mod_new_trial_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
    shinyjs::useShinyjs(),
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
    ))
  )

  main <- list(
    shinyjs::hidden(shiny::div(
      id = ns(BSAFE_ID$DIV_NTA_INCI_MAIN),
      "To assess prior data conflict, compare the prior and posterior probability density function, and the likelihood of the observed data.", # nolint: line_length_linter
    )),
    shinyjs::hidden(shiny::div(
      id = ns(BSAFE_ID$DIV_NTA_AE_MAIN),
      "To assess prior data conflict, compare the prior and posterior probability density function, and the log likelihood of the observed data on the log scale.", # nolint: line_length_linter
    )),
    shiny::plotOutput(ns(BSAFE_ID$OUT_COMPARE_PLT)),
    shiny::tableOutput(ns(BSAFE_ID$OUT_COMPARE_SUM_TBL))
  )

  list(
    side = side,
    main = main
  )
}

mod_new_trial_analysis_server <- function(
    id, data, robust_map_mcmc,
    analysis_type, safety_topic,
    ess_method, treatment, seed) {
  mod <- function(input, output, session) {
    current_trial_data <- shiny::reactive({
      shiny::req(analysis_type())
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        checkmate::assert_true(input[[BSAFE_ID$SLDR_N_PAT]] >= input[[BSAFE_ID$SLDR_N_AE]])
        list(
          new_v1 = input[[BSAFE_ID$SLDR_N_PAT]],
          new_v2 = input[[BSAFE_ID$SLDR_N_AE]]
        )
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        list(
          new_v1 = input[[BSAFE_ID$SLDR_AE_FIRST_OCCURENCE]],
          new_v2 = input[[BSAFE_ID$SLDR_CUMM_TIME_FIRST_AE]]
        )
      }
    })

    post_dist <- shinymeta::metaReactive2(
      {
        shinymeta::metaExpr(
          bsafe::posterior_dist(
            select_analysis = ..(analysis_type()),
            input_data = ..(data()),
            robust_map_prior = ..(robust_map_mcmc()),
            explore = TRUE,
            new_v1 = ..(current_trial_data()[["new_v1"]]),
            new_v2 = ..(current_trial_data()[["new_v2"]]),
            seed = ..(seed())
          )
        )
      },
      varname = "post_dist"
    )

    new_trial_analysis <- shinymeta::metaReactive2(
      {
        shinymeta::metaExpr(
          bsafe::new_trial_compare(
            select_analysis = ..(analysis_type()),
            robust_map_prior = ..(robust_map_mcmc()),
            new_v1 = ..(current_trial_data()[["new_v1"]]),
            new_v2 = ..(current_trial_data()[["new_v2"]]),
            post_dist = ..(post_dist())
          )
        )
      },
      varname = "new_trial_analysis"
    )

    shiny::observe({
      shiny::updateSliderInput(session,
        BSAFE_ID$SLDR_N_AE,
        label = paste0(
          "Number of Patients with AE ",
          safety_topic()
        ),
        min = 0,
        max = input[[BSAFE_ID$SLDR_N_PAT]],
        value = 0,
        step = 1
      )
    })

    shiny::observeEvent(analysis_type(), {
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::show(BSAFE_ID$DIV_NTA_INCI)
        shinyjs::show(BSAFE_ID$DIV_NTA_INCI_MAIN)
        shinyjs::hide(BSAFE_ID$DIV_NTA_AE)
        shinyjs::hide(BSAFE_ID$DIV_NTA_AE_MAIN)
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::hide(BSAFE_ID$DIV_NTA_INCI)
        shinyjs::hide(BSAFE_ID$DIV_NTA_INCI_MAIN)
        shinyjs::show(BSAFE_ID$DIV_NTA_AE)
        shinyjs::show(BSAFE_ID$DIV_NTA_AE_MAIN)
      }
    })

    compare_plot <- shinymeta::metaReactive(
      {
        bsafe::nta_data_conflict_assassment_plot(
          select_analysis = ..(analysis_type()),
          new_trial_analysis = ..(new_trial_analysis()),
          saf_topic = ..(safety_topic()),
          select_btrt = ..(treatment())
        )
      },
      varname = "compare_plot"
    )

    output[[BSAFE_ID$OUT_COMPARE_PLT]] <- shiny::renderPlot({
      compare_plot()
    })

    compare_summary_table <- shinymeta::metaReactive(
      {
        bsafe::summary_stat_all_display(
          select_analysis = ..(analysis_type()),
          robust_map_object = ..(robust_map_mcmc()),
          ess_method = ..(ess_method()),
          current_trial_data = ..(current_trial_data()),
          post_dist = ..(post_dist()),
          download = FALSE
        )
      },
      varname = "compare_summary_table"
    )

    output[[BSAFE_ID$OUT_COMPARE_SUM_TBL]] <- shiny::renderTable({
      compare_summary_table()
    })

    r <- list(
      new_trial_analysis = new_trial_analysis,
      post_dist = post_dist,
      current_trial_data = current_trial_data,
      compare_plot = compare_plot,
      compare_summary_table = compare_summary_table
    )

    do.call(shiny::exportTestValues, as.list(environment()))

    return(r)
  }

  shiny::moduleServer(
    id,
    mod
  )
}

mock_new_trial_analysis_mod <- function(analysis_type = BSAFE_CHOICES$SEL_ANALYSIS[1]) {
  ui <- function(request) {
    shiny::fluidPage(
      mod_new_trial_analysis_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    metareact_in <- purrr::imap(teal.modules.bsafe::test_na_in, function(v, n) {
      shinymeta::metaReactive(
        {
          rlang::quo(teal.modules.bsafe::test_na_in[[!!n]])
        },
        quoted = TRUE,
        varname = n
      )
    })

    metareact_in[["analysis_type"]] <- shinymeta::metaReactive(
      {
        shinymeta::..(analysis_type)
      },
      varname = "analysis_type"
    )

    r <- do.call(mod_new_trial_analysis_server, c(list(id = "mock"), metareact_in))
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
