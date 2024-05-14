mod_map_prior_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
    # TODO: SEL_TAU has a single value
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
    shiny::selectInput(ns(BSAFE_ID$SEL_ESS_METHOD),
      "Effective Sample Size Method",
      choices = BSAFE_CHOICES$SEL_ESS_METHOD,
      selected = BSAFE_DEFAULTS$SEL_ESS_METHOD
    ),
    shiny::actionButton(ns("submit"), "Update Map Priors")
  )

  main <- list(
    shiny::h4("Model Estimates"),
    # nolint start: line_length_linter
    shiny::h6(
      "Displayed are the point estimates for the mean (dots) and their respective 95% frequentistic confidence intervals.
               For a stratified (dashed light blue line) and meta (solid dark blue line) analysis.
               The blue highlighted part displays the 95% credible interval (CrI) for the mean and the MAP Prior."
    ),
    # nolint end
    shiny::plotOutput(ns(BSAFE_ID$OUT_FOREST_PLT)),
    shiny::h4("MAP Prior"),
    shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_PRIOR_TXT)),
    shiny::uiOutput(ns(BSAFE_ID$OUT_DENSITY_FCT)),
    shiny::plotOutput(ns(BSAFE_ID$OUT_MIX_DENSITY_PLT)), # spinner MAP prior distribution
    shiny::htmlOutput(ns(BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL)) # MAP prior distribution summary table
  )

  list(
    side = side,
    main = main
  )
}

mod_map_prior_server <- function(id, data, analysis_type, safety_topic, treatment, seed) {
  mod <- function(input, output, session) {
    # Reactives


    updated_map_priors <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input[["submit"]], {
      shiny::req(input[["submit"]] > 0)
      updated_map_priors(TRUE)
    })

    shiny::observe({
      # try are required so it depends on changes on any of the elements. We want to set it to false regardless of them
      # being an error or not
      try(data())
      try(analysis_type())
      try(input[[BSAFE_ID$SEL_HIST_BORROW]])
      try(input[[BSAFE_ID$SEL_TAU]])
      try(seed())
      updated_map_priors(FALSE)
    })



    adj_tau <- shinymeta::metaReactive2(
      {
        validate_update_map_prior(updated_map_priors())
        shinymeta::metaExpr({
          bsafe::tau_adjust(
            select_analysis = ..(analysis_type()),
            hist_borrow = ..(input[[BSAFE_ID$SEL_HIST_BORROW]])
          )
        })
      },
      varname = "adj_tau"
    )

    map_mcmc <- shinymeta::metaReactive2(
      {
        validate_update_map_prior(updated_map_priors())
        shiny::req(seed())
        # At this moment the origin of the error Argument eta must be a nonempty numeric vector is pervasive
        # We have opted for a catch all approach while we forward this error to the bsafe package developers
        tryCatch(
          {
            shinymeta::metaExpr({
              bsafe::map_prior_func(
                input_data = ..(data()),
                select_analysis = ..(analysis_type()),
                tau_dist = ..(input[[BSAFE_ID$SEL_TAU]]),
                adj_tau = ..(adj_tau()),
                seed = ..(seed())
              )
            })
          },
          error = function(e) {
            shiny::validate(FALSE, paste("Error calculating::map_prior_func:", e[["message"]]))
          }
        )
      },
      varname = "map_mcmc"
    )

    # Parametric approximation object
    param_approx <- shinymeta::metaReactive2(
      {
        validate_update_map_prior(updated_map_priors())
        shinymeta::metaExpr({
          bsafe::parametric_approx(
            select_analysis = ..(analysis_type()),
            map_prior = ..(map_mcmc())
          )
        })
      },
      varname = "parm_approx"
    )

    map_mix_density <- shinymeta::metaReactive({
      bsafe::param_mix_density_display( # nolint: object_usage_linter
        param_approx = ..(param_approx()),
        select_analysis = ..(analysis_type()),
        saf_topic = ..(safety_topic()),
        select_btrt = ..(treatment())
      )
    })

    map_summary_table <- shinymeta::metaReactive(
      {
        bsafe::model_summary_display( # nolint: object_usage_linter
          map_object = ..(map_mcmc()),
          select_analysis = ..(analysis_type()),
          param_approx = ..(param_approx()),
          ess_method = ..(input[[BSAFE_ID$SEL_ESS_METHOD]])
        )
      },
      varname = "map_summary_table"
    )

    forest_plot <- shinymeta::metaReactive(
      {
        bsafe::forest_plot_display(
          map_object = ..(map_mcmc()),
          select_analysis = ..(analysis_type()),
          saf_topic = ..(safety_topic()),
          select_btrt = ..(treatment())
        )
      },
      varname = "forest_plot"
    )

    # Outputs and return

    # Display forest plot
    output[[BSAFE_ID$OUT_FOREST_PLT]] <- shiny::renderPlot({
      forest_plot()
    })

    # Preface the MAP prior distribution
    output[[BSAFE_ID$OUT_PREFACE_PRIOR_TXT]] <- shiny::renderUI({
      shiny::h6(preface_prior_txt(analysis_type()))
    })

    # Display parametric approximation mixture density function
    # TODO: Cannot be included in the reporter MATHJAX not supported
    output[[BSAFE_ID$OUT_DENSITY_FCT]] <- shiny::renderUI({
      # TODO: This function should return the element to be displayed not a shiny element
      f <- bsafe::map_prior_function_display(
        param_approx = param_approx(),
        select_analysis = analysis_type()
      )
      f[[2]][["name"]] <- "span"
      f
    })

    # Display parametric mixture density
    output[[BSAFE_ID$OUT_MIX_DENSITY_PLT]] <- shiny::renderPlot({
      map_mix_density()
    })

    # Display model summary output
    output[[BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL]] <- shiny::renderText({
      map_summary_table() %>%
        knitr::kable("html") %>%
        kableExtra::kable_styling("striped")
    })

    r <- list(
        map_mcmc = map_mcmc,
        param_approx = param_approx,
        adj_tau = adj_tau,
        ess_method = shiny::reactive(input[[BSAFE_ID$SEL_ESS_METHOD]]),
        forest_plot = forest_plot,
        map_summary_table = map_summary_table
      )

    do.call(shiny::exportTestValues, as.list(environment()))

    return(r)
  }

  shiny::moduleServer(id, mod)
}

validate_update_map_prior <- function(updated_map_priors) {
  shiny::validate(shiny::need(updated_map_priors, "Selection or data has changed please update Map Prior"))
}

preface_prior_txt <- function(sel_analysis) {
  checkmate::assert_subset(sel_analysis, BSAFE_CHOICES$SEL_ANALYSIS)
  if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    return(paste0("Using a MAP approach, the prior approximated as the Beta mixture distribution:"))
  } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    return(paste0("Using a MAP approach, the log scale of the prior approximated as the Normal mixture distribution:"))
  }
}

mock_map_prior_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      shiny::actionButton("invalidate_data", "Invalidate Data"),
      shiny::actionButton("invalidate_analysis", "Invalidate Analysis"),
      shiny::actionButton("invalidate_safety", "Invalidate Safety"),
      shiny::actionButton("invalidate_treatment", "Invalidate Treatment"),
      shiny::actionButton("invalidate_seed", "Invalidate Seed"),
      mod_map_prior_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    data <- shinymeta::metaReactive2({
      input[["invalidate_data"]]
      shinymeta::metaExpr({
        data <- data.frame(
          STUDYID = factor(c(9)),
          N = c(123L),
          N_WITH_AE = c(21L),
          HIST = c(1)
        )
      })
    }, varname = "data")

    analysis_type <- shiny::reactive({
      input[["invalidate_analysis"]]
      "Incidence proportion"})
    safety_topic <- shiny::reactive({
      input[["invalidate_safety"]]
      "Nausea"})
    treatment <- shiny::reactive({
      input[["invalidate_treatment"]]
      "Treatment"})
    seed <- shiny::reactive({
      input[["invalidate_seed"]]
      1})

    r <- mod_map_prior_server(
      id = "mock",
      data = data,
      analysis_type = analysis_type,
      safety_topic = safety_topic,
      treatment = treatment,
      seed = seed
    )

    output[["out"]] <- shiny::renderPrint({
      r[["map_mcmc"]]()
      utils::str(r)
    })
    do.call(shiny::exportTestValues, as.list(environment()))
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
