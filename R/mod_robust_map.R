mod_robust_map_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
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
        )
  )

  main <- list(
      shiny::h2("Robust MAP Prior"),
        shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_ROB_TXT)),
        shiny::uiOutput(ns(BSAFE_ID$OUT_ROB_DENSITY_FCT)),
        shiny::plotOutput(ns(BSAFE_ID$OUT_ROB_MAP_PLT)), # spinner
        shiny::tableOutput(ns(BSAFE_ID$OUT_ROB_SUM_TBL))
  )

  list(side = side, main = main)
}

mod_robust_map_server <- function(id, data, map_mcmc, param_approx, adj_tau, analysis_type, safety_topic, ess_method, treatment, seed) {
  mod <- function(input, output, server) {
    # Compare robust MAP prior to MAP prior

    shiny::observeEvent(analysis_type(), {
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::hide(BSAFE_ID$DIV_ROB_MEAN)
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::show(BSAFE_ID$DIV_ROB_MEAN)
      }
    })

    robust_map_mcmc <- shinymeta::metaReactive2({
      # rob weight in function and return that
        shiny::req(analysis_type())
        shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
        shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])
        shinymeta::metaExpr({
          bsafe::robust_map(
            select_analysis = ..(analysis_type()),
            param_approx = ..(param_approx()),
            input_data = ..(data()),
            robust_weight = ..(input[[BSAFE_ID$SLDR_ROB_WEIGHT]]),
            robust_mean = ..(input[[BSAFE_ID$SLDR_ROB_MEAN]]), # TODO: It use the mean in the selector even when it is hidden
            adj_tau = ..(adj_tau()),
            seed = ..(seed())
          )
      })
    })

    rob_comp <- shinymeta::metaReactive2({
      shinymeta::metaExpr(
        bsafe::robust_compare(
          select_analysis = ..(analysis_type()),
          robust_map_prior = ..(robust_map_mcmc()),
          param_approx = ..(param_approx())
        )
      )
    })

    # Preface robust MAP prior output
    robust_txt <- shinymeta::metaReactive2({
      shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
      shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])
      shinymeta::metaExpr({
        shiny::withMathJax(
          shiny::h6(
            preface_rob_txt(
              sel_analysis = ..(analysis_type()),
              rob_weight = ..(input[[BSAFE_ID$SLDR_ROB_WEIGHT]]),
              rob_mean = ..(input[[BSAFE_ID$SLDR_ROB_MEAN]])
            )
          )
        )
      })
    })

    robust_plot <- shinymeta::metaReactive({
      bsafe::robust_map_prior_plot( # nolint: object_usage_linter
        rob_comp = ..(rob_comp()),
        saf_topic = ..(safety_topic()),
        select_btrt = ..(treatment()),
        select_analysis = ..(analysis_type())
      )
    })

    robust_summary <- shinymeta::metaReactive2({
      shiny::req(robust_map_mcmc())
      shinymeta::metaExpr(
        bsafe::summary_stats_robust_map_prior_display( # nolint: object_usage_linter
          map_object = ..(map_mcmc()),
          select_analysis = ..(analysis_type()),
          param_approx = ..(param_approx()),
          ess_method = ..(ess_method()),
          robust_map_object = ..(robust_map_mcmc()),
          rob_ess_method = ..(input[[BSAFE_ID$SEL_ROB_ESS_METHOD]]),
          download = FALSE
        )
      )
    })

    output[[BSAFE_ID$OUT_PREFACE_ROB_TXT]] <- shiny::renderUI({
      robust_txt()
    })

    robust_formula <- shinymeta::metaReactive2({
      shiny::req(input[[BSAFE_ID$BUT_UPDATE_ROB]])
      shiny::isolate({
        shinymeta::metaExpr({
          bsafe::robust_map_prior_mix_dens_display(
            robust_map_object = ..(robust_map_mcmc()),
            select_analysis = ..(analysis_type())
          )
        })
      })
    })

    # Display robust MAP prior mixture density function
    output[[BSAFE_ID$OUT_ROB_DENSITY_FCT]] <- shiny::renderUI({
      robust_formula()
    })

    # Compare robust MAP prior to MAP prior
    output[[BSAFE_ID$OUT_ROB_MAP_PLT]] <- shiny::renderPlot({
      robust_plot()
    })

    # Display summary stats of robust MAP prior and MAP prior
    output[[BSAFE_ID$OUT_ROB_SUM_TBL]] <- function() {
      robust_summary() %>%
        knitr::kable("html") %>%
        kableExtra::kable_styling("striped")
    }

    list(
      robust_map_mcmc = robust_map_mcmc,
      robust_plot = robust_plot,
      robust_summary = robust_summary
    )
  }

  shiny::moduleServer(id, mod)
}

mock_robust_map_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_robust_map_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {

    data <- data.frame(
        STUDYID = factor(c(9)),
        N = c(123L),
        N_WITH_AE = c(21L),
        HIST = c(1)
    )

    map_prior_out <- readRDS("map_prior_out.rds")

    x <- mod_robust_map_server(
      id = "mock",
      data = shiny::reactive(data),
      map_mcmc = shiny::reactive(map_prior_out[["map_mcmc"]]),
      param_approx =  shiny::reactive(map_prior_out[["param_approx"]]),
      adj_tau = shiny::reactive(map_prior_out[["adj_tau"]]),
      analysis_type = shiny::reactive("Incidence proportion"),
      treatment = shiny::reactive("Treatment"),
      safety_topic = shiny::reactive("Nausea"),
      ess_method = shiny::reactive(map_prior_out[["ess_method"]]),
      seed = shiny::reactive(round(as.numeric(Sys.time()), 0))
    )

    output[["out"]] <- shiny::renderPrint({
    #   x[["data"]]()
    #   utils::str(x)
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
