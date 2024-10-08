mod_robust_map_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
    shinyjs::useShinyjs(),
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
    shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_ROB_TXT)),
    shiny::uiOutput(ns(BSAFE_ID$OUT_ROB_DENSITY_FCT)),
    shiny::plotOutput(ns(BSAFE_ID$OUT_ROB_MAP_PLT)), # spinner
    shiny::htmlOutput(ns(BSAFE_ID$OUT_ROB_SUM_TBL))
  )

  list(side = side, main = main)
}

mod_robust_map_server <- function(
    id, data, map_mcmc, param_approx, adj_tau, analysis_type, safety_topic, ess_method, treatment, seed) {
  mod <- function(input, output, server) {
    # Compare robust MAP prior to MAP prior

    shiny::observeEvent(analysis_type(), {
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::hide(BSAFE_ID$DIV_ROB_MEAN)
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::show(BSAFE_ID$DIV_ROB_MEAN)
      }
    })

    robust_map_mcmc <- shinymeta::metaReactive2(
      {
        # rob weight in function and return that
        # TODO: bsafe::robust_map cannot handle values below than 1, because a log is calculated in bsafe and then RBest
        # complains because mean cannot be below 0. BSAFE_ID$SLDR_ROB_MEAN offers values below one that when
        # log-transformed are <0
        # TODO: bsafe throws error for given combinations on prior weight and prior mean exp
        shiny::req(analysis_type())
        shiny::req(input[[BSAFE_ID$SLDR_ROB_WEIGHT]])
        shiny::req(input[[BSAFE_ID$SLDR_ROB_MEAN]])
        shinymeta::metaExpr({
          bsafe::robust_map(
            select_analysis = ..(analysis_type()),
            param_approx = ..(param_approx()),
            input_data = ..(data()),
            robust_weight = ..(input[[BSAFE_ID$SLDR_ROB_WEIGHT]]),
            # TODO: It uses the mean in the selector even when it is hidden
            # TODO: Method selection is done in bsafe which does not seem correct
            robust_mean = ..(input[[BSAFE_ID$SLDR_ROB_MEAN]]),
            adj_tau = ..(adj_tau()),
            seed = ..(seed())
          )
        })
      },
      varname = "robust_map_mc_mc"
    )

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
    robust_txt <- shinymeta::metaReactive2(
      {
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
      },
      varname = "robust_txt"
    )

    robust_plot <- shinymeta::metaReactive(
      {
        bsafe::robust_map_prior_plot( # nolint: object_usage_linter
          rob_comp = ..(rob_comp()),
          saf_topic = ..(safety_topic()),
          select_btrt = ..(treatment()),
          select_analysis = ..(analysis_type())
        )
      },
      varname = "robust_plot"
    )

    robust_summary <- shinymeta::metaReactive2(
      {
        shiny::req(robust_map_mcmc())
        shinymeta::metaExpr(
          bsafe::summary_stats_robust_map_prior_display( # nolint: object_usage_linter
            map_object = ..(map_mcmc()),
            select_analysis = ..(analysis_type()),
            param_approx = ..(param_approx()),
            ess_method = ..(ess_method()),
            robust_map_object = ..(robust_map_mcmc()),
            rob_ess_method = ..(input[[BSAFE_ID$SEL_ROB_ESS_METHOD]]),
            numerical = FALSE,
            seed = ..(seed())
          )
        )
      },
      varname = "robust_summary"
    )

    output[[BSAFE_ID$OUT_PREFACE_ROB_TXT]] <- shiny::renderUI({
      robust_txt()
    })

    robust_formula <- shinymeta::metaReactive(
      {
        shinymeta::metaExpr({
          bsafe::robust_map_prior_mix_dens_display(
            robust_map_object = ..(robust_map_mcmc()),
            select_analysis = ..(analysis_type())
          )
        })
      },
      varname = "robust_formula"
    )

    # Display robust MAP prior mixture density function
    output[[BSAFE_ID$OUT_ROB_DENSITY_FCT]] <- shiny::renderUI({
      f <- robust_formula()
      shiny::withMathJax(f)
    })

    # Compare robust MAP prior to MAP prior
    output[[BSAFE_ID$OUT_ROB_MAP_PLT]] <- shiny::renderPlot({
      robust_plot()
    })

    # Display summary stats of robust MAP prior and MAP prior
    output[[BSAFE_ID$OUT_ROB_SUM_TBL]] <- shiny::renderText({
      robust_summary() %>%
        knitr::kable("html") %>%
        kableExtra::kable_styling("striped")
    })

    r <- list(
      robust_map_mcmc = robust_map_mcmc,
      robust_plot = robust_plot,
      robust_summary = robust_summary
    )

    do.call(shiny::exportTestValues, as.list(environment()))

    return(r)
  }

  shiny::moduleServer(id, mod)
}

preface_rob_txt <- function(sel_analysis, rob_weight, rob_mean) {
  if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    return(paste0(
      "Based on a weakly informative conjugate component with weight w = ",
      rob_weight, " and mean ", rob_mean,
      " the robust MAP prior is approximated as (1-w) * (MAP Prior) + w * (weakly informative prior): "
    ))
  } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    return(paste0(
      "Based on a weakly informative conjugate component with w =  ",
      rob_weight, " and mean ", rob_mean,
      " on the exp scale (", round(log(rob_mean), 4), " on the log scale) of the robust MAP prior is approximated as ",
      " (1 - w) * (MAP prior) + w * (weakly informative prior): "
    ))
  }
}

mock_robust_map_mod <- function(analysis_type = BSAFE_CHOICES$SEL_ANALYSIS[1]) {
  ui <- function(request) {
    shiny::fluidPage(
      mod_robust_map_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    metareact_in <- purrr::imap(teal.modules.bsafe::test_rm_in, function(v, n) {
      shinymeta::metaReactive(
        {
          rlang::quo(teal.modules.bsafe::test_rm_in[[!!n]])
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

    r <- do.call(mod_robust_map_server, c(list(id = "mock"), metareact_in))

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
