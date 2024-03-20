mod_map_prior_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::sidebarLayout(
          shiny::sidebarPanel(
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
            shiny::actionButton(ns(BSAFE_ID$BUT_UPDATE_MAP), "Update")
          ),
          shiny::mainPanel(
            shiny::h2("Model Estimates"),
            # nolint start: line_length_linter
            shiny::h6(
              "Displayed are the point estimates for the mean (dots) and their respective 95% frequentistic confidence intervals.
               For a stratified (dashed light blue line) and meta (solid dark blue line) analysis.
               The blue highlighted part displays the 95% credible interval (CrI) for the mean and the MAP Prior."
            ),
            # nolint end
            shiny::plotOutput(ns(BSAFE_ID$OUT_FOREST_PLT)),
            shiny::h2("MAP Prior"),
            shiny::uiOutput(ns(BSAFE_ID$OUT_PREFACE_PRIOR_TXT)),
            shiny::uiOutput(ns(BSAFE_ID$OUT_DENSITY_FCT)),
            shiny::plotOutput(ns(BSAFE_ID$OUT_MIX_DENSITY_PLT)), # spinner MAP prior distribution
            shiny::tableOutput(ns(BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL)) # MAP prior distribution summary table
          )
        )
    )
} 

mod_map_prior_server <- function(id, data, analysis_type, safety_topic, treatment, seed) {

    mod <- function(input, output, session) {
    
    # Reactives

    adj_tau <- shinymeta::metaReactive2({
        shinymeta::metaExpr({
        bsafe::tau_adjust(
          select_analysis = ..(analysis_type()),
          hist_borrow = ..(input[[BSAFE_ID$SEL_HIST_BORROW]])
        )
      })
    })

    map_mcmc <- shinymeta::metaReactive2({
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
    })

    # Parametric approximation object
    param_approx <- shinymeta::metaReactive2({
        shinymeta::metaExpr({
          bsafe::parametric_approx(
            select_analysis = ..(analysis_type()),
            map_prior = ..(map_mcmc())
          )
        })
    })

    map_mix_density <- shinymeta::metaReactive({
      bsafe::param_mix_density_display( # nolint: object_usage_linter
        param_approx = ..(param_approx()),
        select_analysis = ..(analysis_type()),
        saf_topic = ..(safety_topic()),
        select_btrt = ..(treatment())
      )
    })

    map_summary_table <- shinymeta::metaReactive({
      bsafe::model_summary_display( # nolint: object_usage_linter
        map_object = ..(map_mcmc()),
        select_analysis = ..(analysis_type()),
        param_approx = ..(param_approx()),
        ess_method = ..(input[[BSAFE_ID$SEL_ESS_METHOD]])
      )
    })

    forest_plot <- shinymeta::metaReactive({
      bsafe::forest_plot_display(
        map_object = ..(map_mcmc()),
        select_analysis = ..(analysis_type()),
        saf_topic = ..(safety_topic()),
        select_btrt = ..(treatment())
      )
    })

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
      bsafe::map_prior_function_display(
        param_approx = param_approx(),
        select_analysis = analysis_type()
      )
    })

    # Display parametric mixture density
    output[[BSAFE_ID$OUT_MIX_DENSITY_PLT]] <- shiny::renderPlot({
      map_mix_density()
    })

    # Display model summary output
    output[[BSAFE_ID$OUT_MAP_PRIOR_SUM_TBL]] <- function() {
      ({
        map_summary_table() %>%
          knitr::kable("html") %>%
          kableExtra::kable_styling("striped")
      })
    }

    # This reactive works under the assumption that the three events are activated by the same button and, therefore,
    # the same data correspond to the three of them.
    to_report <- list()
    to_report[["map"]] <- 1
    
    # reactive_snapshot({
    # #   shiny::req(forest_plot())
    # #   shiny::req(map_mix_density())
    # #   shiny::req(map_summary_table())

    # #   ec <- shinymeta::newExpansionContext()

    # #   data <- shiny::isolate({
    # #     data()
    # #   }) # Block reactivity due to data changes only for final plots

    # #   if (!is.null(attr(data, "code"))) {
    # #     shinymeta::expandChain(
    # #       "# teal.data::get_code returns some library calls and assignments that are not required in our case",
    # #       "# that is why this call seems a bit unsual",
    # #       .expansionContext = ec
    # #     )
    # #     data_receive_code <- rlang::parse_expr(paste0("{", attr(data, "code"), "}"))
    # #     ec$substituteMetaReactive(
    # #       data,
    # #       function() {
    # #         shinymeta::metaExpr(..(data_receive_code))
    # #       }
    # #     )
    # #   }


    # #   list(
    # #     name = "MAP Prior",
    # #     forest = list(
    # #       code = shinymeta::expandChain(forest_plot(), .expansionContext = ec) |>
    # #         shinymeta::formatCode() |>
    # #         as.character() |>
    # #         paste(collapse = "\n"),
    # #       plot = forest_plot(),
    # #       prior_txt = preface_prior_txt(analysis_type())
    # #     ),
    # #     map = list(
    # #       code = shinymeta::expandChain(map_mix_density(), .expansionContext = ec) |>
    # #         shinymeta::formatCode() |>
    # #         as.character() |>
    # #         paste(collapse = "\n"),
    # #       plot = map_mix_density()
    # #     ),
    # #     summary = list(
    # #       code = shinymeta::expandChain(map_summary_table(), .expansionContext = ec) |>
    # #         shinymeta::formatCode() |>
    # #         as.character() |>
    # #         paste(collapse = "\n"),
    # #       table = map_summary_table()
    # #     )
    # #   )
    # # })

    return(
        list(
            map_mcmc = map_mcmc,
            param_approx = param_approx,
            adj_tau = adj_tau,
            ess_method = shiny::reactive(input[[BSAFE_ID$SEL_ESS_METHOD]])
        )
    )

    }

    shiny::moduleServer(id, mod)
}

mock_map_prior_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_map_prior_ui(
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

    x <- mod_map_prior_server(
      id = "mock",
      data = shiny::reactive(data),
      analysis_type = shiny::reactive("Incidence proportion"),
      safety_topic = shiny::reactive("Nausea"),
      treatment = shiny::reactive("Treatment"),
      seed = shiny::reactive(round(as.numeric(Sys.time()), 0))
    )

    output[["out"]] <- shiny::renderPrint({
        x[["map_mcmc"]]()
    #   utils::str(x)
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
