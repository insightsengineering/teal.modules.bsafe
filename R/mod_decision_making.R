mod_decision_making_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
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
    )
  )

  main <- list(
    shiny::uiOutput(ns(BSAFE_ID$OUT_DM_HEADER_TXT)),
    shiny::uiOutput(ns(BSAFE_ID$OUT_DM_PREFACE_TXT)),
    shiny::plotOutput(ns(BSAFE_ID$OUT_STAT_INF_DENSITY_PLT)), # spinner
    shiny::textOutput(ns(BSAFE_ID$OUT_AREA_UNDER_CURVE)),
    shiny::tableOutput(ns(BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL))
  )

  list(
    side = side,
    main = main
  )
}

prior_func <- bsafe::map_prior_func

mod_decision_making_server <- function(
    id, data, analysis_type,
    safety_topic, treatment,
    current_trial_data, param_approx,
    robust_map_mcmc, post_dist,
    new_trial_analysis) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    mix <- shinymeta::metaReactive2({
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shiny::req(input[[BSAFE_ID$SEL_DIST]])
        shinymeta::metaExpr({
          bsafe::mix_distribution_all(
            current_trial_data = ..(current_trial_data()),
            select_dist = ..(input[[BSAFE_ID$SEL_DIST]]),
            select_analysis = ..(analysis_type()),
            param_approx = ..(param_approx()),
            robust_map_object = ..(robust_map_mcmc()),
            post_dist = ..(post_dist())
          )
        })
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shiny::req(input[[BSAFE_ID$SEL_DIST_AE]])
        shinymeta::metaExpr({
          bsafe::mix_distribution_all(
            current_trial_data = ..(current_trial_data()),
            select_dist = ..(input[[BSAFE_ID$SEL_DIST_AE]]),
            select_analysis = ..(input[[BSAFE_ID$SEL_ANALYSIS]]),
            param_approx = ..(param_approx()),
            robust_map_object = ..(robust_map_mcmc()),
            post_dist = ..(post_dist())
          )
        })
      }
    }, varname = "mix")

    stat_inf_dist <- shinymeta::metaReactive2({
      select_dist_selector <- function(sel_analysis, sel_dist, sel_dist_ae) {
        if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
          return(sel_dist)
        } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
          return(sel_dist_ae)
        }
      }

      select_dist <- select_dist_selector(
        sel_analysis = analysis_type(),
        sel_dist = input[[BSAFE_ID$SEL_DIST]],
        sel_dist_ae = input[[BSAFE_ID$SEL_DIST_AE]]
      )

      shinymeta::metaExpr({
        bsafe::sampling_all_plot(
          select_analysis = ..(analysis_type()),
          select_dist = ..(select_dist),
          param_approx = ..(param_approx()),
          new_trial_analysis = ..(new_trial_analysis())
        )
      })
    }, varname = "stat_inf_dist")

    # Slider input for proportion of adverse event (quantiles of distribution)
    ae_prop <- shiny::reactive({
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shiny::req(!is.null(input[[BSAFE_ID$OUT_PERC_SLDR]]))
        input[[BSAFE_ID$OUT_PERC_SLDR]] / 100
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shiny::req(!is.null(input[[BSAFE_ID$OUT_AE_PERC_SLDR]]))
        input[[BSAFE_ID$OUT_AE_PERC_SLDR]] / 100
      }
    })

    shiny::observeEvent(analysis_type(), {
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::show(BSAFE_ID$DIV_DM_INCI)
        shinyjs::hide(BSAFE_ID$DIV_DM_AE)
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::show(BSAFE_ID$DIV_DM_AE)
        shinyjs::hide(BSAFE_ID$DIV_DM_INCI)
      }
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
    outputOptions(output, BSAFE_ID$OUT_PERC_SLDR, suspendWhenHidden = FALSE)


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

    outputOptions(output, BSAFE_ID$OUT_AE_PERC_SLDR, suspendWhenHidden = FALSE)

    # Header text
    dm_header <- shiny::reactive({
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        input[[BSAFE_ID$SEL_DIST]]
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        input[[BSAFE_ID$SEL_DIST_AE]]
      }
    })
    output[[BSAFE_ID$OUT_DM_HEADER_TXT]] <- shiny::renderUI({
      shiny::h4(dm_header())
    })


    # Preface text for each distribution

    dm_preface <- shiny::reactive({
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        switch(input[[BSAFE_ID$SEL_DIST]],
          "Likelihood" = paste0(
            "The likelihood represents information about the proportion of patients with ",
            safety_topic(),
            " in the population of the new trial that is contained in the observed data."
          ),
          "MAP Prior" = paste0(""),
          "Robust MAP Prior" = paste0(
            "The robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ",
            safety_topic(),
            " in the population of the new trial if they were to receive placebo."
          ),
          "Posterior" = paste0(
            "The posterior distribution represents information about the proportion of patients with ",
            safety_topic(),
            " in the population of the new trial after combining the prior (historical data)",
            " and the likelihood (new trial)."
          )
        )
      } else if (input[[BSAFE_ID$SEL_ANALYSIS]] == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        switch(input[[BSAFE_ID$SEL_DIST]],
          "Likelihood" = paste0(
            "The log scale of the likelihood represents information about the proportion of patients with ",
            safety_topic(),
            " in the population of the new trial that is contained in the observed data."
          ),
          "MAP Prior" = paste0(""),
          "Robust MAP Prior" = paste0(
            "The log scale of the robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ", # nolint: line_length_linter
            safety_topic(),
            " in the population of the new trial if they were to receive placebo."
          ),
          "Posterior" = paste0(
            "The log scale of the posterior distribution represents information about the proportion of patients with ",
            safety_topic(),
            " in the population of the new trial after combining the prior (historical data) and the likelihood (new trial)." # nolint: line_length_linter
          )
        )
      }
    })
    output[[BSAFE_ID$OUT_DM_PREFACE_TXT]] <- shiny::renderUI({
      dm_preface()
    })

    # Plot density
    stat_inf_plot <- shinymeta::metaReactive({
      bsafe::decision_making_density_plot(
        stat_inf_dist = ..(stat_inf_dist()),
        select_analysis = ..(analysis_type()),
        ae_prop = ..(ae_prop()),
        saf_topic = ..(safety_topic()),
        select_btrt = ..(treatment())
      )
    })

    output[[BSAFE_ID$OUT_STAT_INF_DENSITY_PLT]] <- shiny::renderPlot({
      stat_inf_plot()
    })

    # Interpret area under the curve
    auc <- shinymeta::metaReactive({
      bsafe::area_under_the_curve(
        ae_prop = ..(ae_prop()),
        mix = ..(mix()),
        saf_topic = ..(safety_topic())
      )
    }, varname = "auc")
    output[[BSAFE_ID$OUT_AREA_UNDER_CURVE]] <- shiny::renderText({
      auc()
    })

    # Table of preset statistical inference statements
    preset_statements <- shinymeta::metaReactive({
      d <- bsafe::preset_stat_table(
        mix = ..(mix()),
        saf_topic = ..(safety_topic()),
        select_analysis = ..(analysis_type())
      )
      names(d) <- "Statement"
      d
    }, varname = "preset_statements")

    output[[BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL]] <- shiny::renderTable({
      preset_statements()
    })



    return(
      list(
        header = dm_header,
        preface = dm_preface,
        stat_inf_plot = stat_inf_plot,
        auc = auc,
        preset_statements = preset_statements
      )
    )
  }

  shiny::moduleServer(id, mod)
}

mock_decision_making_mod <- function() {
  ui <- function(request) {
    shiny::fluidPage(
      mod_decision_making_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    trial_in <- readRDS("nt.rds")
    trial_in_react <- purrr::map(trial_in, ~ local({
      shiny::reactive({
        .x
      })
    }))
    x <- do.call(mod_decision_making_server, c(list(id = "mock"), trial_in_react))
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
