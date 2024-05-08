mod_decision_making_ui <- function(id) {
  ns <- shiny::NS(id)

  side <- list(
    shiny::selectInput(ns(BSAFE_ID$SEL_DIST),
      "Make statistical inference about the",
      choices = BSAFE_CHOICES$SEL_DIST,
      selected = BSAFE_DEFAULTS$SEL_DIST
    ),
    shinyjs::hidden(
      shiny::div(
        id = ns(BSAFE_ID$DIV_DM_INCI),
        shiny::withMathJax("Adjust sliders for \\(P(LB_{AE} < p_{AE} < UB_{AE})\\),"),
        htmltools::HTML("<br/>"),
        shiny::uiOutput(ns(BSAFE_ID$OUT_PERC_SLDR_CONT)),
      )
    ),
    shinyjs::hidden(
      shiny::div(
        id = ns(BSAFE_ID$DIV_DM_AE),
        shiny::withMathJax("Adjust sliders for \\(P(LB_{AE} < p_{AE} < UB_{AE})\\),"),
        htmltools::HTML("<br/>"),
        shiny::uiOutput(ns(BSAFE_ID$OUT_AE_PERC_SLDR_CONT))
      )
    )
  )

  main <- list(
    shinyjs::useShinyjs(),
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

mod_decision_making_server <- function(
    id, data, analysis_type,
    safety_topic, treatment,
    current_trial_data, param_approx,
    robust_map_mcmc, post_dist,
    new_trial_analysis) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    mix <- shinymeta::metaReactive2(
      {
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
      },
      varname = "mix"
    )

    stat_inf_dist <- shinymeta::metaReactive2(
      {
        shinymeta::metaExpr({
          bsafe::sampling_all_plot(
            select_analysis = ..(analysis_type()),
            select_dist = ..(input[[BSAFE_ID$SEL_DIST]]),
            param_approx = ..(param_approx()),
            new_trial_analysis = ..(new_trial_analysis())
          )
        })
      },
      varname = "stat_inf_dist"
    )

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
      shiny::req(analysis_type())
      if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
        shinyjs::show(BSAFE_ID$DIV_DM_INCI)
        shinyjs::hide(BSAFE_ID$DIV_DM_AE)
      } else if (analysis_type() == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
        shinyjs::show(BSAFE_ID$DIV_DM_AE)
        shinyjs::hide(BSAFE_ID$DIV_DM_INCI)
      }
    })

    output[[BSAFE_ID$OUT_PERC_SLDR_CONT]] <- shiny::renderUI({
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
    shiny::outputOptions(output, BSAFE_ID$OUT_PERC_SLDR, suspendWhenHidden = FALSE)


    output[[BSAFE_ID$OUT_AE_PERC_SLDR_CONT]] <- shiny::renderUI({
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

    shiny::outputOptions(output, BSAFE_ID$OUT_AE_PERC_SLDR, suspendWhenHidden = FALSE)

    # Header text
    dm_header <- shiny::reactive({
      input[[BSAFE_ID$SEL_DIST]]
    })
    output[[BSAFE_ID$OUT_DM_HEADER_TXT]] <- shiny::renderUI({
      shiny::h4(dm_header())
    })


    # Preface text for each distribution

    dm_preface <- shiny::reactive({
      get_preface(analysis_type(), safety_topic(), input[[BSAFE_ID$SEL_DIST]])
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
    auc <- shinymeta::metaReactive(
      {
        bsafe::area_under_the_curve(
          ae_prop = ..(ae_prop()),
          mix = ..(mix()),
          saf_topic = ..(safety_topic())
        )
      },
      varname = "auc"
    )
    output[[BSAFE_ID$OUT_AREA_UNDER_CURVE]] <- shiny::renderText({
      auc()
    })

    # Table of preset statistical inference statements
    preset_statements <- shinymeta::metaReactive(
      {
        d <- bsafe::preset_stat_table(
          mix = ..(mix()),
          saf_topic = ..(safety_topic()),
          select_analysis = ..(analysis_type())
        )
        names(d) <- "Statement"
        d
      },
      varname = "preset_statements"
    )

    output[[BSAFE_ID$OUT_DM_PRESET_STATEMENTS_TBL]] <- shiny::renderTable({
      preset_statements()
    })

    r <- list(
      header = dm_header,
      preface = dm_preface,
      stat_inf_plot = stat_inf_plot,
      auc = auc,
      preset_statements = preset_statements
    )

    do.call(shiny::exportTestValues, as.list(environment()))

    return(r)
  }

  shiny::moduleServer(id, mod)
}

# TODO: Alexander input on what to expect and what to put in
calc_log_hazard_area <- function(param_approx) {
  val <- c(
    round(param_approx[2, 1] - 2 * param_approx[3, 1], 3),
    round(param_approx[2, 1] + 2 * param_approx[3, 1], 3)
  )
  return(val)
}

# TODO: Alexander input on what to expect and what to put in
calc_param_approx_boundaries <- function(param_approx) {
  lower_bound <- param_approx[2, 1] - param_approx[3, 1]
  upper_bound <- param_approx[2, 1] + param_approx[3, 1]
  return(c(lower_bound, upper_bound))
}

get_preface <- function(analysis_type, safety_topic, distribution) {
  if (analysis_type == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    switch(distribution,
      "Likelihood" = paste0(
        "The likelihood represents information about the proportion of patients with ",
        safety_topic,
        " in the population of the new trial that is contained in the observed data."
      ),
      "MAP Prior" = paste0(""),
      "Robust MAP Prior" = paste0(
        "The robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ",
        safety_topic,
        " in the population of the new trial if they were to receive placebo."
      ),
      "Posterior" = paste0(
        "The posterior distribution represents information about the proportion of patients with ",
        safety_topic,
        " in the population of the new trial after combining the prior (historical data)",
        " and the likelihood (new trial)."
      )
    )
  } else if (analysis_type == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    switch(distribution,
      "Likelihood" = paste0(
        "The log scale of the likelihood represents information about the proportion of patients with ",
        safety_topic,
        " in the population of the new trial that is contained in the observed data."
      ),
      "MAP Prior" = paste0(""),
      "Robust MAP Prior" = paste0(
        "The log scale of the robust MAP prior distribution is our prediction of the true, underlying proportion of patients with ", # nolint: line_length_linter
        safety_topic,
        " in the population of the new trial if they were to receive placebo."
      ),
      "Posterior" = paste0(
        "The log scale of the posterior distribution represents information about the proportion of patients with ",
        safety_topic,
        " in the population of the new trial after combining the prior (historical data) and the likelihood (new trial)." # nolint: line_length_linter
      )
    )
  }
}


mock_decision_making_mod <- function(analysis_type = BSAFE_CHOICES$SEL_ANALYSIS[1]) {
  ui <- function(request) {
    shiny::fluidPage(
      mod_decision_making_ui(
        id = "mock"
      ),
      shiny::verbatimTextOutput("out")
    )
  }

  server <- function(input, output, session) {
    trial_in_metareact <- purrr::imap(teal.modules.bsafe::test_dm_in, function(v, n) {
      shinymeta::metaReactive(
        {
          rlang::quo(teal.modules.bsafe::test_dm_in[[!!n]])
        },
        quoted = TRUE,
        varname = n
      )
    })
    trial_in_metareact[["analysis_type"]] <- shinymeta::metaReactive(
      {
        analysis_type
      },
      varname = "analysis_type"
    )
    x <- do.call(mod_decision_making_server, c(list(id = "mock"), trial_in_metareact))

    output[["out"]] <- shiny::renderPrint({
      utils::str(x)
    })
    do.call(shiny::exportTestValues, as.list(environment()))
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
