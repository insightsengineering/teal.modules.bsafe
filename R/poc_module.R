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

  as_sb_layout <- function(l){shiny::sidebarLayout(sidebarPanel = shiny::sidebarPanel(l[["side"]]), mainPanel = shiny::mainPanel(l[["main"]]))}

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
      # shiny::tabPanel(
      #   "Data preparation",
      #   mod_data_preparation_ui(ns("data_preparation"))
      # ),
      shiny::tabPanel(
        "Select Analysis",
        mod_select_analysis_ui(ns("sel_analysis")) |> as_sb_layout()
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
        mod_decision_making_ui(ns("decision_making"))
      ),
      # shiny::tabPanel(
      #   "Download Results",
      #   mod_simulation_ui(ns("simulation"))
      # )
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


    # data input/checks/transformation ----------------------------------------
    receive_data <- shinymeta::metaReactive({
      # Here is where we should include the dataset calculation steps
      ..(dataset())
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

    decision_making <- mod_decision_making_server(
      "decision_making",
      data = selected_data,
      analysis_type = sel_analysis[["analysis_type"]],
      safety_topic = sel_analysis[["safety_topic"]],
      treatment = sel_analysis[["treatment"]],
      robust_map_mcmc = robust_map[["robust_map_mcmc"]],
      param_approx = map_prior[["param_approx"]],
      current_trial_data = new_trial[["current_trial_data"]],
      post_dist = new_trial[["post_dist"]],
      new_trial_analysis = new_trial[["new_trial_analysis"]]
    )

    decision_making <- mod_decision_making_server(
      "decision_making",
      data = selected_data,
      analysis_type = sel_analysis[["analysis_type"]],
      safety_topic = sel_analysis[["safety_topic"]],
      treatment = sel_analysis[["treatment"]],
      robust_map_mcmc = robust_map[["robust_map_mcmc"]],
      param_approx = map_prior[["param_approx"]],
      current_trial_data = new_trial[["current_trial_data"]],
      post_dist = new_trial[["post_dist"]],
      new_trial_analysis = new_trial[["new_trial_analysis"]]
    )

    # data_preparation <- mod_data_preparation_server(
    #   "data_preparation",
    #   data = receive_data
    # )

    # download_results <- mod_simulation_server(
    #   "simulation",
    #   data = receive_data
    # )

#    shiny::observe({
#    new_trial[["current_trial_data"]]()
#    browser()
#  })

#   nt <- list(
#       data = selected_data(),
#         analysis_type = sel_analysis[["analysis_type"]](),
#         safety_topic = sel_analysis[["safety_topic"]](),
#         treatment = sel_analysis[["treatment"]](),
#         robust_map_mcmc = robust_map[["robust_map_mcmc"]](),
#         param_approx = map_prior[["param_approx"]](),
#         current_trial_data = new_trial[["current_trial_data"]](),
#         post_dist = new_trial[["post_dist"]](),
#         new_trial_analysis = new_trial[["new_trial_analysis"]]()
#     )
#   saveRDS(nt, "nt.rds")

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
        new_trial[["compare_summary_table"]](),
        "# Decision Making",
        "## Plot",
        decision_making[["stat_inf_plot"]](),
        "## Inference table",
        decision_making[["preset_statements"]]()
      )
      })

      list(
        code = code,
        forest_plot = map_prior[["forest_plot"]](),
        map_summary_table = map_prior[["map_summary_table"]](),
        robust_plot = robust_map[["robust_plot"]](),
        robust_summary = robust_map[["robust_summary"]](),
        compare_plot = new_trial[["compare_plot"]](),
        compare_summary_table = new_trial[["compare_summary_table"]](),
        dm_header = decision_making[["header"]](),
        dm_preface = decision_making[["preface"]](),
        stat_inf_plot = decision_making[["stat_inf_plot"]](),
        auc = decision_making[["auc"]](),
        preset_statements = decision_making[["preset_statements"]]()
      )
    })
    return(to_report)
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}

