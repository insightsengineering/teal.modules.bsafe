#' @title quic.bsafe's ui function
#' @description  implements the UI for the quic.bsafe shiny app
#'
#' @param id the id
#' @param header elements to be included in the header of the tabset of the module
#'
#' @return the UI
#' @export
bsafe_UI <- function(id, header = NULL) { # nolint

  bs3_panel <- function(...) {
    shiny::div(
      class = "panel panel-default",
      shiny::div(
        class = "panel-body",
        ...
      )
    )
  }

  bars_check <- function(id, label, value = TRUE) {
    bc <- shiny::checkboxInput(id, shiny::tagList(shiny::span(class = "chevron"), label), value = value)
    bc[["attribs"]][["style"]] <- "margin-bottom: 0px"
    bc[["children"]][[1]][["attribs"]][["style"]] <- "margin-bottom: 0px; margin-top: 0px"
    bc[["children"]][[1]][["attribs"]][["class"]] <- c(bc[["children"]][[1]][["attribs"]][["class"]], "collapse_menu")
    bc
  }

  collapsible_panel <- function(id, label, ..., open = FALSE) {
    bs3_panel(
      bars_check(id, label, value = open),
      shiny::conditionalPanel(
        condition = paste0("input['", id, "']"),
        shiny::hr(style = "margin-top: 5px"),
        ...
      )
    )
  }

  ns <- shiny::NS(id)



  ui_list <- list(
    prep = list(mod_data_preparation_ui(ns("data_preparation")), "Data Preparation", FALSE),
    a_sel = list(mod_select_analysis_ui(ns("sel_analysis")), "Analysis selection", FALSE),
    mp = list(mod_map_prior_ui(ns("map_prior")), "MAP Prior", FALSE),
    rmp = list(mod_robust_map_ui(ns("robust_map")), "Robust MAP Prior", FALSE),
    nta = list(mod_new_trial_analysis_ui(ns("new_trial")), "New Trial Analysis", FALSE),
    dm = list(mod_decision_making_ui(ns("decision_making")), "Decision Making", FALSE),
    down = list(mod_simulation_ui(ns("simulation")), "Default Comparisons", FALSE)
  )

  # ui_list[["mp"]][[1]][["main"]] <- shiny::div(
  #   style = "display: grid; grid-template-columns: 1fr 1fr; grid-template-rows:auto auto; grid-gap: 1rem; width = 100%; max-width: 100%; min-width: 0", # nolint
  #   shiny::div(ui_list[["mp"]][[1]][["main"]][1:3], style = "grid-column: 1; grid-row:1; max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["mp"]][[1]][["main"]][4:7], style = "grid-column: 2; grid-row:1; max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["mp"]][[1]][["main"]][8], style = "grid-column: 1 / span 2; grid-row:2; max-width: 100%; min-width: 0") # nolint
  # )
  #
  # ui_list[["rmp"]][[1]][["main"]] <- shiny::div(
  #   style = "display: grid; grid-template-columns: 1.2fr 0.8fr; grid-template-rows:auto auto; grid-gap: 1rem; width = 100%; max-width: 100%; min-width: 0", # nolint
  #   shiny::div(ui_list[["rmp"]][[1]][["main"]][1:2], style = "grid-column: 1 / span 2; grid-row:1;max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["rmp"]][[1]][["main"]][3], style = "grid-column: 1; grid-row:2;max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["rmp"]][[1]][["main"]][4], style = "grid-column: 2; grid-row:2;max-width: 100%; min-width: 0") # nolint
  # )
  #
  # ui_list[["nta"]][[1]][["main"]] <- shiny::div(
  #   style = "display: grid; grid-template-columns: 1.2fr 0.8fr; grid-template-rows:auto auto; grid-gap: 1rem; width = 100%; max-width: 100%", # nolint
  #   shiny::div(ui_list[["nta"]][[1]][["main"]][1:2], style = "grid-column: 1 / span 2; grid-row:1;max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["nta"]][[1]][["main"]][3], style = "grid-column: 1; grid-row:2;max-width: 100%; min-width: 0"), # nolint
  #   shiny::div(ui_list[["nta"]][[1]][["main"]][4], style = "grid-column: 2; grid-row:2;max-width: 100%; min-width: 0") # nolint
  # )
  #
  # ui_list[["dm"]][[1]][["main"]] <- shiny::div(
  #   style = "display: grid; grid-template-columns: 1fr 1fr; grid-template-rows:auto auto; grid-gap: 1rem; width = 100%; max-width: 100%; min-width: 0", # nolint
  #   shiny::div(ui_list[["dm"]][[1]][["main"]][1:2], style = "grid-column: 1 / span 2; grid-row:1; min-width: 0"), # nolint
  #   shiny::div(ui_list[["dm"]][[1]][["main"]][3:4], style = "grid-column: 1; grid-row:2; min-width: 0"), # nolint
  #   shiny::div(ui_list[["dm"]][[1]][["main"]][5], style = "grid-column: 2; grid-row:2; min-width: 0") # nolint
  # )

  side <- shiny::tagList(
    header,
    purrr::imap(
      ui_list,
      function(v, n) {
        collapsible_panel(
          id = ns(paste0(n, "_side_check")),
          label = v[[2]],
          v[[1]][["side"]]
        )
      }
    )
  )

  main <- purrr::imap(
    ui_list,
    function(v, n) {
      collapsible_panel(
        id = ns(paste0(n, "_main_check")),
        label = v[[2]],
        v[[1]][["main"]],
        open = v[[3]]
      )
    }
  )

  manual_ui <- collapsible_panel(
    id = ns("manual"),
    open = FALSE,
    label = "Getting Started",
    shiny::includeMarkdown(system.file("gettingStarted_bsafe.Rmd",
      package = "teal.modules.bsafe",
      mustWork = TRUE
    )),
    shiny::h5("User Manual:"),
    shiny::a(
      "open SAP",
      href = "www/manual.pdf",
      target = "_blank"
    ),
    shiny::h5("Statistical Analysis Plan:"),
    shiny::a(
      "open SAP",
      href = "www/statsplan.pdf",
      target = "_blank"
    ),
    shiny::h5("Testing Documentation:"),
    shiny::a(
      "open Testing Documentation",
      href = "www/test_validate_review.pdf",
      target = "_blank"
    )
  )
  main <- list(manual_ui, main)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::includeCSS(system.file("www/bsafe.css", mustWork = TRUE, package = "teal.modules.bsafe")),
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(side),
      mainPanel = shiny::mainPanel(main)
    )
  )
}


bsafe_server <- function(
    id,
    dataset) {
  module <- function(input, output, session) {

    shiny::addResourcePath("www", system.file("www", package = "teal.modules.bsafe"))


    # global variables --------------------------------------------------------

    ns <- session[["ns"]]

    ae_summary_data <- NULL

    # data input/checks/transformation ----------------------------------------
    receive_data <- shinymeta::metaReactive2(
      {
        data <- dataset()

        shiny::validate(
          shiny::need(
            checkmate::test_numeric(data[["DOSE"]], null.ok = TRUE),
            "Dose needs to be a number"
          ),
          shiny::need(
            checkmate::test_integer(data[["FREQ"]], null.ok = TRUE),
            "Freq needts to be an integer"
          ),
          shiny::need(
            checkmate::test_numeric(data[["LENGTH"]], null.ok = TRUE),
            "Length needts to be an number"
          ),
          shiny::need(
            checkmate::test_character(data[["STUDYID"]]),
            "STUDYID needs to be a character"
          ),
          shiny::need(
            checkmate::test_numeric(data[["N"]], null.ok = FALSE, any.missing = FALSE),
            "N needs to be a number"
          ),
          shiny::need(
            checkmate::test_numeric(data[["N_WITH_AE"]], null.ok = FALSE, any.missing = FALSE),
            "N_with_AE needs to be a number"
          ),
          shiny::need(
            checkmate::test_numeric(data[["TOT_EXP"]]),
            "TOT_EXP needs to be a number"
          ),
          shiny::need(
            checkmate::test_character(data[["SAF_TOPIC"]]),
            "SAF_TOPIC needs to be a character"
          ),
          shiny::need(
            checkmate::test_character(data[["ARM"]]),
            "ARM needs to be a character"
          ),
          shiny::need(
            checkmate::test_true(all(data[["N_WITH_AE"]] <= data[["N"]])),
            "N_WITH_AE_COLUMN must me lower or equal than N"
          )
        )

        shinymeta::metaExpr({
          ..(dataset())
          data_saf_topic_char_limiter(..(dataset()))
        })
      },
      varname = "receive_data"
    )

    data_preparation <- mod_data_preparation_server(
      "data_preparation",
      data = receive_data
    )

    # Data table preparation
    sel_analysis <- mod_select_analysis_server("sel_analysis", data_preparation)
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
      new_trial_analysis = new_trial[["new_trial_analysis"]],
      seed = sel_analysis[["seed"]]
    )

    # nolint start

    tmpfolder <- tempfile(tmpdir = system.file("www", package = "teal.modules.bsafe"))
    dir.create(tmpfolder)

    download_results <- mod_simulation_server(
      "simulation",
      data = data_preparation,
      tmpfolder = tmpfolder
    )

    shiny::onStop(function() {
      unlink(
        system.file(paste0("/www", strsplit(tmpfolder, "www")[[1]][2]),
          package = "teal.modules.bsafe",
          mustWork = TRUE
        ),
        recursive = TRUE
      )
    })

    # nolint end


    # return ----

    to_report <- shiny::reactive({
      code <- local({
        ec <- shinymeta::newExpansionContext()
        ec$substituteMetaReactive(receive_data, function() {
          shinymeta::metaExpr(attr(dataset(), "code"), quoted = TRUE)
        })
        shinymeta::expandChain(
          "# Data loading",
          receive_data(),
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
          decision_making[["preset_statements"]](),
          .expansionContext = ec
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

data_saf_topic_char_limiter <- function(data) {
  data[["SAF_TOPIC"]] <- stringr::str_sub(data[["SAF_TOPIC"]], end = 30)
  return(data)
}
