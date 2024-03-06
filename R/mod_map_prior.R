map_prior_ui <- function(id){
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
            shiny::selectInput(BSAFE_ID$SEL_ESS_METHOD,
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

map_prior_server <- function(id, data, analysis_type) {

    mod <- function(input, output, session) {
    adj_tau <- shinymeta::metaReactive2({
      shiny::req(input[[BSAFE_ID$BUT_UPDATE_MAP]])
      shiny::isolate(shinymeta::metaExpr({
        bsafe::tau_adjust(
          select_analysis = ..(analysis_type()),
          hist_borrow = ..(input[[BSAFE_ID$SEL_HIST_BORROW]])
        )
      }))
    })

    

    }

    shiny::moduleServer(id, mod)        
}