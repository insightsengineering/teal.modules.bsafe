BSAFE_ID <- pack_of_constants( # nolint
  # Arm Creation Tab
  SEL_COLUMN = "input_column",
  OUT_SEL_VAR = "input_var",
  BUT_ADD_ARM = "add_arm",
  OUT_ARM_SEL = "arm_info",

  # Upload Data Tab
  FILE_IN = "input_data",
  SEL_TRT = "select_btrt",
  SEL_ANALYSIS = "select_analysis",
  SEL_SAF_TOPIC = "saf_topic",
  SET_SEED = "seed",
  CB_POOLED = "pooled_input",
  OUT_FILE_TABLE = "file_table",

  # MAP Prior Tab
  SEL_TAU = "tau_dist",
  SEL_HIST_BORROW = "hist_borrow",
  SEL_ESS_METHOD = "ess_method",
  BUT_UPDATE_MAP = "update_map",
  DIV_INCI = "inci",
  OUT_FOREST_PLT = "forest",
  DIV_AE = "ae",
  OUT_PREFACE_PRIOR_TXT = "pref_p_approx_pdf",
  OUT_DENSITY_FCT = "p_approx",
  OUT_MIX_DENSITY_PLT = "param_hist2",
  OUT_MAP_PRIOR_SUM_TBL = "summary",

  # Robust MAP Prior
  SLDR_ROB_WEIGHT = "robust_weight",
  DIV_ROB_MEAN = "robust_mean",
  SLDR_ROB_MEAN = "robust_mean",
  SEL_ROB_ESS_METHOD = "rob_ess_method",
  BUT_UPDATE_ROB = "update_rob",
  OUT_PREFACE_ROB_TXT = "preface_robust_map",
  OUT_ROB_DENSITY_FCT = "robust_map",
  OUT_ROB_MAP_PLT = "robust_map_plot",
  OUT_ROB_SUM_TBL = "rob_map_sumstats",

  # New Trial Analysis Tab
  DIV_NTA_INCI = "nta_inci",
  SLDR_N_PAT = "new_n",
  SLDR_N_AE = "new_r",
  DIV_NTA_AE = "nta_er",
  SLDR_AE_FIRST_OCCURENCE = "new_events",
  SLDR_CUMM_TIME_FIRST_AE = "new_exposed_t",
  BUT_UPDATE_PRIOR = "update_prior",
  DIV_NTA_INCI_MAIN = "nta_inci_main",
  DIV_NTA_AE_MAIN = "nta_er_main",
  OUT_COMPARE_PLT = "d_compare",
  OUT_COMPARE_SUM_TBL = "d_comp_sumstats",

  # Decision Making Tab
  DIV_DM_INCI = "dm_inci",
  SEL_DIST = "select_dist",
  OUT_PERC_SLDR = "ae_perc_slider",
  DIV_DM_AE = "dm_er",
  SEL_DIST_AE = "select_dist_er",
  OUT_AE_PERC_SLDR = "ae_perc_slider_dm",
  BUT_UPDATE_STAT_INF = "update_stat_inf",
  OUT_DM_HEADER_TXT = "pdf_title",
  OUT_DM_PREFACE_TXT = "interpret",
  OUT_STAT_INF_DENSITY_PLT = "stat_inf_density",
  OUT_AREA_UNDER_CURVE = "stat_inf",
  OUT_DM_PRESET_STATEMENTS_TBL = "preset_inf",

  # Download Results Tab
  SLDR_NUM_COMP = "numb_comp",
  OUT_COMP_CB = "checkbox",
  BUT_COMP_SUBMIT = "submit_compare",
  OUT_DWNLD_PLTS = "download_plots",
  BUT_DWNLD_SUM_TBLS = "download_all_results",
  BUT_DWNLD_EXCEL = "download_excel",
  BUT_DWNLD_LOG = "download_log",
  OUT_EXCEL_PATH_TXT = "excel_path",
  OUT_COMP_DISPLAY = "comp_display",
  OUT_TXT_SELECTED_ANALYSIS = "selected_analysis"


)

BSAFE_CHOICES <- pack_of_constants( # noLint
  SEL_ANALYSIS = c("Incidence proportion", "Exposure-adjusted AE rate"),
  SEL_HIST_BORROW = c("Small", "Moderate", "Substantial", "Large",
                      "Very Large"),
  SEL_TAU = c("Half-normal" = "HalfNormal"),
  SEL_ESS_METHOD = c("Expected Local Information Ratio" = "elir"),
  SEL_DIST = c("Likelihood", "MAP Prior", "Robust MAP Prior", "Posterior")
)

BSAFE_DEFAULTS <- pack_of_constants( # noLint
  SEL_TAU = "HalfNormal",
  SEL_HIST_BORROW = "Large",
  SEL_ESS_METHOD = "elir",
  SEL_DIST = "Posterior"
)


REPORT_IDS <- pack_of_constants(
  DEFAULT = pack_of_constants(
    ADD = "add_report_card",
    DOWNLOAD = "download_button",
    RESET = "reset_button"
  )
)

REPORT_IDS[["MAP"]] <- do.call(pack_of_constants, purrr::map(REPORT_IDS$DEFAULT, ~paste0("map_", .x)))
REPORT_IDS[["ROBUST_MAP"]] <- do.call(pack_of_constants, purrr::map(REPORT_IDS$DEFAULT, ~paste0("robust_map_", .x)))

