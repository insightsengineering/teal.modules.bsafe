# Alexander input on what to expect and what to put in
calc_log_hazard_area <- function(param_approx) {
  val <- c(
    round(param_approx[2, 1] - 2 * param_approx[3, 1], 3),
    round(param_approx[2, 1] + 2 * param_approx[3, 1], 3)
  )
  return(val)
}

# Alexander input on what to expect and what to put in
calc_param_approx_boundaries <- function(param_approx) {
  lower_bound <- param_approx[2, 1] - param_approx[3, 1]
  upper_bound <- param_approx[2, 1] + param_approx[3, 1]
  return(c(lower_bound, upper_bound))
}
# covered
data_saf_topic_char_limiter <- function(data) {
  data[, "SAF_TOPIC"] <- stringr::str_sub(data[, "SAF_TOPIC"], end = 30)
  return(data)
}
# covered
data_reader <- function(filename) {
  data <- utils::read.csv(filename$datapath)
  return(data)
}
# covered
select_dist_selector <- function(sel_analysis, sel_dist, sel_dist_ae) {
  if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    return(sel_dist)
  } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    return(sel_dist_ae)
  }
}
# covered
preface_prior_txt <- function(sel_analysis) {
  if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    return(paste0("Using a MAP approach, the prior approximated as the Beta mixture distribution:"))
  } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    return(paste0("Using a MAP approach, the log scale of the prior approximated as the Normal mixture distribution:"))
  }
}
# covered
preface_rob_txt <- function(sel_analysis, rob_weight, rob_mean) {
  if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[1]) {
    return(paste0(
      "Based on a weakly informative conjugate component with weight ",
      rob_weight, " and mean ", rob_mean,
      " the robust MAP prior is approximated as w * (MAP Prior) + (1 - w) * (weakly informative prior): "
    ))
  } else if (sel_analysis == BSAFE_CHOICES$SEL_ANALYSIS[2]) {
    return(paste0(
      "Based on a EX-NEX approach with non-exchangeable probability P(nex) =  ",
      rob_weight, " and mean ", rob_mean,
      " on the log scale of the robust MAP prior is approximated as (1 - P(nex)) * (MAP Prior) + P(nex) * (weakly informative prior): "
    ))
  }
}


# covered
main_sel_arm_creation_update <- function(data, col_names_to_be_excluded) {
  choices_helper <- data %>%
    dplyr::select(col_names_to_be_excluded) %>%
    colnames()
  return(choices_helper)
}
