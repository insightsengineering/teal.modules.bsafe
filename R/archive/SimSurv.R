# function to simulate 1 Study
# nPat = Number of patients in each group
# g1 = group 1 (treatment) and g2 = group 2 (control)
# dropout = 0.05 translates to a 5% dropout after time units of measure
# accr = accrual time, is to be in rgards to the hazard,
# 6/12 reflects 6 month of accrual time
# nObsEvt = type 2 censoring, censor after nObsEvt number of observed events
sim_1_study <- function(n_pat = c(g1 = 100, g2 = 100),
                        hz = c(g1 = 0.1, g2 = 0.2),
                        dropout = c(rate = 0.05, time = 12),
                        accr = (6 / 12),
                        n_obs_evt = 0.5) {
  n <- sum(n_pat)

  if (n_obs_evt < 1) {
    n_obs_evt <- sum(n_pat) * n_obs_evt
  }

  # res will be the result output
  res <- matrix(
    data = NA, nrow = n, ncol = 7,
    dimnames = list(
      ID = c(1:n),
      c(
        "gID", "ID", "Entry", "EventTime", "ObsTime",
        "CensorTime", "EventIndicator"
      )
    )
  )
  res[, "ID"] <- 1:n
  res[, "gID"] <- c(rep(1, n_pat["g1"]), rep(2, n_pat["g2"]))
  res[, "Entry"] <- stats::runif(n, 0, accr)

  for (i in seq_along(n_pat)) {
    surv_times_g <- stats::rexp(n_pat[i], hz[i])

    if (i == 1) {
      surv_times <- surv_times_g
    } else {
      surv_times <- c(surv_times, surv_times_g)
    }
  }

  # Eventtimes
  res[, "EventTime"] <- res[, "Entry"] + surv_times

  # get rate parameter for exponential distributed censoring times
  # if rate=0, no censoring is applied
  cens_rate <- if (dropout["rate"] > 0) {
    -log(1 - dropout["rate"]) / dropout["time"]
  } else {
    0
  }

  # censoring times for all individuals, infinity if no censoring is applied
  cens_time <- if (dropout["rate"] > 0) {
    stats::rexp(n, cens_rate)
  } else {
    rep(Inf, n)
  }

  res[, "CensorTime"] <- cens_time + res[, "Entry"]


  # introduce Censoring indices
  evt_ind <- which(res[, "EventTime"] < res[, "CensorTime"])
  n_evt_ind <- which(res[, "EventTime"] >= res[, "CensorTime"])
  res[evt_ind, "EventIndicator"] <- 1
  res[n_evt_ind, "EventIndicator"] <- 0
  res[evt_ind, "ObsTime"] <- res[evt_ind, "EventTime"]
  res[n_evt_ind, "ObsTime"] <- res[n_evt_ind, "CensorTime"]


  if (n_obs_evt < Inf) {
    type_2_cens_time <- sort(res[evt_ind, "ObsTime"])[n_obs_evt]
    type_2_cens_t_ind <- which(res[, "ObsTime"] <= type_2_cens_time)
    res[type_2_cens_t_ind, "ObsTime"] <- type_2_cens_time
    res[type_2_cens_t_ind, "EventIndicator"] <- 0
  }
  return(res)
}

# nSets: number of settings to test
n_sets <- 2
default_array <- array(
  data = NA,
  dim = c(n_sets, 2, 2),
  dimnames = list(
    c(paste0("set", 1:n_sets)),
    c("g1", "g2"), c("nPat", "hz")
  )
)


# -> testing treshold  says mean: 0.15 and 0.25, CrI 0.01 and 0.99
default_array[1, , ] <- matrix(data = c(100, 100, 0.1, 0.2))
default_array[1, , ]

# -> testing treshold: mean: 0.1 and 0.5
default_array[2, , ] <- matrix(data = c(50, 35, 0.3, 0.3))
default_array[2, , ]


## function to get data to test BSAFE
# nStud: Number of studies: nStud-1 will be historical
sim_test_data <- function(n_stud = 5, tau = 0.05,
                          set_array = default_array,
                          dropout = c(rate = 0.02, time = 12),
                          accr = (6 / 12),
                          n_obs_evt = 0.5,
                          noise = 0,
                          keep_stud = FALSE) {
  n_sets <- dim(set_array)[1]
  res <- array(
    data = NA, dim = c(n_sets, n_stud + 1, 5),
    dimnames = list(
      set = c(paste0("set", 1:n_sets)),
      STUDYID = c(1:n_stud, n_stud),
      c("group", "HIST", "N", "N_WITH_AE", "TOT_EXP")
    )
  )



  for (i in 1:n_sets) {
    res[i, , "HIST"] <- c(rep(1, n_stud - 1), 0, 0)
    res[i, , "group"] <- c(rep(1, n_stud), 2)


    for (h in 1:n_stud) {
      if (noise > 0) {
        temp_hz <- exp(log(set_array[i, , "hz"]) + stats::rnorm(2, 0, tau))
      } else {
        temp_hz <- exp(log(set_array[i, , "hz"]))
      }

      temp_study <- sim_1_study(
        n_pat = set_array[i, , "nPat"],
        hz = temp_hz,
        dropout = dropout,
        accr = accr,
        n_obs_evt = n_obs_evt
      )

      g1_ind <- which(temp_study[, "gID"] == 1)
      g2_ind <- which(temp_study[, "gID"] == 2)

      temp_study_g1 <- temp_study[g1_ind, ]
      temp_study_g2 <- temp_study[g2_ind, ]

      res[i, h, "N"] <- set_array[i, "g2", "nPat"]
      res[i, h, "N_WITH_AE"] <- sum(temp_study_g2[, "EventIndicator"])
      res[i, h, "TOT_EXP"] <- sum(temp_study_g2[, "ObsTime"] - temp_study_g2[, "Entry"])
      if (h == n_stud) {
        res[i, h + 1, "N"] <- set_array[i, "g1", "nPat"]
        res[i, h + 1, "N_WITH_AE"] <- sum(temp_study_g1[, "EventIndicator"])
        res[i, h + 1, "TOT_EXP"] <- sum(temp_study_g1[, "ObsTime"] - temp_study_g1[, "Entry"])
      }
    }
  }


  res_fin <- as.data.frame(res[1, , ])
  res_fin$STUDYID <- c(1:n_stud, n_stud)
  res_fin$SAF_TOPIC <- "Set1"

  if (n_sets > 1) {
    for (i in 2:n_sets) {
      res_wip <- as.data.frame(res[i, , ])
      res_wip$STUDYID <- c(1:n_stud, n_stud)
      res_wip$SAF_TOPIC <- paste0("Set", i)
      res_fin <- rbind(res_fin, res_wip)
    }
  }

  res_fin$ARM <- res_fin$SAF_TOPIC
  return(res_fin)
}




### Just do zz <- simTestData()
### and then call zz[1,,] and you have 1 Dataset to test

### You can start with a test for the mean of the mcmc samples
### they should be around the hz["g2"]
### that is specified in the setArray (settings Array)

zz <- sim_test_data()
