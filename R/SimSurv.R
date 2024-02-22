# function to simulate 1 Study
# nPat = Number of patients in each group
# g1 = group 1 (treatment) and g2 = group 2 (control)
# dropout = 0.05 translates to a 5% dropout after time units of measure
# accr = accrual time, is to be in rgards to the hazard,
# 6/12 reflects 6 month of accrual time
# nObsEvt = type 2 censoring, censor after nObsEvt number of observed events
sim1Study <- function(nPat = c(g1 = 100, g2 = 100),
                      hz = c(g1 = 0.1, g2 = 0.2),
                      dropout = c(rate = 0.05, time = 12),
                      accr = (6 / 12),
                      nObsEvt = 0.5) {
  N <- sum(nPat)

  if (nObsEvt < 1) {
    nObsEvt <- sum(nPat) * nObsEvt
  }

  # res will be the result output
  res <- matrix(
    data = NA, nrow = N, ncol = 7,
    dimnames = list(
      ID = c(1:N),
      c(
        "gID", "ID", "Entry", "EventTime", "ObsTime",
        "CensorTime", "EventIndicator"
      )
    )
  )
  res[, "ID"] <- 1:N
  res[, "gID"] <- c(rep(1, nPat["g1"]), rep(2, nPat["g2"]))
  res[, "Entry"] <- runif(N, 0, accr)

  for (i in 1:length(nPat)) {
    survTimesG <- rexp(nPat[i], hz[i])

    if (i == 1) {
      survTimes <- survTimesG
    } else {
      survTimes <- c(survTimes, survTimesG)
    }
  }

  # Eventtimes
  res[, "EventTime"] <- res[, "Entry"] + survTimes

  # get rate parameter for exponential distributed censoring times
  # if rate=0, no censoring is applied
  censRate <- if (dropout["rate"] > 0) {
    -log(1 - dropout["rate"]) / dropout["time"]
  } else {
    0
  }

  # censoring times for all individuals, infinity if no censoring is applied
  censTime <- if (dropout["rate"] > 0) {
    rexp(N, censRate)
  } else {
    rep(Inf, N)
  }

  res[, "CensorTime"] <- censTime + res[, "Entry"]


  # introduce Censoring indices
  evtInd <- which(res[, "EventTime"] < res[, "CensorTime"])
  nEvtInd <- which(res[, "EventTime"] >= res[, "CensorTime"])
  res[evtInd, "EventIndicator"] <- 1
  res[nEvtInd, "EventIndicator"] <- 0
  res[evtInd, "ObsTime"] <- res[evtInd, "EventTime"]
  res[nEvtInd, "ObsTime"] <- res[nEvtInd, "CensorTime"]


  if (nObsEvt < Inf) {
    type2CensTime <- sort(res[evtInd, "ObsTime"])[nObsEvt]
    type2CensTInd <- which(res[, "ObsTime"] <= type2CensTime)
    res[type2CensTInd, "ObsTime"] <- type2CensTime
    res[type2CensTInd, "EventIndicator"] <- 0
  }
  return(res)
}

# nSets: number of settings to test
nSets <- 2
defaultArray <- array(data = NA, dim = c(nSets, 2, 2), dimnames = list(c(paste0("set", 1:nSets)), c("g1", "g2"), c("nPat", "hz")))


defaultArray[1, , ] <- matrix(data = c(100, 100, 0.1, 0.2)) # -> testing treshold  says mean: 0.15 and 0.25, CrI 0.01 and 0.99
defaultArray[1, , ]

defaultArray[2, , ] <- matrix(data = c(50, 35, 0.3, 0.3)) # -> testing treshold: mean: 0.1 and 0.5
defaultArray[2, , ]
#
# defaultArray[3,,] <- matrix(data = c(10000,10000,0.05,0.5)) # -> testing treshold: mean: 0.4
# defaultArray[3,,]
#
# defaultArray[4,,] <- matrix(data = c(10000,10000,0.2,0.2)) # -> testing treshold: mean: 0.4
# defaultArray[4,,]


## function to get data to test BSAFE
# nStud: Number of studies: nStud-1 will be historical
simTestData <- function(nStud = 5, tau = 0.05,
                        setArray = defaultArray,
                        dropout = c(rate = 0.02, time = 12),
                        accr = (6 / 12),
                        nObsEvt = 0.5,
                        noise = 0,
                        keepStud = FALSE) {
  nSets <- dim(setArray)[1]
  res <- array(
    data = NA, dim = c(nSets, nStud + 1, 5),
    dimnames = list(
      set = c(paste0("set", 1:nSets)),
      STUDYID = c(1:nStud, nStud),
      c("group", "HIST", "N", "N_WITH_AE", "TOT_EXP")
    )
  )



  for (i in 1:nSets) {
    res[i, , "HIST"] <- c(rep(1, nStud - 1), 0, 0)
    res[i, , "group"] <- c(rep(1, nStud), 2)


    for (h in 1:nStud) {
      if (noise > 0) {
        tempHz <- exp(log(setArray[i, , "hz"]) + rnorm(2, 0, tau))
      } else {
        tempHz <- exp(log(setArray[i, , "hz"]))
      }

      tempStudy <- sim1Study(
        nPat = setArray[i, , "nPat"],
        hz = tempHz,
        dropout = dropout,
        accr = accr,
        nObsEvt = nObsEvt
      )

      g1Ind <- which(tempStudy[, "gID"] == 1)
      g2Ind <- which(tempStudy[, "gID"] == 2)

      tempStudyG1 <- tempStudy[g1Ind, ]
      tempStudyG2 <- tempStudy[g2Ind, ]

      res[i, h, "N"] <- setArray[i, "g2", "nPat"]
      res[i, h, "N_WITH_AE"] <- sum(tempStudyG2[, "EventIndicator"])
      res[i, h, "TOT_EXP"] <- sum(tempStudyG2[, "ObsTime"]
      - tempStudyG2[, "Entry"])
      if (h == nStud) {
        res[i, h + 1, "N"] <- setArray[i, "g1", "nPat"]
        res[i, h + 1, "N_WITH_AE"] <- sum(tempStudyG1[, "EventIndicator"])
        res[i, h + 1, "TOT_EXP"] <- sum(tempStudyG1[, "ObsTime"]
        - tempStudyG1[, "Entry"])
      }
    }
  }


  res_fin <- as.data.frame(res[1, , ])
  res_fin$STUDYID <- c(1:nStud, nStud)
  res_fin$SAF_TOPIC <- "Set1"

  if (nSets > 1) {
    for (i in 2:nSets) {
      res_wip <- as.data.frame(res[i, , ])
      res_wip$STUDYID <- c(1:nStud, nStud)
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

zz <- simTestData()
