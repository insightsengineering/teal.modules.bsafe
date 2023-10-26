set.seed(8)

n_studies <- 30
hist <- c(1, 0)
dose_list <- c(10, 20, 30, NA)
freq_list <- c(1, 2, NA)
length_list <- c(100, 200, 300, 400)
trt_list <- c("Placebo", "Treatment")
arm_list <- c("Placebo", "Placebo with XX", "Treatment", "Treatment with XX")
ae_list <- c("DILI", "Nausea", "Vomitting")



bsafe_data <- tibble::tibble(
  STUDYID = factor(1:n_studies)
) %>%
  dplyr::mutate(
    HIST = sample(hist, size = n_studies, replace = TRUE),
    DOSE = sample(dose_list, size = n_studies, replace = TRUE),
    FREQ = sample(freq_list, size = n_studies, replace = TRUE),
    LENGTH = sample(length_list, size = n_studies, replace = TRUE),
    TREAT = sample(trt_list, size = n_studies, replace = TRUE),
    ARM = sample(arm_list, size = n_studies, replace = TRUE),
    N = sample(1:200, size = n_studies, replace = TRUE),
    SAF_TOPIC = sample(ae_list, size = n_studies, replace = TRUE),
    N_WITH_AE = sample(1:80, size = n_studies, replace = TRUE),
    TOT_EXP = sample(100:300, size = n_studies, replace = TRUE),
    STUDY_ARM = paste(
      STUDYID, ARM
    ),
  )

usethis::use_data(bsafe_data, overwrite = TRUE)
