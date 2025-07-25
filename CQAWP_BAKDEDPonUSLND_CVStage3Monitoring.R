CQAWP_BAKDEDPonUSLND_CVStage3Monitoring <- function(
    data1, Residue1, USL1, Fiscal_Year1,
    data2 = NULL, Residue2 = NULL, USL2 = NULL, Fiscal_Year2 = NULL,
    data3 = NULL, Residue3 = NULL, USL3 = NULL, Fiscal_Year3 = NULL,
    Time_cut,
    CIL = NULL,
    BW = "Silver1.06",
    initial_boot = 1000,
    max_boot = 10000,
    conf_level = 0.95,
    seed = NULL
) {
  library(dplyr)
  if (!is.null(seed)) set.seed(seed)
  clean_na_residue <- function(data, residue_col) {
    data %>% filter(!is.na({{ residue_col }}))
  }
  split_by_fiscal_year <- function(data, fiscal_year_col, time_cut) {
    training <- data %>% 
      filter(!is.na({{ fiscal_year_col }})) %>%
      filter({{ fiscal_year_col }} < time_cut)
    
    testing <- data %>% 
      filter(!is.na({{ fiscal_year_col }})) %>%
      filter({{ fiscal_year_col }} >= time_cut)
    
    list(training = training, testing = testing)
  }
  split1 <- split_by_fiscal_year(data1, {{ Fiscal_Year1 }}, Time_cut)
  TrainingData1 <- clean_na_residue(split1$training, {{ Residue1 }})
  TestingData1 <- clean_na_residue(split1$testing, {{ Residue1 }})
  
  if (!is.null(data2)) {
    split2 <- split_by_fiscal_year(data2, {{ Fiscal_Year2 }}, Time_cut)
    TrainingData2 <- clean_na_residue(split2$training, {{ Residue2 }})
    TestingData2 <- clean_na_residue(split2$testing, {{ Residue2 }})
  } else {
    TrainingData2 <- NULL
    TestingData2 <- NULL
  }
  if (!is.null(data3)) {
    split3 <- split_by_fiscal_year(data3, {{ Fiscal_Year3 }}, Time_cut)
    TrainingData3 <- clean_na_residue(split3$training, {{ Residue3 }})
    TestingData3 <- clean_na_residue(split3$testing, {{ Residue3 }})
  } else {
    TrainingData3 <- NULL
    TestingData3 <- NULL
  }
  if (is.null(CIL) || CIL < 1) {
    ppu_ci_obj <- Ppu_CQAWP_BAKDEDPonUSLND(
      data1 = TrainingData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
      data2 = TrainingData2, Residue2 = {{ Residue2 }}, USL2 = {{ USL2 }},
      data3 = TrainingData3, Residue3 = {{ Residue3 }}, USL3 = {{ USL3 }},
      BW = BW,
      n_boot = initial_boot,
      conf_level = conf_level,
      seed = seed
    )
    if (ppu_ci_obj$CI_lower < 1) {
      message("Initial CI lower bound < 1, increasing bootstrap to ", max_boot)
      ppu_ci_obj <- Ppu_CQAWP_BAKDEDPonUSLND(
        data1 = TrainingData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
        data2 = TrainingData2, Residue2 = {{ Residue2 }}, USL2 = {{ USL2 }},
        data3 = TrainingData3, Residue3 = {{ Residue3 }}, USL3 = {{ USL3 }},
        BW = BW,
        n_boot = max_boot,
        conf_level = conf_level,
        seed = seed
      )
      if (ppu_ci_obj$CI_lower < 1) {
        warning("CIL still < 1 after ", max_boot, " bootstraps. Data quality or size may be insufficient.")
      }
    }
    CIL <- ppu_ci_obj$CI_lower
  } else {
    training_ppu <- Ppu_CQAWP_KDEDPonUSLND(
      data1 = TrainingData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
      data2 = TrainingData2, Residue2 = {{ Residue2 }}, USL2 = {{ USL2 }},
      data3 = TrainingData3, Residue3 = {{ Residue3 }}, USL3 = {{ USL3 }},
      BW = BW
    )$Ppu
    ppu_ci_obj <- list(
      Ppu = training_ppu,
      CI_lower = CIL
    )
  }
  CombinedData1 <- bind_rows(TrainingData1, TestingData1)
  CombinedData2 <- if (!is.null(TrainingData2)) bind_rows(TrainingData2, TestingData2) else NULL
  CombinedData3 <- if (!is.null(TrainingData3)) bind_rows(TrainingData3, TestingData3) else NULL
  testing_ppu_result <- Ppu_CQAWP_KDEDPonUSLND(
    data1 = CombinedData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
    data2 = CombinedData2, Residue2 = {{ Residue2 }}, USL2 = {{ USL2 }},
    data3 = CombinedData3, Residue3 = {{ Residue3 }}, USL3 = {{ USL3 }},
    BW = BW
  )
  Testing_Ppu <- testing_ppu_result$Ppu
  decision <- if (Testing_Ppu >= CIL) {
    "Cleaning process is capable."
  } else if (Testing_Ppu >= 1 && Testing_Ppu < CIL) {
    "Cleaning process is capable with low confidence — warning triggered."
  } else {
    "Cleaning process is NOT capable."
  }
  output <- data.frame(
    Ppu_training = ppu_ci_obj$Ppu,
    Ppu_threshold = CIL,
    Ppu_monitoring = Testing_Ppu,
    Performance_conclusion = decision,
    stringsAsFactors = FALSE
  )
  return(output)
}
