CQAWP_BAKDEDPonUSLND <- function(
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
  
  # Helper to clean NA from residue column
  clean_na_residue <- function(data, residue_col) {
    data %>% filter(!is.na({{ residue_col }}))
  }
  
  # Split data by fiscal year into training and testing
  split_by_fiscal_year <- function(data, fiscal_year_col, time_cut) {
    training <- data %>%
      filter(!is.na({{ fiscal_year_col }})) %>%
      filter({{ fiscal_year_col }} < time_cut)
    
    testing <- data %>%
      filter(!is.na({{ fiscal_year_col }})) %>%
      filter({{ fiscal_year_col }} >= time_cut)
    
    list(training = training, testing = testing)
  }
  
  # Split data1
  split1 <- split_by_fiscal_year(data1, {{ Fiscal_Year1 }}, Time_cut)
  TrainingData1 <- clean_na_residue(split1$training, {{ Residue1 }})
  TestingData1 <- clean_na_residue(split1$testing, {{ Residue1 }})
  
  # Split data2
  if (!is.null(data2)) {
    split2 <- split_by_fiscal_year(data2, {{ Fiscal_Year2 }}, Time_cut)
    TrainingData2 <- clean_na_residue(split2$training, {{ Residue2 }})
    TestingData2 <- clean_na_residue(split2$testing, {{ Residue2 }})
  } else {
    TrainingData2 <- NULL
    TestingData2 <- NULL
  }
  
  # Split data3
  if (!is.null(data3)) {
    split3 <- split_by_fiscal_year(data3, {{ Fiscal_Year3 }}, Time_cut)
    TrainingData3 <- clean_na_residue(split3$training, {{ Residue3 }})
    TestingData3 <- clean_na_residue(split3$testing, {{ Residue3 }})
  } else {
    TrainingData3 <- NULL
    TestingData3 <- NULL
  }
  
  # Estimate CIL if not provided or < 1
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
      Ppu_training = training_ppu,
      CI_lower = CIL,
      CI_upper = NA_real_,
      bootstrap_ppus = NULL
    )
  }
  
  # Combine training + testing for full evaluation
  CombinedData1 <- bind_rows(TrainingData1, TestingData1)
  CombinedData2 <- if (!is.null(TrainingData2)) bind_rows(TrainingData2, TestingData2) else NULL
  CombinedData3 <- if (!is.null(TrainingData3)) bind_rows(TrainingData3, TestingData3) else NULL
  
  # Calculate testing Ppu
  testing_ppu_result <- Ppu_CQAWP_KDEDPonUSLND(
    data1 = CombinedData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
    data2 = CombinedData2, Residue2 = {{ Residue2 }}, USL2 = {{ USL2 }},
    data3 = CombinedData3, Residue3 = {{ Residue3 }}, USL3 = {{ USL3 }},
    BW = BW
  )
  
  Ppu_testing <- testing_ppu_result$Ppu
  
  # Make decision based on comparison
  decision <- if (Testing_Ppu >= CIL) {
    "Cleaning process is capable."
  } else if (Testing_Ppu >= 1 && Testing_Ppu < CIL) {
    "Cleaning process is capable with low confidence — warning triggered."
  } else {
    "Cleaning process is NOT capable."
  }
  
  message(sprintf(
    "Testing Ppu: %.3f, Training CIL: %.3f, Decision: %s",
    Testing_Ppu, CIL, decision
  ))
  
  return(list(
    Ppu_training = ppu_ci_obj$Ppu,
    training_CIL = CIL,
    training_CIU = ppu_ci_obj$CI_upper,
    bootstrap_ppus = ppu_ci_obj$bootstrap_ppus,
    Ppu_testing = Testing_Ppu,
    decision = decision
  ))
}
