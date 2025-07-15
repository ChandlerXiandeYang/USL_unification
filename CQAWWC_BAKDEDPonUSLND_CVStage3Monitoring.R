CQAWWC_BAKDEDPonUSLND_CVStage3Monitoring <- function(
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
  
  # Split data into training and testing
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
  
  # Estimate CIL by bootstrap if needed
  if (is.null(CIL) || CIL < 1) {
    ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
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
      ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
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
  } 
  else {
    training_ppu <- Ppu_CQAWWC_KDEDPonUSLND(
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
  
  # Combine training + testing for Ppu evaluation
  CombinedData1 <- bind_rows(TrainingData1, TestingData1)
  CombinedData2 <- if (!is.null(TrainingData2)) bind_rows(TrainingData2, TestingData2) else NULL
  CombinedData3 <- if (!is.null(TrainingData3)) bind_rows(TrainingData3, TestingData3) else NULL
  
  testing_ppu_result <- Ppu_CQAWWC_KDEDPonUSLND(
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
  
  message(sprintf(
    "Testing Ppu: %.3f, Training CIL: %.3f, Decision: %s",
    Testing_Ppu, CIL, decision
  ))
  
  return(list(
    training_Ppu = ppu_ci_obj$Ppu_training,
    training_CIL = CIL,
    training_CIU = ppu_ci_obj$CI_upper,
    bootstrap_ppus = ppu_ci_obj$bootstrap_ppus,
    Testing_Ppu = Testing_Ppu,
    decision = decision
  ))
}


#CQAWWC_BAKDEDPonUSLND_CVStage3Monitoring(data1=Eq_DAR, Residue1=DAR, USL1=USL, Fiscal_Year1=Fiscal_Year, data2=Eq_CAR, Residue2=CAR, USL2=USL, Fiscal_Year2=Fiscal_Year, data3=Eq_Mic, Residue3=Mic, USL3=USL, Fiscal_Year3=Fiscal_Year, seed=1234, Time_cut=2025)
# CQAWWC_BAKDEDPonUSLND_CVStage3Monitoring <- function(
#     data1, Residue1, USL1, Fiscal_Year1,
#     data2 = NULL, Residue2 = NULL, USL2 = NULL, Fiscal_Year2 = NULL,
#     data3 = NULL, Residue3 = NULL, USL3 = NULL, Fiscal_Year3 = NULL,
#     Time_cut,
#     CIL = NULL,
#     BW = "Silver1.06",
#     initial_boot = 1000,
#     max_boot = 10000,
#     conf_level = 0.95,
#     seed = NULL
# ) {
#   library(dplyr)
#   
#   if (!is.null(seed)) set.seed(seed)
#   
#   # Helper to clean NA from residue column
#   clean_na_residue <- function(data, residue_col) {
#     data %>% filter(!is.na({{ residue_col }}))
#   }
#   
#   # Split data into training and testing
#   split_by_fiscal_year <- function(data, fiscal_year_col, time_cut) {
#     training <- data %>% 
#       filter(!is.na({{ fiscal_year_col }})) %>%
#       filter({{ fiscal_year_col }} < time_cut)
#     
#     testing <- data %>% 
#       filter(!is.na({{ fiscal_year_col }})) %>%
#       filter({{ fiscal_year_col }} >= time_cut)
#     
#     list(training = training, testing = testing)
#   }
#   
#   # Process data1
#   split1 <- split_by_fiscal_year(data1, {{ Fiscal_Year1 }}, Time_cut)
#   TrainingData1 <- clean_na_residue(split1$training, {{ Residue1 }})
#   TestingData1 <- clean_na_residue(split1$testing, {{ Residue1 }})
#   
#   # Process data2 if provided
#   if (!is.null(data2) && !is.null(Residue2) && !is.null(USL2) && !is.null(Fiscal_Year2)) {
#     split2 <- split_by_fiscal_year(data2, {{ Fiscal_Year2 }}, Time_cut)
#     TrainingData2 <- clean_na_residue(split2$training, {{ Residue2 }})
#     TestingData2 <- clean_na_residue(split2$testing, {{ Residue2 }})
#   } else {
#     TrainingData2 <- NULL
#     TestingData2 <- NULL
#   }
#   
#   # Process data3 if provided
#   if (!is.null(data3) && !is.null(Residue3) && !is.null(USL3) && !is.null(Fiscal_Year3)) {
#     split3 <- split_by_fiscal_year(data3, {{ Fiscal_Year3 }}, Time_cut)
#     TrainingData3 <- clean_na_residue(split3$training, {{ Residue3 }})
#     TestingData3 <- clean_na_residue(split3$testing, {{ Residue3 }})
#   } else {
#     TrainingData3 <- NULL
#     TestingData3 <- NULL
#   }
#   
#   # Estimate CIL
#   if (is.null(CIL) || CIL < 1) {
#     ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
#       data1 = TrainingData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
#       BW = BW,
#       n_boot = initial_boot,
#       conf_level = conf_level,
#       seed = seed,
#       !!!if (!is.null(TrainingData2)) list(data2 = TrainingData2, Residue2 = enquo(Residue2), USL2 = {{ USL2 }}) else list(),
#       !!!if (!is.null(TrainingData3)) list(data3 = TrainingData3, Residue3 = enquo(Residue3), USL3 = {{ USL3 }}) else list()
#     )
#     
#     if (ppu_ci_obj$CI_lower < 1) {
#       message("Initial CI lower bound < 1, increasing bootstrap to ", max_boot)
#       ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
#         data1 = TrainingData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
#         BW = BW,
#         n_boot = max_boot,
#         conf_level = conf_level,
#         seed = seed,
#         !!!if (!is.null(TrainingData2)) list(data2 = TrainingData2, Residue2 = enquo(Residue2), USL2 = {{ USL2 }}) else list(),
#         !!!if (!is.null(TrainingData3)) list(data3 = TrainingData3, Residue3 = enquo(Residue3), USL3 = {{ USL3 }}) else list()
#       )
#       if (ppu_ci_obj$CI_lower < 1) {
#         warning("CIL still < 1 after ", max_boot, " bootstraps. Data quality or size may be insufficient.")
#       }
#     }
#     CIL <- ppu_ci_obj$CI_lower
#   } else {
#     ppu_ci_obj <- list(Ppu = NA_real_, CI_lower = CIL, CI_upper = NA_real_, bootstrap_ppus = NULL)
#   }
#   
#   # Combine training and testing for inference
#   CombinedData1 <- bind_rows(TrainingData1, TestingData1)
#   CombinedData2 <- if (!is.null(TrainingData2)) bind_rows(TrainingData2, TestingData2) else NULL
#   CombinedData3 <- if (!is.null(TrainingData3)) bind_rows(TrainingData3, TestingData3) else NULL
#   
#   testing_ppu_result <- Ppu_CQAWWC_KDEDPonUSLND(
#     data1 = CombinedData1, Residue1 = {{ Residue1 }}, USL1 = {{ USL1 }},
#     BW = BW,
#     !!!if (!is.null(CombinedData2)) list(data2 = CombinedData2, Residue2 = enquo(Residue2), USL2 = {{ USL2 }}) else list(),
#     !!!if (!is.null(CombinedData3)) list(data3 = CombinedData3, Residue3 = enquo(Residue3), USL3 = {{ USL3 }}) else list()
#   )
#   
#   Testing_Ppu <- testing_ppu_result$Ppu
#   
#   decision <- if (Testing_Ppu >= CIL) {
#     "Cleaning process is capable."
#   } else if (Testing_Ppu >= 1 && Testing_Ppu < CIL) {
#     "Cleaning process is capable with low confidence — warning triggered."
#   } else {
#     "Cleaning process is NOT capable."
#   }
#   
#   message(sprintf(
#     "Testing Ppu: %.3f, Training CIL: %.3f, Decision: %s",
#     Testing_Ppu, CIL, decision
#   ))
#   
#   return(list(
#     training_Ppu = ppu_ci_obj$Ppu,
#     training_CIL = CIL,
#     training_CIU = ppu_ci_obj$CI_upper,
#     bootstrap_ppus = ppu_ci_obj$bootstrap_ppus,
#     Testing_Ppu = Testing_Ppu,
#     decision = decision
#   ))
# }

