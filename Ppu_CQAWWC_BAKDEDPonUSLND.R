Ppu_CQAWWC_BAKDEDPonUSLND <- function(data1, Residue1, USL1,
                                      data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                      data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                      BW = "Silver1.06",
                                      n_boot = 1000,
                                      conf_level = 0.95,
                                      seed = NULL) {
  library(dplyr)
  library(rlang)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Helper: clean dataset by removing rows with missing Residue or USL
  clean_data <- function(data, residue, usl) {
    residue_quo <- enquo(residue)
    usl_quo <- enquo(usl)
    
    data %>%
      filter(!is.na(!!residue_quo), !is.na(!!usl_quo))
  }
  
  # Clean all datasets
  data1_clean <- clean_data(data1, {{Residue1}}, {{USL1}})
  data2_clean <- if (!is.null(data2)) clean_data(data2, {{Residue2}}, {{USL2}}) else NULL
  data3_clean <- if (!is.null(data3)) clean_data(data3, {{Residue3}}, {{USL3}}) else NULL
  
  # Point estimate of Ppu on cleaned data
  point_estimate_all <- Ppu_CQAWWC_KDEDPonUSLND(data1_clean, {{Residue1}}, {{USL1}},
                                                data2_clean, {{Residue2}}, {{USL2}},
                                                data3_clean, {{Residue3}}, {{USL3}},
                                                BW = BW)
  point_estimate <- point_estimate_all[["Ppu"]]
  
  # Bootstrap function: resample and calculate minimal Ppu from all datasets
  bootstrap_ppus <- replicate(n_boot, {
    boot_data1 <- data1_clean %>% sample_n(size = nrow(data1_clean), replace = TRUE)
    boot_data2 <- if (!is.null(data2_clean)) data2_clean %>% sample_n(size = nrow(data2_clean), replace = TRUE) else NULL
    boot_data3 <- if (!is.null(data3_clean)) data3_clean %>% sample_n(size = nrow(data3_clean), replace = TRUE) else NULL
    
    tryCatch({
      res <- Ppu_CQAWWC_KDEDPonUSLND(boot_data1, {{Residue1}}, {{USL1}},
                                     boot_data2, {{Residue2}}, {{USL2}},
                                     boot_data3, {{Residue3}}, {{USL3}},
                                     BW = BW)
      res[["Ppu"]]  # extract minimal Ppu value only
    }, error = function(e) NA_real_)
  })
  
  bootstrap_ppus <- na.omit(bootstrap_ppus)
  
  # Calculate confidence interval from bootstrap distribution
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_ppus, probs = alpha / 2, names = FALSE)
  ci_upper <- quantile(bootstrap_ppus, probs = 1 - alpha / 2, names = FALSE)
  
  list(
    Ppu = round(point_estimate, 3),
    CI_lower = round(ci_lower, 3),
    CI_upper = round(ci_upper, 3),
    n_boot = n_boot,
    bootstrap_ppus = bootstrap_ppus
  )
}


