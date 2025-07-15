Ppu_CQAWP_BAKDEDPonUSLND <- function(data1, Residue1, USL1,
                                      data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                      data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                      BW = "Silver1.06",
                                      n_boot = 1000,
                                      conf_level = 0.95,
                                      seed = NULL) {
  library(dplyr)
  library(rlang)
  
  if (!is.null(seed)) set.seed(seed)
  
  # 1) Point estimate
  point_estimate <- Ppu_CQAWP_KDEDPonUSLND(data1, {{Residue1}}, {{USL1}},
                                           data2, {{Residue2}}, {{USL2}},
                                           data3, {{Residue3}}, {{USL3}},
                                           BW = BW)$Ppu
  
  # Helper to clean data (removing NAs in Residue and USL)
  clean_data <- function(data, residue, usl) {
    residue_quo <- enquo(residue)
    usl_quo <- enquo(usl)
    data %>%
      filter(!is.na(!!residue_quo), !is.na(!!usl_quo))
  }
  
  # Clean datasets
  data1_clean <- clean_data(data1, {{Residue1}}, {{USL1}})
  data2_clean <- if (!is.null(data2)) clean_data(data2, {{Residue2}}, {{USL2}}) else NULL
  data3_clean <- if (!is.null(data3)) clean_data(data3, {{Residue3}}, {{USL3}}) else NULL
  
  # 2) Bootstrap iterations
  bootstrap_ppus <- replicate(n_boot, {
    boot_data1 <- data1_clean %>% sample_n(size = nrow(data1_clean), replace = TRUE)
    boot_data2 <- if (!is.null(data2_clean)) data2_clean %>% sample_n(size = nrow(data2_clean), replace = TRUE) else NULL
    boot_data3 <- if (!is.null(data3_clean)) data3_clean %>% sample_n(size = nrow(data3_clean), replace = TRUE) else NULL
    
    # 3) Calculate Ppu on bootstrap samples, safely
    tryCatch({
      Ppu_CQAWP_KDEDPonUSLND(boot_data1, Residue, USL,
                             boot_data2, Residue, USL,
                             boot_data3, Residue, USL,
                             BW = BW)$Ppu
    }, error = function(e) NA_real_)
  })
  
  # Remove NAs from bootstrap results
  bootstrap_ppus <- na.omit(bootstrap_ppus)
  
  # 4) Confidence intervals from empirical quantiles
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_ppus, probs = alpha / 2, names = FALSE)
  ci_upper <- quantile(bootstrap_ppus, probs = 1 - alpha / 2, names = FALSE)
  
  # Return a list with point estimate, CI, bootstrap vector for further inspection
  list(
    Ppu = round(point_estimate, 3),
    CI_lower = round(ci_lower, 3),
    CI_upper = round(ci_upper, 3),
    n_boot = n_boot,
    bootstrap_ppus = bootstrap_ppus
  )
}
