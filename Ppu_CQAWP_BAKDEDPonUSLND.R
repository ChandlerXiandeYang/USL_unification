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
  point_estimate <- Ppu_CQAWP_KDEDPonUSLND(data1, {{Residue1}}, {{USL1}},
                                           data2, {{Residue2}}, {{USL2}},
                                           data3, {{Residue3}}, {{USL3}},
                                           BW = BW)$Ppu
  
  clean_data <- function(data, residue, usl) {
    residue_quo <- enquo(residue)
    usl_quo <- enquo(usl)
    data %>%
      filter(!is.na(!!residue_quo), !is.na(!!usl_quo))
  }
  data1_clean <- clean_data(data1, {{Residue1}}, {{USL1}})
  data2_clean <- if (!is.null(data2)) clean_data(data2, {{Residue2}}, {{USL2}}) else NULL
  data3_clean <- if (!is.null(data3)) clean_data(data3, {{Residue3}}, {{USL3}}) else NULL
  
  # Capture quosures for Residue and USL to use in bootstrap safely
  Residue1_quo <- enquo(Residue1)
  USL1_quo <- enquo(USL1)
  Residue2_quo <- if (!missing(Residue2)) enquo(Residue2) else NULL
  USL2_quo <- if (!missing(USL2)) enquo(USL2) else NULL
  Residue3_quo <- if (!missing(Residue3)) enquo(Residue3) else NULL
  USL3_quo <- if (!missing(USL3)) enquo(USL3) else NULL
  
  # 2) Bootstrap iterations
  bootstrap_ppus <- replicate(n_boot, {
    boot_data1 <- data1_clean %>% sample_n(size = nrow(data1_clean), replace = TRUE)
    boot_data2 <- if (!is.null(data2_clean)) data2_clean %>% sample_n(size = nrow(data2_clean), replace = TRUE) else NULL
    boot_data3 <- if (!is.null(data3_clean)) data3_clean %>% sample_n(size = nrow(data3_clean), replace = TRUE) else NULL
    
    # 3) Calculate Ppu on bootstrap samples, safely
    tryCatch({
      Ppu_CQAWP_KDEDPonUSLND(
        boot_data1, !!Residue1_quo, !!USL1_quo,
        boot_data2, !!Residue2_quo, !!USL2_quo,
        boot_data3, !!Residue3_quo, !!USL3_quo,
        BW = BW)$Ppu
    }, error = function(e) NA_real_)
  })
  bootstrap_ppus <- na.omit(bootstrap_ppus)
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_ppus, probs = alpha / 2, names = FALSE)
  ci_upper <- quantile(bootstrap_ppus, probs = 1 - alpha / 2, names = FALSE)
    result_df <- data.frame(
    Ppu = round(point_estimate, 3),
    CI_lower = round(ci_lower, 3),
    n_boot = length(bootstrap_ppus),
    conf_level = conf_level,
    stringsAsFactors = FALSE
  )
   return(result_df)
}
