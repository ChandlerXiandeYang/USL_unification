Ppu_CQAWP_KDEDPonUSLND <- function(data1, Residue1, USL1,
                                   data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                   data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                   BW = "Silver1.06") {
  library(dplyr)
  library(rlang)
  
  # Helper to clean and rename dataset columns to common names Residue and USL
  clean_and_rename <- function(data, residue, usl) {
    residue_quo <- enquo(residue)
    usl_quo <- enquo(usl)
    data %>%
      filter(!is.na(!!residue_quo), !is.na(!!usl_quo)) %>%
      select(Residue = !!residue_quo, USL = !!usl_quo)
  }
  
  # Clean datasets
  data1_clean <- clean_and_rename(data1, {{Residue1}}, {{USL1}})
  
  if (!is.null(data2) && !rlang::quo_is_null(rlang::enquo(Residue2)) && !rlang::quo_is_null(rlang::enquo(USL2))) {
    data2_clean <- clean_and_rename(data2, {{Residue2}}, {{USL2}})
  } else {
    data2_clean <- NULL
  }
  
  if (!is.null(data3) && !rlang::quo_is_null(rlang::enquo(Residue3)) && !rlang::quo_is_null(rlang::enquo(USL3))) {
    data3_clean <- clean_and_rename(data3, {{Residue3}}, {{USL3}})
  } else {
    data3_clean <- NULL
  }
  
  # Pool datasets
  pooled_data <- data1_clean
  
  if (!is.null(data2_clean)) {
    pooled_data <- bind_rows(pooled_data, data2_clean)
  }
  
  if (!is.null(data3_clean)) {
    pooled_data <- bind_rows(pooled_data, data3_clean)
  }
  
  # Now call Ppu_KDEDPonUSLND() with pooled data using common columns Residue and USL
  ppu_result <- Ppu_KDEDPonUSLND(
    data = pooled_data,
    Residue = Residue,
    USL = USL,
    BW = BW
  )
  
  return(ppu_result)
}
