Ppu_CQAWP_KDEDPonUSLND <- function(data1, Residue1, USL1,
                                   data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                   data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                   BW = "Silver1.06") {
  pooled_data <- NULL
  
  # Dataset 1
  data1_clean <- dplyr::filter(data1, !is.na({{ Residue1 }}) & !is.na({{ USL1 }})) %>%
    dplyr::mutate(Residue_normalized = {{ Residue1 }} / {{ USL1 }}) %>%
    dplyr::select(Residue_normalized)
  pooled_data <- data1_clean
  
  # Dataset 2
  if (!is.null(data2) && 
      !rlang::quo_is_null(rlang::enquo(Residue2)) && 
      !rlang::quo_is_null(rlang::enquo(USL2))) {
    
    data2_clean <- dplyr::filter(data2, !is.na({{ Residue2 }}) & !is.na({{ USL2 }})) %>%
      dplyr::mutate(Residue_normalized = {{ Residue2 }} / {{ USL2 }}) %>%
      dplyr::select(Residue_normalized)
    
    pooled_data <- dplyr::bind_rows(pooled_data, data2_clean)
  }
  
  # Dataset 3
  if (!is.null(data3) && 
      !rlang::quo_is_null(rlang::enquo(Residue3)) && 
      !rlang::quo_is_null(rlang::enquo(USL3))) {
    
    data3_clean <- dplyr::filter(data3, !is.na({{ Residue3 }}) & !is.na({{ USL3 }})) %>%
      dplyr::mutate(Residue_normalized = {{ Residue3 }} / {{ USL3 }}) %>%
      dplyr::select(Residue_normalized)
    
    pooled_data <- dplyr::bind_rows(pooled_data, data3_clean)
  }
  
  # Apply Ppu_KDEDPonUSLND on the pooled normalized data (USL = 1)
  ppu_result <- Ppu_KDEDPonUSLND(pooled_data, Residue = Residue_normalized, USL = 1, BW = BW)
  
  return(ppu_result)
}

