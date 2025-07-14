Ppu_CQAWP_KDEDPonUSLND <- function(data1, Residue1, USL1,
                                   data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                   data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                   BW = "Silver1.06") {
  # Enquote variables
  Residue1_enquo <- rlang::enquo(Residue1)
  USL1_enquo <- rlang::enquo(USL1)
  
  # Normalize and clean dataset 1
  data1_clean <- dplyr::filter(data1, !is.na(!!Residue1_enquo) & !is.na(!!USL1_enquo))
  data1_norm <- dplyr::mutate(data1_clean,
                              Residue_normalized = !!Residue1_enquo / !!USL1_enquo)
  pooled_data <- dplyr::select(data1_norm, Residue_normalized)
  
  # Dataset 2
  if (!is.null(data2) && !is.null(Residue2) && !is.null(USL2)) {
    Residue2_enquo <- rlang::enquo(Residue2)
    USL2_enquo <- rlang::enquo(USL2)
    
    data2_clean <- dplyr::filter(data2, !is.na(!!Residue2_enquo) & !is.na(!!USL2_enquo))
    data2_norm <- dplyr::mutate(data2_clean,
                                Residue_normalized = !!Residue2_enquo / !!USL2_enquo)
    pooled_data <- dplyr::bind_rows(pooled_data, dplyr::select(data2_norm, Residue_normalized))
  }
  
  # Dataset 3
  if (!is.null(data3) && !is.null(Residue3) && !is.null(USL3)) {
    Residue3_enquo <- rlang::enquo(Residue3)
    USL3_enquo <- rlang::enquo(USL3)
    
    data3_clean <- dplyr::filter(data3, !is.na(!!Residue3_enquo) & !is.na(!!USL3_enquo))
    data3_norm <- dplyr::mutate(data3_clean,
                                Residue_normalized = !!Residue3_enquo / !!USL3_enquo)
    pooled_data <- dplyr::bind_rows(pooled_data, dplyr::select(data3_norm, Residue_normalized))
  }
  
  # Apply Ppu_KDEDPonUSLND to the pooled normalized data (USL = 1)
  ppu_result <- Ppu_KDEDPonUSLND(pooled_data, Residue = Residue_normalized, USL = 1, BW = BW)
  
  return(ppu_result)
}
