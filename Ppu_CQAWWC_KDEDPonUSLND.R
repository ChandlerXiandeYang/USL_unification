Ppu_CQAWWC_KDEDPonUSLND <- function(data1, Residue1, USL1,
                       data2 = NULL, Residue2 = NULL, USL2 = NULL,
                       data3 = NULL, Residue3 = NULL, USL3 = NULL,
                       BW = "Silver1.06") {
  # Capture variables for NSE (non-standard evaluation)
  Residue1_enquo <- rlang::enquo(Residue1)
  USL1_enquo <- rlang::enquo(USL1)
  
  ppu_list <- list()
  
  # Required: Dataset 1
  ppu1 <- Ppu_KDEDPonUSLND(data1, !!Residue1_enquo, !!USL1_enquo, BW)$Ppu
  ppu_list <- c(ppu_list, ppu1)
  
  # Optional: Dataset 2
  if (!is.null(data2) && !is.null(Residue2) && !is.null(USL2)) {
    Residue2_enquo <- rlang::enquo(Residue2)
    USL2_enquo <- rlang::enquo(USL2)
    ppu2 <- Ppu_KDEDPonUSLND(data2, !!Residue2_enquo, !!USL2_enquo, BW)$Ppu
    ppu_list <- c(ppu_list, ppu2)
  }
  
  # Optional: Dataset 3
  if (!is.null(data3) && !is.null(Residue3) && !is.null(USL3)) {
    Residue3_enquo <- rlang::enquo(Residue3)
    USL3_enquo <- rlang::enquo(USL3)
    ppu3 <- Ppu_KDEDPonUSLND(data3, !!Residue3_enquo, !!USL3_enquo, BW)$Ppu
    ppu_list <- c(ppu_list, ppu3)
  }
  
  # Return the minimum Ppu value across inputs
  return(min(unlist(ppu_list), na.rm = TRUE))
}
