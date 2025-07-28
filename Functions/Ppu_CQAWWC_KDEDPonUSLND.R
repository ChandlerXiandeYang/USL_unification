Ppu_CQAWWC_KDEDPonUSLND <- function(data1, Residue1, USL1,
                                    data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                    data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                    BW = "Silver1.06") {
  ppu_named <- c()
  ppu1 <- Ppu_KDEDPonUSLND(data1, {{ Residue1 }}, {{ USL1 }}, BW)$Ppu
  ppu_named <- c(ppu_named, Ppu_Data1 = ppu1)
  
  if (!is.null(data2) && !rlang::quo_is_null(rlang::enquo(Residue2)) && !rlang::quo_is_null(rlang::enquo(USL2))) {
    ppu2 <- Ppu_KDEDPonUSLND(data2, {{ Residue2 }}, {{ USL2 }}, BW)$Ppu
    ppu_named <- c(ppu_named, Ppu_Data2 = ppu2)
  }
  
  if (!is.null(data3) && !rlang::quo_is_null(rlang::enquo(Residue3)) && !rlang::quo_is_null(rlang::enquo(USL3))) {
    ppu3 <- Ppu_KDEDPonUSLND(data3, {{ Residue3 }}, {{ USL3 }}, BW)$Ppu
    ppu_named <- c(ppu_named, Ppu_Data3 = ppu3)
  }
  
  min_ppu <- min(ppu_named, na.rm = TRUE)
 
  output <- as.data.frame(as.list(c(Ppu = min_ppu, ppu_named)))
  
  return(output)
}

