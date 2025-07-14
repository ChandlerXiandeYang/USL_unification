Ppu_SWWC_KDEDP <- function(data, Residue, USL, BW = "Silver1.06") {
  library(dplyr)
  library(rlang)
  
  residue_quo <- enquo(Residue)
  usl_quo <- enquo(USL)
  
  # Clean input data
  data_clean <- data %>%
    filter(!is.na(!!residue_quo), !is.na(!!usl_quo)) %>%
    mutate(Residue = !!residue_quo, USL = !!usl_quo)
  
  if (nrow(data_clean) == 0) {
    stop("No valid rows after removing NA values in Residue or USL.")
  }
  
  # Split into subgroups by unique USL
  subgroups <- split(data_clean, data_clean$USL)
  
  # Compute Ppu per subgroup
  ppu_values <- lapply(subgroups, function(sub_df) {
    res_values <- sub_df$Residue
    
    if (length(unique(res_values)) == 1) {
      return(100)  # Constant residue values → ideal capability
    } else {
      result <- Ppu_KDEDPonUSLND(sub_df, Residue = Residue, USL = USL, BW = BW)
      return(result$Ppu)
    }
  })
  
  return(min(unlist(ppu_values), na.rm = TRUE))
}