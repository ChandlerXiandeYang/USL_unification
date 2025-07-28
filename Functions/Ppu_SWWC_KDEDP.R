Ppu_SWWC_KDEDP <- function(data, Residue, USL, BW = "Silver1.06") {
  library(dplyr)
  library(rlang)
  residue_quo <- enquo(Residue)
  usl_quo <- enquo(USL)
# Filter rows with non-NA Residue and USL, and select those columns renamed to fixed names
data_clean <- data %>%
  filter(!is.na(!!residue_quo), !is.na(!!usl_quo)) %>%
  select(Residue = !!residue_quo, USL = !!usl_quo)

  if (nrow(data_clean) == 0) {
    stop("No valid rows after removing NA values in Residue or USL.")
  }

  subgroups <- split(data_clean, data_clean$USL)

  ppu_named_vector <- sapply(names(subgroups), function(usl_val) {
    sub_df <- subgroups[[usl_val]]
    res_values <- sub_df$Residue

    if (length(unique(res_values)) == 1) {
      return(100)
    } else {
  # For the following, we used USL-normalization method. In fact, it can be replaced by PPU_KDEDP(). 
  # The output is the same since Ppu is an invariant under USL-normalization    
  result <- Ppu_KDEDPonUSLND(sub_df, Residue = Residue, USL = USL, BW = BW) 
      return(result$Ppu)
    }
  })
  names(ppu_named_vector) <- paste0("Ppu_USL_", names(ppu_named_vector))
  overall_min <- min(ppu_named_vector, na.rm = TRUE)
  output <- as.data.frame(as.list(c(Ppu = overall_min, ppu_named_vector)), stringsAsFactors = FALSE)
  return(output)
}
