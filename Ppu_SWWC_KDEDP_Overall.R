Ppu_SWWC_KDEDP_Overall <- function(
    data1, Residue1, USL1,
    data2 = NULL, Residue2 = NULL, USL2 = NULL,
    data3 = NULL, Residue3 = NULL, USL3 = NULL,
    BW = "Silver1.06"
) {
  library(rlang)
  
  # Capture quosures of residue and USL column names
  Residue1_quo <- enquo(Residue1)
  USL1_quo <- enquo(USL1)
  
  Residue2_quo <- if (!missing(Residue2)) enquo(Residue2) else NULL
  USL2_quo <- if (!missing(USL2)) enquo(USL2) else NULL
  
  Residue3_quo <- if (!missing(Residue3)) enquo(Residue3) else NULL
  USL3_quo <- if (!missing(USL3)) enquo(USL3) else NULL
  
  # Helper to check if input is valid
  valid_input <- function(data, residue_quo, usl_quo) {
    !is.null(data) && !quo_is_null(residue_quo) && !quo_is_null(usl_quo)
  }
  
  all_ppus <- c()
  
  if (valid_input(data1, Residue1_quo, USL1_quo)) {
    ppus1 <- Ppu_SWWC_KDEDP(data1, !!Residue1_quo, !!USL1_quo, BW = BW)
    names(ppus1) <- paste0("Data1_", names(ppus1))
    all_ppus <- c(all_ppus, ppus1)
  }
  
  if (valid_input(data2, Residue2_quo, USL2_quo)) {
    ppus2 <- Ppu_SWWC_KDEDP(data2, !!Residue2_quo, !!USL2_quo, BW = BW)
    names(ppus2) <- paste0("Data2_", names(ppus2))
    all_ppus <- c(all_ppus, ppus2)
  }
  
  if (valid_input(data3, Residue3_quo, USL3_quo)) {
    ppus3 <- Ppu_SWWC_KDEDP(data3, !!Residue3_quo, !!USL3_quo, BW = BW)
    names(ppus3) <- paste0("Data3_", names(ppus3))
    all_ppus <- c(all_ppus, ppus3)
  }
  
  if (length(all_ppus) == 0) {
    stop("No valid datasets or variables provided.")
  }
  
  subgroup_ppus <- all_ppus[!grepl("^Ppu$", names(all_ppus))]
  overall_min <- min(subgroup_ppus, na.rm = TRUE)
  
  output_vector <- c(Ppu_Overall = overall_min, subgroup_ppus)
  
  # Return as a single-row data frame
  output_df <- as.data.frame(as.list(output_vector), stringsAsFactors = FALSE)
  
  return(output_df)
}
