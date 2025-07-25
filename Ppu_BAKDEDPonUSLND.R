Ppu_BAKDEDPonUSLND <- function(data, Residue, USL, BW = "Silver1.06", n_boot = 1000, conf_level = 0.95, seed = 1234) {
  library(rlang)
  Residue_enquo <- enquo(Residue)
  USL_enquo <- enquo(USL)
  set.seed(seed)
  ppu_full <- Ppu_KDEDPonUSLND(data, !!Residue_enquo, !!USL_enquo, BW)$Ppu
  n <- nrow(data)
  ppu_boot <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    data_boot <- data[sample(seq_len(n), size = n, replace = TRUE), ]
    ppu_boot[i] <- tryCatch({
      Ppu_KDEDPonUSLND(data_boot, !!Residue_enquo, !!USL_enquo, BW)$Ppu
    }, error = function(e) NA_real_)
  }
  ppu_boot <- ppu_boot[!is.na(ppu_boot)]
  alpha <- (1 - conf_level)
  cil <- quantile(ppu_boot, probs = alpha / 2, names = FALSE)
  ciu <- quantile(ppu_boot, probs = 1 - alpha / 2, names = FALSE)
  result_df <- data.frame(
    Ppu = ppu_full,
    CI_lower = cil,
    CI_upper = ciu,
    conf_level = conf_level,
    n_boot = length(ppu_boot),
    stringsAsFactors = FALSE
  )
  return(result_df)
}
