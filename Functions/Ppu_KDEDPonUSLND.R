Ppu_KDEDPonUSLND <- function(data, Residue, USL, BW = "Silver1.06") {
  library(dplyr)
  library(rlang)
  residue_quo <- enquo(Residue)
  usl_quo <- enquo(USL)
  data_clean <- data %>%
    filter(!is.na(!!residue_quo), !is.na(!!usl_quo)) %>%
    mutate(Residue_Pct = (!!residue_quo / !!usl_quo) * 100) # USL-normalization step
  
  if (nrow(data_clean) == 0) stop("No valid rows remaining after removing NAs in Residue or USL.")
   x <- data_clean$Residue_Pct
  n <- length(x)
  s <- sd(x)
  if (is.character(BW)) {
    BW <- match.arg(BW, choices = c("Silver1.06", "Silver0.9", "Silver0.9IQR"))
    h <- switch(BW,
                "Silver1.06" = 1.06 * s / n^(1/5),
                "Silver0.9" = 0.9 * s / n^(1/5),
                "Silver0.9IQR" = {
                  iqr_val <- IQR(x)
                  sigma <- min(s, iqr_val / 1.34)
                  0.9 * sigma / n^(1/5)
                })
    bw_method <- BW
  } else if (is.numeric(BW) && length(BW) == 1 && BW > 0) {
    h <- BW
    bw_method <- "User-defined"
  } else {
    stop("BW must be numeric > 0 or one of: 'Silver1.06', 'Silver0.9', 'Silver0.9IQR'.")
  }
  kde <- density(x, bw = h, n = 2^20) # the default kernel is Gaussian
  fx <- kde$y
  x_vals <- kde$x
  dx <- diff(x_vals)[1]
  Fx <- cumsum(fx) * dx
  Fx <- Fx / max(Fx)
  inv_cdf <- approxfun(Fx, x_vals, rule = 2) # we did not use uniroot() which maybe better
  P0.5 <- inv_cdf(0.5)
  P0.99865 <- inv_cdf(0.99865)
  Ppu <- (100 - P0.5) / (P0.99865 - P0.5)
  df_result <- data.frame(
    Ppu = round(Ppu, 3),
    P0.5 = round(P0.5, 3),
    P0.99865 = round(P0.99865, 3),
    N = n,
    Sample_SD = round(s, 3),
    Bandwidth_Method = bw_method,
    Bandwidth_Value = round(h, 5)
  )
  return(df_result)
}
