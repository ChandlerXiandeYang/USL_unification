
package_vector <- c("quarto", "rmarkdown", "shiny", "rlang", "tidyr", "DBI", "odbc", "openxlsx", "dplyr", "knitr", 
                    "stringr", "ggplot2", "forcats", "readxl", "formattable", "easyanova", "VCA", "qcc", "lubridate", 
                    "kableExtra", "lme4", "AER", "dunn.test", "boot", "lmtest", "gridExtra", "magick", "gt", 
                    "webshot2", "chromote",  "DiagrammeR", "DiagrammeRsvg", "rsvg")

#"CleaningValidation",
check_and_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org")
    require(pkg, character.only = TRUE)
  }
}

lapply(package_vector, require, character.only=T)

Eq_DAR <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_DAR.xlsx")
Eq_CAR <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_CAR.xlsx")
Eq_Mic <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_Mic.xlsx")
Eq_DAR_20 <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_DAR_20.xlsx")
Eq_DAR_169 <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_DAR_169.xlsx")
Eq_DAR_122 <- read_excel("C:\\Users\\xyang1\\AAPSPharmSciTech\\USL_unification-main\\Equipment A\\Eq_DAR_122.xlsx")

Ppu_KDEDPonUSLND_Merged <- function(data1, Residue1, USL1,
                                    data2 = NULL, Residue2 = NULL, USL2 = NULL,
                                    data3 = NULL, Residue3 = NULL, USL3 = NULL,
                                    BW = "Silver1.06") {
  library(dplyr)
  library(rlang)
  
  # A helper function to clean, select, and normalize data
  clean_and_normalize <- function(data, residue, usl) {
    if (is.null(data) || rlang::quo_is_null(rlang::enquo(residue)) || rlang::quo_is_null(rlang::enquo(usl))) {
      return(NULL)
    }
    
    residue_quo <- enquo(residue)
    usl_quo <- enquo(usl)
    
    data %>%
      filter(!is.na(!!residue_quo), !is.na(!!usl_quo)) %>%
      mutate(Residue_Pct = (!!residue_quo / !!usl_quo) * 100) %>%
      pull(Residue_Pct)
  }
  
  # Clean and normalize each dataset
  x1 <- clean_and_normalize(data1, {{Residue1}}, {{USL1}})
  x2 <- clean_and_normalize(data2, {{Residue2}}, {{USL2}})
  x3 <- clean_and_normalize(data3, {{Residue3}}, {{USL3}})
  
  # Pool the data into a single vector
  pooled_x <- c(x1, x2, x3)
  
  # Handle cases where no valid data is provided
  if (length(pooled_x) == 0) {
    stop("No valid rows remaining after removing NAs and merging data.")
  }
  
  n <- length(pooled_x)
  s <- sd(pooled_x)
  
  # Bandwidth calculation
  if (is.character(BW)) {
    BW <- match.arg(BW, choices = c("Silver1.06", "Silver0.9", "Silver0.9IQR"))
    h <- switch(BW,
                "Silver1.06" = 1.06 * s / n^(1/5),
                "Silver0.9" = 0.9 * s / n^(1/5),
                "Silver0.9IQR" = {
                  iqr_val <- IQR(pooled_x)
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
  
  # Kernel Density Estimation
  kde <- density(pooled_x, bw = h, n = 2^20)
  
  # Calculate Ppu
  fx <- kde$y
  x_vals <- kde$x
  dx <- diff(x_vals)[1]
  Fx <- cumsum(fx) * dx
  Fx <- Fx / max(Fx)
  inv_cdf <- approxfun(Fx, x_vals, rule = 2)
  P0.5 <- inv_cdf(0.5)
  P0.99865 <- inv_cdf(0.99865)
  Ppu <- (100 - P0.5) / (P0.99865 - P0.5)
  
  # Calculate the probability P(x > 100)
  idx_100 <- which(kde$x >= 100)[1]
  
  if (is.na(idx_100)) {
    prob_defective <- 0
  } else {
    area_over_100 <- sum(kde$y[idx_100:length(kde$x)] * diff(kde$x)[1])
    total_area <- sum(kde$y) * diff(kde$x)[1]
    prob_defective <- format(area_over_100 / total_area, scientific=TRUE)
  }
  
  df_result <- data.frame(
    Ppu = round(Ppu, 3),
    P0.5 = round(P0.5, 3),
    P0.99865 = round(P0.99865, 3),
    Prob_Defective = prob_defective,
    N = n,
    Sample_SD = round(s, 3),
    Bandwidth_Method = bw_method,
    Bandwidth_Value = round(h, 5)
  )
  
  return(df_result)
}

Ppu_KDEDPonUSLND_Merged(data1=Eq_DAR, Residue1=DAR, USL1=USL,
                        data2 = Eq_CAR, Residue2 = CAR, USL2 = USL,
                        data3 = Eq_Mic, Residue3 = Mic, USL3 = USL,
                        BW = "Silver1.06")

my_result<-Ppu_KDEDPonUSLND_Merged(data1=Eq_DAR, Residue1=DAR, USL1=USL,
                                    data2 = Eq_CAR, Residue2 = CAR, USL2 = USL,
                                    data3 = Eq_Mic, Residue3 = Mic, USL3 = USL,
                                    BW = "Silver1.06") 

is.null(my_result$Prob_Defective)# This will show the number with full precision

print(my_result$Prob_Defective, digits = 22)# Or, as scientific notation


format(my_result$Prob_Defective, scientific = TRUE)
