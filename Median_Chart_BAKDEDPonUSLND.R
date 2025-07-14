Median_Chart_BAKDEDPonUSLND <- function(data) {
  required_cols <- c("CleaningEvent", "DAR", "USL", "Fiscal_Year")
  if (!all(required_cols %in% names(data))) {
    stop("Input data must contain: CleaningEvent, DAR, USL, Fiscal_Year")
  }
  
  D_unified <- data %>%
    mutate(DAR_Pct = (DAR / USL) * 100)
  
  Median_Event <- D_unified %>%
    group_by(CleaningEvent) %>%
    summarise(
      median_DAR = median(DAR_Pct),
      Year = max(Fiscal_Year),
      .groups = "drop"
    ) %>%
    arrange(CleaningEvent)
  
  max_year <- max(Median_Event$Year, na.rm = TRUE)
  Median_train <- Median_Event %>% filter(Year < max_year)
  Median_test <- Median_Event %>% filter(Year == max_year)
  
  m_vals <- Median_train$median_DAR
  n <- length(m_vals)
  s <- sd(m_vals)
  iqr_val <- IQR(m_vals)
  h <- max(0.9 * min(s, iqr_val / 1.34) * n^(-1/5), 1e-3)
  
  kde_est <- density(m_vals, bw = h, kernel = "gaussian", n = 2048)
  cdf_vals <- cumsum(kde_est$y) / sum(kde_est$y)
  inv_cdf <- approxfun(cdf_vals, kde_est$x)
  
  P <- c(0.99865, 0.9772, 0.8413, 0.5)
  UCL_KDE <- sapply(P, function(p) inv_cdf(p))
  names(UCL_KDE) <- paste0("P", P)
  
  set.seed(123)
  B <- 1000
  UCL_boot <- matrix(NA, nrow = B, ncol = length(P))
  for (b in 1:B) {
    samp <- sample(m_vals, n, replace = TRUE)
    s_b <- sd(samp)
    iqr_b <- IQR(samp)
    h_b <- max(0.9 * min(s_b, iqr_b / 1.34) * (n - 1)^(-1/5), 1e-3)
    kde_b <- density(samp, bw = h_b, kernel = "gaussian", n = 2048)
    cdf_b <- cumsum(kde_b$y) / sum(kde_b$y)
    inv_cdf_b <- approxfun(cdf_b, kde_b$x)
    UCL_boot[b, ] <- sapply(P, function(p) inv_cdf_b(p))
  }
  colnames(UCL_boot) <- paste0("P", P)
  
  UCL_final <- sapply(1:length(P), function(i) {
    median(c(UCL_KDE[i], UCL_boot[, i]), na.rm = TRUE)
  })
  names(UCL_final) <- paste0("P", P)
  
  plot_df <- Median_Event %>%
    mutate(
      index = 1:n(),
      Above_UCL = median_DAR > UCL_final["P0.99865"]
    )
  plot_df$EventIndex <- as.numeric(as.character(plot_df$index))
  # Determine the number of breaks based on the length of the data
  l <- length(plot_df$EventIndex)
  # Custom logic for breaks
  n_breaks <- if (l < 20) {
    10
  } else if (l < 50) {
    15
  } else if (l < 100) {
    20
  } else if (l < 200) {
    25
  } else if (l < 300) {
    30
  } else {
    30
  }
  # Generate dynamic breaks
  breaks_dynamic <- pretty(range(plot_df$EventIndex, na.rm = TRUE), n = n_breaks)
  
  # Ensure the max value is included if it's not already
  if (max(plot_df$EventIndex, na.rm = TRUE) > max(breaks_dynamic)) {
    breaks_dynamic <- c(breaks_dynamic, max(plot_df$EventIndex, na.rm = TRUE))
  }
  
  split_index <- min(which(plot_df$Year == max_year))
  
  p <- ggplot(plot_df, aes(x = index, y = median_DAR)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = UCL_final["P0.8413"], fill = "green", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = UCL_final["P0.8413"], ymax = UCL_final["P0.99865"], fill = "khaki", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = UCL_final["P0.99865"], ymax = Inf, fill = "orange", alpha = 0.3) +
    geom_line(color = "gray") +
    geom_point(aes(color = Above_UCL), size = 2,show.legend=FALSE) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    geom_hline(yintercept = UCL_final["P0.99865"], color = "red", linetype = "dashed") +
    geom_hline(yintercept = UCL_final["P0.9772"], color = "blue", linetype = "dotted") +
    geom_hline(yintercept = UCL_final["P0.8413"], color = "black", linetype = "dotted") +
    geom_hline(yintercept = UCL_final["P0.5"], color = "purple", linetype = "dotted") +
    annotate("text", x = Inf, y = UCL_final["P0.99865"], label = paste0("UCL = ", round(UCL_final["P0.99865"], 2)), hjust = 1.1, vjust = -0.5, color = "red") +
    annotate("text", x = Inf, y = UCL_final["P0.9772"], label = paste0("P0.9772 = ", round(UCL_final["P0.9772"], 2)), hjust = 1.1, vjust = 1.1, color = "blue") +
    annotate("text", x = Inf, y = UCL_final["P0.8413"], label = paste0("P0.8413 = ", round(UCL_final["P0.8413"], 2)), hjust = 1.1, vjust = -0.5, color = "purple") +
    annotate("text", x = Inf, y = UCL_final["P0.5"], label = paste0("Median = ", round(UCL_final["P0.5"], 2)), hjust = 1.1, vjust = -0.5, color = "black") +
    geom_vline(xintercept = split_index - 0.5, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = breaks_dynamic, labels = breaks_dynamic) +
    labs(
      title = "BAKDL-Median Chart of Residue per Cleaning Event",
      x = "Cleaning Event(Time Ordered)",
      y = "Median DAR_Pct (USL-Unified=100)",
      color = "Above UCL"
    ) +
    annotate("text",
             x = max(plot_df$index),
             y = -UCL_final["P0.99865"] * 0.05,
             label = paste("Above UCL:", sum(plot_df$Above_UCL, na.rm = TRUE) > 0),
             hjust = 1, vjust = 1, size = 4, fontface = "bold") +
    theme_minimal() +
    coord_cartesian(ylim = c(-UCL_final["P0.99865"] * 0.1, UCL_final["P0.99865"] * 1.1))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  return(p)
}
