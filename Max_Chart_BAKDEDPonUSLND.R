Max_Chart_BAKDPonUSLND<- function(data, 
                            CleaningEvent = "CleaningEvent",
                            DAR = "DAR",
                            USL = "USL",
                            Fiscal_Year = "Fiscal_Year",
                            Location = "Location",
                            seed = 123, 
                            B = 1000) {
  required_packages <- c("dplyr", "ggplot2", "tidyr", "scales", "cowplot")
  
  # Function to check and install/load packages
  load_or_install <- function(packages) {
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        message(paste("Installing package:", pkg))
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      } else {
        library(pkg, character.only = TRUE)
      }
    }
  }
  
  # Load or install all required packages
  load_or_install(required_packages)
  
  # Check if specified columns exist in data
  specified_cols <- c(CleaningEvent, DAR, USL, Fiscal_Year, Location)
  if (!all(specified_cols %in% names(data))) {
    missing_cols <- setdiff(specified_cols, names(data))
    stop(paste("The following specified columns are not found in the data:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Rename columns to standard names for internal processing
  data <- data %>%
    rename(
      CleaningEvent = !!sym(CleaningEvent),
      DAR = !!sym(DAR),
      USL = !!sym(USL),
      Fiscal_Year = !!sym(Fiscal_Year),
      Location = !!sym(Location)
    )
  
  # Normalize DAR
  data <- data %>%
    mutate(DAR_Pct = (DAR / USL) * 100)
  
  # Identify CleaningEvents where any DAR_Pct > 100
  bad_events <- data %>%
    group_by(CleaningEvent) %>%
    summarise(Any_OOC = any(DAR_Pct > 100, na.rm = TRUE), .groups = "drop") %>%
    filter(Any_OOC) %>%
    pull(CleaningEvent)
  
  # Max per CleaningEvent (used for plotting)
  max_df <- data %>%
    group_by(CleaningEvent) %>%
    summarise(
      Maximum_DAR = max(DAR_Pct, na.rm = TRUE),
      Location = Location[which.max(DAR_Pct)][1],
      Year = max(Fiscal_Year),
      .groups = "drop"
    ) %>%
    mutate(EventIndex = as.numeric(factor(CleaningEvent, levels = unique(CleaningEvent)))) %>%
    arrange(EventIndex) 
  
  # Define training set: remove future year + those with any DAR_Pct > 100
  max_year <- max(max_df$Year, na.rm = TRUE)
  train_df <- max_df %>%
    filter(Year < max_year & !(CleaningEvent %in% bad_events))
  
  m_vals <- train_df$Maximum_DAR
  n <- length(m_vals)
  if (n == 0) stop("No training data (prior years) found for KDE estimation.")
  s <- sd(m_vals)
  iqr_val <- IQR(m_vals)
  h <- max(0.9 * min(s, iqr_val / 1.34) * n^(-1/5), 1e-3)
  
  kde_est <- density(m_vals, bw = h, kernel = "gaussian", n = 2048)
  cdf_vals <- cumsum(kde_est$y) / sum(kde_est$y)
  inv_cdf <- approxfun(cdf_vals, kde_est$x)
  
  P <- c(0.99865, 0.9772, 0.8413, 0.5)
  UCL_KDE <- sapply(P, function(p) inv_cdf(p))
  names(UCL_KDE) <- paste0("P", P)
  
  set.seed(seed)
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
  
  # Add index and UCL flag
  plot_df <- max_df %>%
    mutate(
      EventIndex = row_number(),
      Above_UCL = Maximum_DAR > UCL_final["P0.99865"]
    )
  
  # Summary for labels
  loc_summary <- plot_df %>%
    group_by(Location) %>%
    summarise(
      count = as.character(n()),
      .groups = "drop"
    )
  
  # Consistent labels and colors
  location_labels <- with(loc_summary, paste0(Location, ": ", count))
  names(location_labels) <- loc_summary$Location
  
  custom_hue_pal <- function(n) {
    hues <- seq(45, 315, length.out = n)
    hcl(h = hues, l = 65, c = 100)
  }
  
  location_colors <- setNames(custom_hue_pal(length(location_labels)), names(location_labels))
  plot_df$Location <- factor(plot_df$Location, levels = names(location_labels))
  
  # Split index for training vs future
  split_index <- min(which(plot_df$Year == max_year))
  
  # Summary string
  above_ucl <- any(plot_df$Above_UCL, na.rm = TRUE)
  most_frequent_location <- plot_df %>%
    count(Location, sort = TRUE) %>%
    slice(1) %>%
    pull(Location)
  
  max_val_row <- plot_df %>%
    filter(Maximum_DAR == max(Maximum_DAR, na.rm = TRUE)) %>%
    slice(1)
  
  summary_label <- paste0(
    "Above UCL: ", above_ucl,
    " | Most Freq. Loc.: ", most_frequent_location,
    " | Max: (", round(max_val_row$Maximum_DAR, 1), ", ", max_val_row$Location, ")"
  )
  
  # X-axis break logic
  l <- nrow(plot_df)
  n_breaks <- if (l < 20) 10 else if (l < 50) 15 else if (l < 100) 20 else if (l < 200) 25 else 30
  breaks_dynamic <- pretty(range(plot_df$EventIndex, na.rm = TRUE), n = n_breaks)
  
  if (max(plot_df$EventIndex) > max(breaks_dynamic)) {
    breaks_dynamic <- c(breaks_dynamic, max(plot_df$EventIndex))
  }
  
  main_plot <- ggplot(plot_df, aes(x = EventIndex, y = Maximum_DAR, shape = Location, color = Above_UCL)) +
    # Background zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = UCL_final["P0.8413"], fill = "green", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = UCL_final["P0.8413"], ymax = UCL_final["P0.99865"], fill = "khaki", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = UCL_final["P0.99865"], ymax = Inf, fill = "orange", alpha = 0.3) +
    
    # Data elements
    geom_line(aes(group = 1), color = "gray") +
    geom_point(size = 1) +
    
    # Reference lines
    geom_hline(yintercept = UCL_final["P0.99865"], color = "red", linetype = "dashed") +
    geom_hline(yintercept = UCL_final["P0.9772"], color = "blue", linetype = "dotted") +
    geom_hline(yintercept = UCL_final["P0.8413"], color = "purple", linetype = "dotted") +
    geom_hline(yintercept = UCL_final["P0.5"], color = "black", linetype = "dotted") +
    
    # Annotations
    annotate("text", x = Inf, y = UCL_final["P0.99865"], label = paste0("UCL = ", round(UCL_final["P0.99865"], 2)), 
             hjust = 1.1, vjust = -0.5, color = "red") +
    annotate("text", x = Inf, y = UCL_final["P0.9772"], label = paste0("P0.9772 = ", round(UCL_final["P0.9772"], 2)), 
             hjust = 1.1, vjust = 1.1, color = "blue") +
    annotate("text", x = Inf, y = UCL_final["P0.8413"], label = paste0("P0.8413 = ", round(UCL_final["P0.8413"], 2)), 
             hjust = 1.1, vjust = -0.5, color = "purple") +
    annotate("text", x = Inf, y = UCL_final["P0.5"], label = paste0("Median = ", round(UCL_final["P0.5"], 2)), 
             hjust = 1.1, vjust = -0.5, color = "black") +
    geom_vline(xintercept = split_index - 0.5, linetype = "dashed", color = "black") +
    annotate("text", x = max(plot_df$EventIndex), y = -UCL_final["P0.99865"] * 0.05, 
             label = summary_label, hjust = 1.05, vjust = 1, size =2, fontface = "bold", color="red") +
    
    # Scales
    scale_x_continuous(breaks = breaks_dynamic) +
    scale_color_manual(values = c('TRUE' = "red", 'FALSE' = "black"), guide = "none") +
    scale_shape_manual(values = 0:(length(location_labels) - 1), 
                       labels = location_labels, 
                       name = "Location : #") +
    
    # Labels and theme
    labs(
      title = "BAKDL Max Chart of Residue per Cleaning Event",
      x = "Cleaning Event (Time Ordered)",
      y = paste0("Max ", DAR, "_Pct (USL-Unified=100)")
    ) +
    coord_cartesian(ylim = c(-as.numeric(UCL_final["P0.99865"]) * 0.1, max(plot_df$Maximum_DAR, na.rm = TRUE) * 1.1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      legend.justification = "top",
      legend.box.margin = margin(0, 0, 0, 20),  # Add space for legend
      plot.margin = margin(20, 60, 20, 20),     # Adjust right margin for legend
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    ) +
    guides(shape = guide_legend(
      ncol = 1,
      title.position = "top",
      override.aes = list(size = 3, color = "black")  # Consistent legend appearance
    ))
  
  return(main_plot)
}