CQAWWC_BAKDEDPonUSLND_CVStage3Monitoring <- function(
    data1, Residue1, USL1,
    data2 = NULL, Residue2 = NULL, USL2 = NULL,
    data3 = NULL, Residue3 = NULL, USL3 = NULL,
    new_data_list = NULL,
    BW = "Silver1.06",
    initial_boot = 1000,
    max_boot = 10000,
    conf_level = 0.95,
    seed = NULL
) {
  library(dplyr)
  library(stringr)
  if (!is.null(seed)) set.seed(seed)
  
  extract_largest_event_num <- function(data) {
    nums <- data %>%
      mutate(event_num = str_extract(CleaningEvent, "\\d+")) %>%
      filter(!is.na(event_num)) %>%
      pull(event_num) %>%
      as.integer()
    if (length(nums) == 0) return(NA_integer_)
    max(nums)
  }
  
  # 1) Point estimate + bootstrap CI
  ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
    data1 = data1, Residue1 = {{Residue1}}, USL1 = {{USL1}},
    data2 = data2, Residue2 = {{Residue2}}, USL2 = {{USL2}},
    data3 = data3, Residue3 = {{Residue3}}, USL3 = {{USL3}},
    BW = BW,
    n_boot = initial_boot,
    conf_level = conf_level,
    seed = seed
  )
  
  if (ppu_ci_obj$CI_lower < 1) {
    message("Initial CI lower bound < 1, increasing bootstrap to ", max_boot)
    ppu_ci_obj <- Ppu_CQAWWC_BAKDEDPonUSLND(
      data1 = data1, Residue1 = {{Residue1}}, USL1 = {{USL1}},
      data2 = data2, Residue2 = {{Residue2}}, USL2 = {{USL2}},
      data3 = data3, Residue3 = {{Residue3}}, USL3 = {{USL3}},
      BW = BW,
      n_boot = max_boot,
      conf_level = conf_level,
      seed = seed
    )
    if (ppu_ci_obj$CI_lower < 1) {
      warning("CIL still < 1 after ", max_boot, " bootstraps. Data quality or size may be insufficient.")
    }
  }
  
  # 2) Extract largest event numbers
  largest_1 <- extract_largest_event_num(data1)
  largest_2 <- if (!is.null(data2)) extract_largest_event_num(data2) else NA
  largest_3 <- if (!is.null(data3)) extract_largest_event_num(data3) else NA
  
  message("Largest CleaningEvent number in Dataset 1: ", largest_1)
  if (!is.na(largest_2)) message("Largest CleaningEvent number in Dataset 2: ", largest_2)
  if (!is.na(largest_3)) message("Largest CleaningEvent number in Dataset 3: ", largest_3)
  
  # 3) New data Ppu evaluation
  new_ppu <- NA_real_
  decision <- "No new data provided."
  
  if (!is.null(new_data_list)) {
    new1 <- new_data_list[[1]]
    new2 <- if (length(new_data_list) >= 2) new_data_list[[2]] else NULL
    new3 <- if (length(new_data_list) >= 3) new_data_list[[3]] else NULL
    
    filter_new <- function(new_data, largest_event) {
      if (is.null(new_data)) return(NULL)
      new_data %>%
        filter(as.integer(str_extract(CleaningEvent, "\\d+")) > largest_event)
    }
    
    new1_filtered <- filter_new(new1, largest_1)
    new2_filtered <- filter_new(new2, largest_2)
    new3_filtered <- filter_new(new3, largest_3)
    
    combined1 <- bind_rows(data1, new1_filtered)
    combined2 <- if (!is.null(data2)) bind_rows(data2, new2_filtered) else NULL
    combined3 <- if (!is.null(data3)) bind_rows(data3, new3_filtered) else NULL
    
    new_ppu_result <- Ppu_CQAWWC_KDEDPonUSLND(
      data1 = combined1, Residue1 = {{Residue1}}, USL1 = {{USL1}},
      data2 = combined2, Residue2 = {{Residue2}}, USL2 = {{USL2}},
      data3 = combined3, Residue3 = {{Residue3}}, USL3 = {{USL3}},
      BW = BW
    )
    
    new_ppu <- new_ppu_result$Ppu
    
    # 4) Decision
    decision <- if (new_ppu >= ppu_ci_obj$CI_lower) {
      "Cleaning process is capable."
    } else if (new_ppu >= 1 && new_ppu < ppu_ci_obj$CI_lower) {
      "Cleaning process is capable with low confidence — warning triggered."
    } else {
      "Cleaning process is NOT capable."
    }
    
    message(sprintf(
      "New Ppu: %.3f, Training CIL: %.3f, Decision: %s",
      new_ppu, ppu_ci_obj$CI_lower, decision
    ))
  }
  
  # Final return
  list(
    training_Ppu = ppu_ci_obj$Ppu,
    training_CIL = ppu_ci_obj$CI_lower,
    training_CIU = ppu_ci_obj$CI_upper,
    bootstrap_ppus = ppu_ci_obj$bootstrap_ppus,
    largest_event_numbers = list(
      Dataset1 = largest_1,
      Dataset2 = largest_2,
      Dataset3 = largest_3
    ),
    new_ppu = new_ppu,
    decision = decision
  )
}

