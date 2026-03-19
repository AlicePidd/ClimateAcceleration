# Computing median climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  vocc_term_fol <- make_folder(source_disk, "2_velocity_rolling_annual_termsplit", "")
  median_fol <- make_folder(source_disk, "3_velocity_median_terms", "")


  
  
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  get_median <- function(ssp, term) {
    message("Processing: ", ssp, " - ", term)
    
    # Get files for each ssp-term combo
    combo_files <- dir(vocc_term_fol, full.names = TRUE) %>% 
      str_subset("534-over|ssp119|OISST", negate = TRUE) %>% 
      str_subset(ssp) %>% 
      str_subset(term)
    
    stack <- map(combo_files, readRDS) %>%  # Load and stack all ESMs into one spatraster
      rast()
    stack_median <- app(stack, median, na.rm = TRUE) # Median across ESMs
    stack_min <- app(stack, min, na.rm = TRUE) # Min across ESMs
    stack_max <- app(stack, max, na.rm = TRUE) # Max across ESMs

    full_stack <- c(stack_median, stack_min, stack_max) # Stack them together
    names(full_stack) <- c(paste0(names(stack_median), "_", ssp, "_", term, "-term"),
                           paste0(names(stack_min), "_", ssp, "_", term, "-term"),
                           paste0(names(stack_max), "_", ssp, "_", term, "-term"))
    o_nm <- paste0(median_fol, "vocc_median_", ssp, "_", term, ".RDS")
    saveRDS(full_stack, o_nm)
  }
  
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5]) # No recent-term
  walk2(combos$ssp, combos$term, get_median)
  
  

  
  
  
