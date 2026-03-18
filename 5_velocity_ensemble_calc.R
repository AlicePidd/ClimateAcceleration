# Computing median climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "")
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "")
  median_fol <- make_folder(source_disk, "3_vocc_median_terms", "")


  ssp <- "ssp245"
  term <- "mid"
  
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  make_ensembles <- function(ssp, term) {
    message("Processing: ", ssp, " - ", term)
    
    # Get files for each ssp-term combo
    combo_files <- dir(vocc_term_fol, full.names = TRUE) %>% 
      str_subset("534-over|ssp119|OISST", negate = TRUE) %>% 
      str_subset(ssp) %>% 
      str_subset(term)
    
    stack <- map(combo_files, readRDS) %>%  # Load and stack all ESMs
      rast()
    stack_median <- app(stack, median, na.rm = TRUE) # Median across ESMs
    stack_min <- app(stack, min, na.rm = TRUE) # Median across ESMs
    stack_max <- app(stack, max, na.rm = TRUE) # Median across ESMs
    
    
    o_nm <- paste0(median_fol, "vocc_median", ssp, "_", term, ".RDS")
    saveRDS(ens_median, o_nm)
  }
  
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5]) # No recent-term
  walk2(combos$ssp, combos$term, make_ensembles)
  
  
  
