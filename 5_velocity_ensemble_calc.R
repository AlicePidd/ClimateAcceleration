# Computing median climate velocity from ensemble of all 13 ESMs
  # Written by Alice P
    # 7 March 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "")
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "")
  ensemble_fol <- make_folder(source_disk, "3_vocc_ensemble_terms", "")


  
  
# Get the ensemble median climate velocity, per SSP-term combo -----------------

  make_ensembles <- function(ssp, term) {
    message("Processing: ", ssp, " - ", term)
    
    # Get files for each ssp-term combo
    combo_files <- dir(vocc_term_fol, full.names = TRUE) %>% 
      str_subset("534-over|ssp119|OISST", negate = TRUE) %>% 
      str_subset(ssp) %>% 
      str_subset(term)
    
    ens <- map(combo_files, readRDS) %>%  # Load and stack all ESMs
      rast()
    ens_median <- app(ens, median, na.rm = TRUE) # Median across ESMs
    
    o_nm <- paste0(ensemble_fol, "vocc_yearly_", ssp, "_ensemble_", term, ".RDS")
    saveRDS(ens_median, o_nm)
  }
  
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5]) # No recent-term
  walk2(combos$ssp, combos$term, make_ensembles)
  
  
  
