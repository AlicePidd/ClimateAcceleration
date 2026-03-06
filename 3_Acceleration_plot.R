# Plotting climate acceleration by 1° latitude
  # Written by Alice P
    # 6 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  acc_fol <- make_folder(source_disk, "3_acceleration_aus", "") # Aus files
  # acc_global_fol <- make_folder(source_disk, "3_acceleration_global", "") # Global files
  plot_fol <- make_folder(source_disk, "5_acceleration_aus_plot", "") # Aus files
  
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(vocc_fol, full.names = TRUE) %>% 
    str_subset(., "ssp119", negate = TRUE) %>% 
    str_subset(., "ssp534-over", negate = TRUE)
