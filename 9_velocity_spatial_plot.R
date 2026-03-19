# Plotting climate velocity -- median per grid cell
  # Written by Alice P
    # 7 March 2026

# Climate velocity in each grid cell - median across ESMs, per SSP and term

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "")
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "")
  plot_fol <- make_folder(source_disk, "4_velocity_aus_plot", "spatial")

  
  files <- dir(vocc_term_fol, full.names = TRUE) %>% 
    str_subset(., "534-over", negate = TRUE) %>% 
    str_subset(., "ssp119", negate = TRUE)
    
  files
  f <- files[2]
  f
  
  r <- readRDS(f)
  r
  names(r)  
  
  plot(r)  
  readRDS("/Volumes/AliceShield/acceleration_data/2_vocc_rolling_annual_termsplit/vocc_yearly_ssp119_CanESM5_intermediate_2061-2080.RDS")
  