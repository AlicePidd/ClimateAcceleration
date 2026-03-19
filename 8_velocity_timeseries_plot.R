# Plotting median climate velocity per SSP-term combo
  # Written by Alice P
    # 19 March 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_median_fol <- make_folder(source_disk, "3_velocity_median_terms", "")
  plot_fol <- make_folder(source_disk, "4_velocity_aus_plot", "spatial")

  
  files <- dir(vocc_median_fol, full.names = TRUE)

  files
  f <- files[7]
  f
  
  r <- readRDS(f)
  r
  names(r)  
  
  plot(r)  
