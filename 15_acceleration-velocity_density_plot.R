# Plotting density plots of velcoity and acceleration inside and outside
  # Written by Alice P
    # 7 April 2026
  # Inspired by Brito-Morales et al. 2020



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts") 
  accel_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial") # Aus files
  pdf_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial/pdfs") # Aus files
  
  
  
# Load rasters -----------------------------------------------------------------
  
  vel_files   <- dir(vocc_fol, full.names = TRUE, pattern = ".RDS") %>%
    str_subset("historical", negate = TRUE)
 
  accel_files <- dir(accel_fol,  full.names = TRUE, pattern = ".RDS") %>%
    str_subset("historical", negate = TRUE)
  
    load_stack <- function(files) {
      f <- files
      readRDS(f)[[1]]
    }

  vel_stack <- map(vel_files, load_stack)
  accel_stack <- map(accel_files, load_stack)
  
  