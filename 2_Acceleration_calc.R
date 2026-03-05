# Computing climate acceleration from the velocity layers in script #1
  # Written by Dave S and Alice P
    # 5 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  vocc_fns_fol <- make_folder(source_disk, "_terra_vocc", "") # From Dave
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "") # From Dave
  acc_fol <- make_folder(source_disk, "3_acceleration_global", "") # From Dave
  acc_crop_fol <- make_folder(source_disk, "3_acceleration_aus", "") # From Dave
  
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(vocc_fol, full.names = TRUE)
  f <- files[1] # Just pick a file  

  
  
  
# Make IPCC term year ranges ---------------------------------------------------
  
  term_list
  
  

# Acceleration function --------------------------------------------------------
  
  get_fast <- function(f, yrs){
    
    r <- readRDS(f)
    r <- terra::subset(r, )
    
    
    slope <- tempTrend(f, 3)[[1]]  
    
    
  }
  

  s <- ifel(slope > 10, 10, slope)
  s <- ifel(s < -10, -10, s)
  plot(s[[1]])
    

    
    
# 
