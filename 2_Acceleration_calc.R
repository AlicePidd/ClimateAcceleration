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

  vocc_fns_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "") # Timeseries velocity by esm, ssp
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "") # Velocity by term, esm, ssp
  acc_fol <- make_folder(source_disk, "3_acceleration_aus", "") # Aus files
  # acc_global_fol <- make_folder(source_disk, "3_acceleration_global", "") # Global files
  
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(vocc_fol, full.names = TRUE) %>% 
    str_subset(., "ssp119", negate = TRUE) %>% 
    str_subset(., "ssp534-over", negate = TRUE)
  f <- files[1] # Just pick a file  

  
  
  
# Make IPCC term year ranges ---------------------------------------------------
  
  term_list[2:5]
  
  all_range <- 2021:2090 # Whole timeseries
  near_range <- 2021:2040 
  mid_range <- 2041:2060
  int_range <- 2061:2080
  long_range <- 2080:2090
  
  yrs <- as.character(near_range)

  

# Acceleration function --------------------------------------------------------
  
  get_fast <- function(f, term){
    
    r <- readRDS(f) %>% 
      terra::subset(., str_detect(names(.), paste(near_range, collapse = "|")))

    slope <- tempTrend(r, 3)[[1]]  
    
  }
  

  s <- ifel(slope > 10, 10, slope)
  s <- ifel(s < -10, -10, s)
  plot(s[[1]])
    

    
    
# 
