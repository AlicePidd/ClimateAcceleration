# Plotting climate acceleration spatially
  # Written by Alice P
    # 6 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 



# Ideas: 
  # Acceleration divergence from previous term? Like an anomaly
  # Plotting acceleration spatially as normal, but only show 245 mid term, with rest in sups
  # Latitudinal acceleration per SSP - median of all ESMs as the main line, ribbons as min-max across ESMs, lines per SSP, plot per term?
  # 



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  median_fol <- make_folder(source_disk, "5_accleration_median_terms", "")


  
  
# Get 95th percentile of the data and truncate by that value -------------------
  
  files <- dir(median_fol, full.names = TRUE)
  files
  

  all_r <- map(files, ~{
    readRDS(.x)[[1]]
  }) %>% 
    rast()
  
  names(all_r)
  
  max(all_r)
  min(all_r)
  median(all_r)
  range(all_r)
  q <- quantile(all_r, 0.95)
  q

  
  

# Plot spatially per SSP-term combination --------------------------------------

  
  ggplot() +
    tidyterra::geom_spatraster(data = r) +
    theme_minimal()
  
  
  
  
