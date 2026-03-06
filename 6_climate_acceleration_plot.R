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
  plot_fol <- make_folder(source_disk, "4_acceleration_aus_plot", "") # Aus files
  
  # acc_global_fol <- make_folder(source_disk, "3_acceleration_global", "") # Global files
  
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(acc_fol, full.names = TRUE)
  files
  
  
  
  
# Get median acceleration (slope) per 1° latitude ------------------------------
  
  a <- readRDS(files[1])
  a
r <- a
  
  slope_lat <- function(r) {
    
  # Get lat for each cell
  lat_df <- crds(r, df = TRUE, na.rm = FALSE) %>% # Keep NAs so same number of cols
    as_tibble() %>% # don't want a df
    mutate(lat_band = ceiling(y)) # Round up to 1° band (cus we're in negatives)
  
  # Get values for all layers, pivot and summarise
  vals <- values(r, dataframe = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(lat_df) %>% 
    pivot_longer(cols = -c(x, y, lat_band), 
                 names_to = "layer", 
                 values_to = "slope") %>% 
    group_by(lat_band, layer) %>% 
    summarise(median_slope = median(slope, na.rm = TRUE), .groups = "drop") %>% 
    mutate(
      ssp  = str_split_i(layer, "_", 2),
      esm  = str_split_i(layer, "_", 3),
      term = str_split_i(layer, "_", 4)
    ) %>% 
    dplyr::select(lat = lat_band, median_slope, ssp, term, esm)
  
  }
  
  out <- slope_lat(a)
  
  
  
  
  
  