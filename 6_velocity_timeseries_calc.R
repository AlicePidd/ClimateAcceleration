# Get the median velocity across all ESMs per year
  # Written by Alice P
    # 2nd April 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") 
  vocc_med_timeseries_fol <- make_folder(source_disk, "3_velocity_decadal_median_timeseries", "rasts") 

  
  esm_files <- dir(vocc_fol, full.names = TRUE) %>% 
    str_subset(., pattern = "historical", negate = TRUE)
  esm_files
  hist_files <- dir(vocc_fol, full.names = TRUE, pattern = "historical")
  hist_files

  
  
  
# Get median velocity per year and SSP, across all ESMs ------------------------
  
  files <- esm_files
  # ssp <- "ssp245"
  # term <- "intermediate"
  
  get_median_timeseries <- function(files, ssp, term) {
    
    sub_f <- files %>% 
      str_subset(., ssp) %>%
      str_subset(., term)
    
    rast_list <- map(sub_f, readRDS) # Read all into a list
    
    # Get median across ESMs for each layer
    n_layers <- nlyr(rast_list[[1]])
    median_rast <- map(1:n_layers, function(l) { # For every layer in each rast
      rast_list %>%
        map(function(r) r[[l]]) %>% # Subset it
        rast() %>% # stack them
        app(fun = median, na.rm = TRUE) # compute median 
    }) %>%
      rast()
    
    yr1 <- names(rast_list[[1]])[1] %>% str_split_i(., "_", 1) %>% as.numeric()
    yr2 <- yr1 + n_layers - 1
    names(median_rast) <- paste0(yr1:yr2, "_med_dec_velocity_", ssp)
    
    saveRDS(median_rast, paste0(vocc_med_timeseries_fol, "/velocity_decadal_median_rast_", ssp, "_", term, "_", yr1, "-", yr2, ".RDS"))
  }  
  
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])  
  pwalk(combos, function(ssp, term) get_median_timeseries(esm_files, ssp, term))  
  
  
  
  
# Copy the historical file to the same folder (no cal needed) ------------------
  
  file.copy(hist_files, paste0(vocc_med_timeseries_fol, "/velocity_decadal_median_rast_historical_recent_1995-2014.RDS"))
  



