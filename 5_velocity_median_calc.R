# Computing median DECADAL climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  # vocc_term_yr_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "annual")
  vocc_term_dec_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") # Yearly climate velocity (km/year)
  esm_fol <- make_folder(source_disk, "3_velocity_decadal_ESMs_terms", "dfs")
  median_df_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "dfs") 
  median_rast_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts") 


    
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  ## Projected data files ------------
  
  files <- dir(vocc_term_dec_fol, full.names = TRUE, pattern = ".RDS") %>% 
    str_subset("historical", negate = TRUE)
  files
  
  
  ## Do it ------------
  
    get_velocity_dfs <- function(ssp, term, files, rate) {
      message("Processing: ", ssp, ", ", term)
      
      # Get all ESM files for this SSP-term combo
      combo_files <- files %>%
        str_subset(ssp) %>%
        str_subset(term)
      
      # Make df of velocity per ESM, for each SSP-term combo, and keep the lat/lon
        df1 <- map_dfr(combo_files, function(f) {
          esm_name <- basename(f) %>% str_split_i("_", 4)
          readRDS(f) %>%
            as.data.frame(xy = TRUE, na.rm = TRUE) %>%
            as_tibble() %>%
            rename(lon = 1, lat = 2) %>%
            pivot_longer(cols      = -c(lon, lat),
                         names_to  = "col_name",
                         values_to = "velocity") %>%
            mutate(ssp  = str_split_i(col_name, "_", 2),
                   esm  = esm_name,
                   term = paste0(term, "-term"),
                   rate = rate) %>%
            dplyr::select(-col_name)
        })
        saveRDS(df1, paste0(esm_fol, "/velocity_", rate, "_ESMs_df_", ssp, "_", term, ".RDS"))
  
      # Get median velocity and Q25/Q75 for each SSP-term combo
        df2 <- df1 %>%
          group_by(lon, lat, ssp, term) %>%
          summarise(median_velocity = median(velocity, na.rm = TRUE),
                    q25             = quantile(velocity, 0.25, na.rm = TRUE),
                    q75             = quantile(velocity, 0.75, na.rm = TRUE),
                    .groups         = "drop")
        saveRDS(df2, paste0(median_df_fol, "/velocity_", rate, "_median_df_", ssp, "_", term, ".RDS"))
  
      # Get median velocity and the Q25/Q75 but keep it as a rast
        r_stack <- map(combo_files, readRDS) %>% rast()
        r_median <- app(r_stack, median, na.rm = TRUE)
        r_q25 <- app(r_stack, \(x) quantile(x, 0.25, na.rm = TRUE)) # \(x) is a cool way of doing function(x) with LESS TYPING! Doesn't work with old R though.
        r_q75 <- app(r_stack, \(x) quantile(x, 0.75, na.rm = TRUE))
        
        out_stack <- c(r_median, r_q25, r_q75)
        names(out_stack) <- c(paste0("median_", ssp, "_", term),
                              paste0("q25_", ssp, "_", term),
                              paste0("q75_",    ssp, "_", term))
        saveRDS(out_stack, paste0(median_rast_fol, "/velocity_", rate, "_median_rast_", ssp, "_", term, ".RDS"))
    }
    
    combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])
    combos
    tic()
    walk2(combos$ssp, combos$term, get_velocity_dfs,
          files = files, rate = "decadal")
    toc()
    beep(2)

    
    
    
  ## Do it for the OISST file -----------
    
    hist_files <- dir(vocc_term_dec_fol, full.names = TRUE, pattern = ".RDS") %>% 
      str_subset("historical")
    hist_files # Should just be the one
    
    # single file, no median across ESMs
      r <- readRDS(hist_files)
      r
    
    # Mask to EEZ
      r <- mask(r, reez)
      crs(r) <- "EPSG:4326"
      plot(r)
    
    # df version 
      ## (same structure as df2 from the projected data)
    df_hist <- r %>%
      as.data.frame(xy = TRUE, na.rm = TRUE) %>%
      as_tibble() %>%
      rename(lon = 1, lat = 2) %>%
      pivot_longer(cols = -c(lon, lat),
                   names_to = "col_name",
                   values_to = "velocity") %>%
      mutate(ssp  = "historical",
             term = "recent-term",
             rate = "decadal") %>%
      dplyr::select(-col_name)
    df_hist
    saveRDS(df_hist, paste0(median_df_fol, "/velocity_decadal_median_df_historical_recent.RDS"))
        
    # Rast version
      ## since single obs product, median is just the OG value
    names(r) <- paste0("median_historical_recent_", 1995:2014)
    saveRDS(r, paste0(median_rast_fol, "/velocity_decadal_median_rast_historical_recent.RDS"))
    