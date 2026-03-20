# Computing median climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  accel_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "")
  esm_fol <- make_folder(source_disk, "5_accleration_aus_df_ESMs_terms", "")
  median_fol <- make_folder(source_disk, "5_accleration_aus_df_median_terms", "")

  
  
  
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  files <- dir(accel_fol, full.names = TRUE) %>% 
    str_subset("historical", negate = TRUE)
  files

  get_accel_dfs <- function(f) {
    ssp <- basename(f) %>% 
      str_split_i(., "_", 2)
    term <- basename(f) %>% 
      str_split_i(., "_", 3)
    
    message("Processing: ", ssp, ", ", term)

    # Make df of acceleration per ESM, for each SSP-term combo, and keep the lat/lon
    df1 <- r %>% 
      as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
      as_tibble() %>% 
      rename(lon = 1, lat = 2) %>% 
      pivot_longer(cols = -c(lon, lat),
                   names_to  = "col_name",
                   values_to = "accel") %>% 
      mutate(ssp  = str_split_i(col_name, "_", 2),
             esm  = str_split_i(col_name, "_", 3),
             term = str_split_i(col_name, "_", 4) %>% paste0(., "-term")) %>% 
      dplyr::select(-col_name)
    df1_nm <- paste0(esm_fol, "acceleration_ESMs_df_", ssp, "_", term, ".RDS")
    saveRDS(df1, df1_nm)
    
    # Get median acceleration and the IQR for each SSP-term combo
      # Each row is one 0.25° pixel with its median, Q25, and Q75 across the 11 ESMs
    df2 <- df1 %>% 
      group_by(lon, lat, ssp, term) %>% 
      summarise(median_accel = median(accel, na.rm = TRUE),  
                q25 = quantile(accel, 0.25, na.rm = TRUE), 
                q75 = quantile(accel, 0.75, na.rm = TRUE), 
                .groups = "drop") %>% 
      mutate(ssp = ssp, term = term)
    df2_nm <- paste0(median_fol, "acceleration_median_df_", ssp, "_", term, ".RDS")
    saveRDS(df2, df2_nm)
    
  }
  
  tic()
  walk(files, get_accel_dfs)
  toc() # Takes 32 sec on Alice's machine

  
  