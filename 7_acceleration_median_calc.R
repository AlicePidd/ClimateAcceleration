# Computing median climate velocity across ALL 11 ESMs, NOT an ensemble, just the median
  # Written by Alice P
    # 7 March 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  accel_fol <- make_folder(source_disk, "4_acceleration_aus_termsplit", "")
  median_fol <- make_folder(source_disk, "5_accleration_median_terms", "")

  
  
# Get the median climate velocity across all ESMs together, per SSP-term combo -------------

  files <- dir(accel_fol, full.names = TRUE) %>% 
    str_subset("historical", negate = TRUE)
  files

  get_median <- function(f) {
    ssp <- basename(f) %>% 
      str_split_i(., "_", 3)
    term <- basename(f) %>% 
      str_split_i(., "_", 4)
    
    message("Processing: ", ssp, ", ", term)

    r <- rast(f)
    r_median <- app(r, median, na.rm = TRUE) # Median across ESMs
    r_min <- app(r, min, na.rm = TRUE) # Min across ESMs
    r_max <- app(r, max, na.rm = TRUE) # Max across ESMs

    full_stack <- c(r_median, r_min, r_max) # Stack them together
    names(full_stack) <- c(paste0(names(r_median), "_", ssp, "_", term),
                           paste0(names(r_min), "_", ssp, "_", term),
                           paste0(names(r_max), "_", ssp, "_", term))
    o_nm <- paste0(median_fol, "acceleration_median_", ssp, "_", term, ".RDS")
    saveRDS(full_stack, o_nm)
  }
  
  walk(files, get_median)
  
  
  
  
  
