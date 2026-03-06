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

  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "") # Timeseries velocity by esm, ssp
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "") # Velocity by term, esm, ssp
  acc_fol <- make_folder(source_disk, "3_acceleration_aus", "") # Aus files
  # acc_global_fol <- make_folder(source_disk, "3_acceleration_global", "") # Global files

  
  
  
# Make IPCC term year ranges ---------------------------------------------------
  
  term_list[2:5]
  
  all_range <- 2021:2090 # Whole timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  int_range <- 2061:2080
  long_range <- 2080:2090
  

  # # For tests
  # files <- dir(vocc_fol, full.names = TRUE) %>%
  #   str_subset(., "ssp119", negate = TRUE) %>%
  #   str_subset(., "ssp534-over", negate = TRUE)
  # f <- files[1] # Just pick a file
  # yrs <- as.character(mid_range)
  # ssp <- "ssp126"
  # term <- "mid"

  
    
    
# Acceleration function --------------------------------------------------------
  
  get_fastness <- function(ssp) {
    
    files <- dir(vocc_fol, full.names = TRUE, pattern = ssp) %>% # Get the files for the ssp we are going for
      str_subset(., "ssp119", negate = TRUE) %>% 
      str_subset(., "ssp534-over", negate = TRUE) # Don't want these files (overshoot or 119)
    
      do_ssps <- function(f, term) { 
        
        esm <- basename(f) %>% 
          str_split_i(., "_", 4)
        ssp <- basename(f) %>% 
          str_split_i(., "_", 3)
        term <- term
        
        # Populate the range of years (corresponding to the relevant term) to subset the full timeseries by 
        if(term == "all") {
          range <- all_range # Full time series
        } else {
          range <- get(paste0(term, "_range")) # Series of years in the term
        }
        
        # Subset timeseries
        r <- readRDS(f) %>% 
          terra::subset(., str_detect(names(.), paste(range, collapse = "|"))) # Get only the relevant layers per the term years
        
        # Calculate the slope i.e., acceleration of velocity, for each ssp, term, and esm
        slope <- tempTrend(r, 3)[[1]] 
        names(slope) <- paste0("slope_", ssp, "_", esm, "_", term) # Name the layers accordingly
        return(slope)
      }
    
    out <- map(term_list[2:5], function(term) {
      message("Processing term: ", term)
      map(files, ~do_ssps(.x, term))
    }) %>% 
      flatten() %>%  # Flatten the nested list (terms x files) into a single list (Claude's idea)
      rast() # Stack it
    saveRDS(out, file = paste0(acc_fol, "acceleration_yearly_", ssp, "_aus.RDS")) # Save each SSP file
      # Each file has 52 layers: 1 per ESM, SSP, term combo
  }
  
  tic()
  walk(ssp_list, get_fastness)
  toc()
  

    