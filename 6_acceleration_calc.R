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

  fns_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "") # Timeseries velocity by esm, ssp
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "") # Velocity by term, esm, ssp
  accel_fol <- make_folder(source_disk, "4_acceleration_aus", "") # Aus files
  accel_term_fol <- make_folder(source_disk, "4_acceleration_aus_termsplit", "") # Aus files
  
  
  
  
# Source the velocity functions ------------------------------------------------
  
  dir(fns_fol, full.names = TRUE, pattern = "tempTrend") %>% # Get just the one fn
    map(source)

  
  
  
# Make IPCC term year ranges ---------------------------------------------------
  
  term_list[2:5]
  
  all_range <- 2021:2090 # Whole timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  intermediate_range <- 2061:2080
  long_range <- 2080:2090
  

  # # For tests
  # files <- dir(vocc_fol, full.names = TRUE) %>%
  #   str_subset(., "ssp119", negate = TRUE) %>%
  #   str_subset(., "ssp534-over", negate = TRUE)
  # f <- files[1] # Just pick a file
  # yrs <- as.character(mid_range)
  ssp <- "ssp245"
  # term <- "mid"

  
    
    
# Acceleration function --------------------------------------------------------
  
  get_fastness <- function(ssp) {
    
    files <- dir(vocc_fol, full.names = TRUE, pattern = ssp) %>% # Get the files for the ssp we are going for
      str_subset(., "ssp119|ssp534-over", negate = TRUE) 
    oisst_files <- dir(vocc_term_fol, full.names = TRUE, pattern = "OISST")
    
      do_ssps <- function(f, term) { 
        
        esm <- basename(f) %>% 
          str_split_i(., "_", 4)
        ssp <- basename(f) %>% 
          str_split_i(., "_", 3)
        if(basename(f) %>% str_detect(., "OISST")){
          term <- "historical"
        } else {
          term <- term
        }
        
        # Populate the range of years (corresponding to the relevant term) to subset the full timeseries by 
        if(term == "all") {
          range <- all_range # Full time series
        } else if (term == "historical") {
          range <- recent_range
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
    
      # SSP layers
      ssp_out <- map(term_list[2:5], function(term) {
        message("Processing: ", term, "-term")
        map(files, ~do_ssps(.x, term))
      }) %>%
        flatten() %>% 
        rast()
      saveRDS(ssp_out, file = paste0(accel_fol, "acceleration_yearly_", ssp, "_aus.RDS"))
      # Each file has 53 layers: 1 for the historical OISST layer, and 1 per ESM, SSP, term combo (x52)

      # OISST (historical)
      oisst_out <- do_ssps(oisst_files, "historical") # Not super clean code, this will be re-run 4 times, but ehhh
      saveRDS(oisst_out, file = paste0(accel_fol, "acceleration_yearly_historical_aus.RDS"))
      
      # Term layers
      ssp_term <- map(term_list[2:5], ~{
        tt <- terra::subset(ssp_out, grep(.x, names(ssp_out), value = TRUE))
        saveRDS(tt, file = paste0(accel_term_fol, "acceleration_yearly_", ssp, "_", .x, "-term_aus.RDS"))
      })
        
  }
  
  tic()
  walk(ssp_list, get_fastness)
  toc() # 30 seconds on Alice's machine
  
  
  

  
  