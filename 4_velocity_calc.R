# Computing velocity from annual OISST layers
  # Written by Dave S and Alice P
    # 5 March 2026




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  sst_fol <- make_folder(source_disk, "1_CMIP_regridded_OISST_DBC_sst_annual_Aus", "") # From Dave
  oisst_fol <- make_folder(source_disk, "1_OISST_annual_Aus", "") # From Dave
  fn_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_annual", "")
  vocc_term_fol <- make_folder(source_disk, "2_velocity_rolling_annual_terms", "")
  
  
  

# A sliding window and get the years for one period ----------------------------
  
  make_rolling_seq <- function(yr) {
    seq(yr-10, yr+10, by = 1)
  }
  
  all_range <- 2021:2090 # Projection timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  intermediate_range <- 2061:2080
  long_range <- 2080:2090
  
  all_years <- map(all_range, make_rolling_seq)
  recent_years <- map(recent_range, make_rolling_seq)
  near_years <- map(near_range, make_rolling_seq)
  mid_years <- map(mid_range, make_rolling_seq)
  intermediate_years <- map(intermediate_range, make_rolling_seq)
  long_years <- map(long_range, make_rolling_seq)
  

  
  
# Source the velocity functions ------------------------------------------------
  
  dir(fn_fol, full.names = TRUE) %>% 
    map(source)

  

# Compute velocity -------------------------------------------------------------
    # Compute grid cell velocity for each period, by calculating the rolling velocity for the surrounding 10 years of each year that is within each period (i.e., to calculate the velocity for the first year in the near-term, 2021, we calculate the velocities each of the 10 years prior and after, so between 2011-2031)
    # Use these rolling velocities for each year in each period to compute the acceleration of each period as the slope of those velocities

  files <- dir(sst_fol, full.names = TRUE) %>% 
    str_subset(., "ssp119|ssp534-over|CESM2-WACCM|GFDL-ESM4", negate = TRUE) # None of these files
  oisst_files <- dir(oisst_fol, full.names = TRUE)
  
  term = "mid"
  # f <- files[67]
  # f <- oisst_files
  
  
  get_velocity <- function(f, yrs, term) {
    
    esm <- basename(f) %>% 
      str_split_i(., "_", 3)
    ssp <- basename(f) %>% 
      str_split_i(., "_", 4)
    term <- term
    
    if(term == "all") {
      range <- all_range # Full time series
      yrs <- all_years
    } else {
      range <- get(paste0(term, "_range")) # Series of years in the term
      yrs   <- get(paste0(term, "_years")) # Years +10 and -10 either side of the term range
    }

    indexing <- seq_along(yrs)
    
    spt_raster <- rast(f) %>% 
      crop(., e1) # Crop
    spt_raster <- mask(spt_raster, mask_land) # Mask
    names(spt_raster) <- time(spt_raster) %>% 
      year()
    
      compute <- function(i) {
        r <- terra::subset(spt_raster, yrs[[i]] %>% unlist() %>% as.character()) # Get only the data for the years we need
        spt_grad <- spatGrad(r) # How quickly the SST changes across space (°C/km) i.e., how steep is the temp landscape at each grid cell
        tmp_trend <- tempTrend(r, 3) # How quickly SST is changing through time (°C/year) i.e., how fast is temp changing over time at each grid cell
        out <- gVoCC(tmp_trend, spt_grad) %>% # velocity = (°C/year) / (°C/km) = km/year
          terra::subset(1) # Only need VoCC not the other layers
        nm_yr <- yrs[[i]][11] # The year we're calculating for
        
        if(term == "recent") {
          names(out) <- paste0(nm_yr, "_historical_", esm)
        } else {
          names(out) <- paste0(nm_yr, "_", ssp, "_", esm)
        }
        return(out)
      }
    
    result <- map(indexing, compute) %>% 
      rast()
    saveRDS(result, 
            if(term == "all") {
              paste0(vocc_fol, "vocc_yearly_", ssp, "_", esm, "_", min(range), "-", max(range), ".RDS")
            } else if (term == "recent") {
              paste0(vocc_term_fol, "vocc_yearly_historical_", esm, "_", term, "_", min(range), "-", max(range), ".RDS")
            } else {
              paste0(vocc_term_fol, "vocc_yearly_", ssp, "_", esm, "_", term, "_", min(range), "-", max(range), ".RDS")
            })
  }
  
  # Full projected time series 2015-2100
  tic()
  walk(files, ~get_velocity(.x, years, "all")) 
  toc() # 29 min on Alice's machine

  # By terms
  tic()
  walk(term_list[2:5], function(term) { # No recent-past yet
    message("Processing: ", term, "-term")
    walk(files, ~get_velocity(.x, years, term))
  })
  toc() # 30 mins on Alice's machine

  # For the recent term (using OISST data instead of CMIP)
  tic()
  walk(oisst_files, ~get_velocity(.x, years, "recent")) 
  toc() # 9 sec
  
  
  