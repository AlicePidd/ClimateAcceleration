# Computing velocity from annual OISST layers
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

  sst_fol <- make_folder(source_disk, "1_CMIP_regridded_OISST_DBC_sst_annual_Aus", "") # From Dave
  vocc_fns_fol <- make_folder(source_disk, "_terra_vocc", "") # From VoCC package
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "")
  vocc_term_fol <- make_folder(source_disk, "2_vocc_rolling_annual_termsplit", "")
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(sst_fol, full.names = TRUE)
  # f <- files[67] # Just pick a file
  
  
  

# A sliding window and get the years for one period ----------------------------
  
  make_rolling_seq <- function(yr) {
    seq(yr-10, yr+10, by = 1)
  }
  
  all_range <- 2021:2090 # Projection timeseries
  recent_range <- 1995:2014 # OISST (reality) term
  near_range <- 2021:2040 # Projection terms
  mid_range <- 2041:2060
  int_range <- 2061:2080
  long_range <- 2080:2090
  
  all_years <- map(all_range, make_rolling_seq)
  recent_years <- map(recent_range, make_rolling_seq)
  near_years <- map(near_range, make_rolling_seq)
  mid_years <- map(mid_range, make_rolling_seq)
  int_years <- map(int_range, make_rolling_seq)
  long_years <- map(long_range, make_rolling_seq)
  

  
  
# Source the velocity functions ------------------------------------------------
  
  dir(vocc_fns_fol, full.names = TRUE) %>% 
    map(source)

  

# Compute velocity -------------------------------------------------------------
    # Compute grid cell velocity for each period, by calculating the rolling velocity for the surrounding 10 years of each year that is within each period (i.e., to calculate the velocity for the first year in the near-term, 2021, we calculate the velocities each of the 10 years prior and after, so between 2011-2031, with 2021 being the middle year)
    # Use these rolling velocities for each year in each period to compute the acceleration of each period as the slope of those velocities

  files <- dir(sst_fol, full.names = TRUE)
  # term = "mid"
  # f <- files[67]
  
  get_velocity <- function(f, yrs, term) {
    
    esm <- basename(f) %>% 
      str_split_i(., "_", 3)
    ssp <- basename(f) %>% 
      str_split_i(., "_", 4)
    term <- term
    
    if(term == "near") {
      yrs <- near_years
      range <- near_range
    } else if(term == "mid") {
      yrs <- mid_years
      range <- mid_range
    } else if(term == "intermediate") {
      yrs <- int_years
      range <- int_range
    } else if(term == "long") {
      yrs <- long_years
      range <- long_range
    } else if(term == "all") {
      yrs <- long_years
      range <- long_range
    }

    indexing <- seq_along(yrs)
    
    spt_raster <- rast(f) %>% 
      crop(., e1) # Crop
    spt_raster <- mask(spt_raster, raus) # Mask
    names(spt_raster) <- time(spt_raster) %>% 
      year()
    
      compute <- function(i) {
        r <- terra::subset(spt_raster, yrs[[i]] %>% unlist() %>% as.character()) # Get only the data for the years we need
        spt_grad <- spatGrad(r)
        tmp_trend <- tempTrend(r, 3)
        out <- gVoCC(tmp_trend, spt_grad) %>% 
          terra::subset(1)
        nm_yr <- yrs[[i]][11] # The year we're calculating for
        names(out) <- paste0(nm_yr, "_", ssp, "_", esm)
        return(out)
      }
    
    result <- map(indexing, compute) %>% 
      rast()
    saveRDS(result, 
            if(term == "near" || term == "mid" || term == "intermediate" || term == "long") {
              file = paste0(vocc_term_fol, "vocc_yearly_", ssp, "_", esm, "_", term, "_", min(range), "-", max(range), ".RDS")
            } else if(term == "all") {
              file = paste0(vocc_fol, "vocc_yearly_", ssp, "_", esm, "_", min(range), "-", max(range), ".RDS")
              })
  }
  
  # Full projected time series 2015-2100
  tic()
  walk(files, ~get_velocity(.x, years, "all")) 
  toc() # 31 min per file on Alice's machine

  # By terms
  tic()
  walk(term_list[2:5], function(term) {
    message("Processing: ", term, "-term")
    walk(files, ~get_velocity(.x, years, term))
  })
  toc()
  
    # tic()
    # walk(files, ~get_velocity(.x, years, "near")) 
    # toc() # 9 min per term on Alice's machine
  
  