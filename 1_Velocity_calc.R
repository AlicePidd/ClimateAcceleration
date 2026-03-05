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

  oisst_fol <- make_folder(source_disk, "1_CMIP_regridded_OISST_DBC_sst_annual_Aus", "") # From Dave
  vocc_fns_fol <- make_folder(source_disk, "_terra_vocc", "") # From Dave
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "")
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(oisst_fol, full.names = TRUE)
  f <- files[67] # Just pick a file
  
  
  

# A sliding window and get the years for one period ----------------------------
  
  make_rolling_seq <- function(yr) {
    seq(yr-10, yr+10, by = 1)
  }
  
  range <- 2021:2090
  years <- map(range, make_rolling_seq)
  years
  
  
  # yrs <- years
  

  
  
# Load velocity functions ------------------------------------------------------
  
  dir(vocc_fns_fol, full.names = TRUE) %>% 
    map(source)

  
  ## Velocity function ---------------
    # Compute grid cell velocity for each period, by calculating the rolling velocity for the surrounding 10 years of each year that is within each period (i.e., to calculate the velocity for the first year in the near-term, 2021, we calculate the velocities each of the 10 years prior and after, so between 2011-2031, with 2021 being the middle year)
    # Use these rolling velocities for each year in each period to compute the acceleration of each period as the slope of those velocities


    files <- dir(oisst_fol, full.names = TRUE)
    # f <- files[67] # Just pick a file
    indexing <- 1:length(years) # So it knows what year to name each layer
    # i = 1
    
    get_velocity <- function(f, yrs, i) {
      
      esm <- basename(f) %>% 
        str_split_i(., "_", 3)
      ssp <- basename(f) %>% 
        str_split_i(., "_", 4)
      indexing <- seq_along(yrs)
      # nm_yr <- map(years, `[[`, 11)[i] # get the year at teh 11th position in the list of years, i.e. the year we are computing velocity for
      
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
              file = paste0(vocc_fol, "vocc_yearly_", ssp, "_", esm, "_", min(range), "-", max(range), ".RDS"))
      
    }
    
    tic()
    walk(files[67], ~get_velocity(.x, years, indexing)) 
    toc() # 4 min per file in BigMac


    
  # ## Checking out why there are -Inf and Inf values... -----------------
  #   
  #   e <- readRDS("/Volumes/AliceShield/acceleration_data/2_vocc_rolling_annual/vocc_yearly_ssp585_NorESM2-MM_2021-2090.RDS")
  #   plot(e)
  #   ee <- ifel(e != Inf & e != -Inf, NA, 1)
  #   plot(ee, col = "red")
  #   
  #   t <- rast("/Volumes/AliceShield/acceleration_data/1_CMIP_regridded_OISST_DBC_sst_annual_Aus/tos_Oyear_NorESM2-MM_ssp585_r1i1p1f1_RegriddedBiasCorrectedAus_19820101-21001231.nc")
  #   names(t) <- time(t) %>% 
  #     year()
  #   yrs <- as.character(2011:2050)
  #   t <- terra::subset(t, yrs)
  #   plot(t)
  #   min(t)
  #   max(t)
  #   tt <- is.na(t)
  #   plot(tt[[1:10]])
  #   plot(tt[[11:20]])
  #   plot(tt[[21:30]])
  #   plot(tt[[31:40]])
    