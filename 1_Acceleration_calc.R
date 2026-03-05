# Computing velocity and climate acceleration from annual OISST layers
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
  vocc_fol <- make_folder(source_disk, "2_vocc_rolling_annual", "") # From Dave
  acc_fol <- make_folder(source_disk, "3_acceleration_global", "") # From Dave
  acc_crop_fol <- make_folder(source_disk, "3_acceleration_aus", "") # From Dave
  
  
  
  
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

  compute_acceleration <- function () {
  
    files <- dir(oisst_fol, full.names = TRUE)
    f <- files[67] # Just pick a file
    indexing <- 1:70 # So it knows what year to name each layer
    # i = 1
    
    get_velocity <- function(f, yrs, i) {
      
      esm <- basename(f) %>% 
        str_split_i(., "_", 3)
      ssp <- basename(f) %>% 
        str_split_i(., "_", 4)
      nm_yr <- map(years, `[[`, 11)[i] # get the year at teh 11th position in the list of years, i.e. the year we are computing velocity for

      compute <- function(i) {
        spt_raster <- rast(f) %>% 
          crop(., e1)
        spt_raster <- mask(spt_raster, raus)
        names(spt_raster) <- time(spt_raster) %>% 
          year()
        
        r <- terra::subset(spt_raster, yrs %>% unlist() %>% as.character()) # Get only the data for the years we need
        spt_grad <- spatGrad(r)
        tmp_trend <- tempTrend(r, 3)
        out <- gVoCC(tmp_trend, spt_grad) %>% 
          terra::subset(1)
        names(out) <- paste0(nm_yr, "_", ssp, "_", esm)
        return(out)
      }
      
      result <- map(indexing, compute)
      # bits <- basename(f) %>% str_split(., "_")
      # saveRDS(result, file = paste0(vocc_fol, "vocc_yearly_", bits[[1]][4], "_", bits[[1]][3], "_", min(range), "-", max(range), ".RDS"))
      
    }
    # indexing <- 1:70 # So it knows what year to name each layer
    
    vels <- map(files[66:67], ~get_velocity(.x, years, indexing)) #%>% 
      rast() # Stacks it
    vels
    

  }
  
  
    
  ## Acceleration function ---------------
  
    slope <- tempTrend(vels, 3)  
    s <- ifel(slope > 100, 100, slope)
    s <- ifel(s < -100, -100, s)
    plot(s)
      

    
    
# 
