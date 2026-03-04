# Looking at velocity 
    # Written by Alice Pidd
        # Mar 2026


# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"

  
  
# Folders and data -------------------------------------------------------------

  mag_fol <- make_folder(source_disk, "0_VoCC_mag_layers", "") # Alice
  tos_fol <- make_folder(source_disk, "tos_yearmean", "") # Dave
  
  
  
# Dave way ---------------------------------------------------------------------
  # Calculating acceleration between t2 and t1, as the slope of the line (rate of change) along a 20-year series of climate velcoity, rolling by 1 year (i.e., slope of velocity of )
  
  # Need to:
    # Get a list of 20-year blocks, rolling forward by 1 year
    # isolate each block
    # calculate the slope of velocity for data in each block
    # Save it
  
  
  files <- dir(tos_fol, full.names = TRUE)
  files  
  
    # start_yr <- basename(files[1]) %>% 
    #   str_split_i(., "_", 7) %>% 
    #   str_remove(., ".nc") %>% 
    #   str_split_i(., "-", 1) %>% 
    #   str_sub(., start = 1L, end = 4L) %>%  # Get the chrs in these positions
    #   as.numeric()
    # 
    # end_yr <- basename(files[1]) %>% 
    #   str_split_i(., "_", 7) %>% 
    #   str_remove(., ".nc") %>% 
    #   str_split_i(., "-", 2) %>% 
    #   str_sub(., start = 1L, end = 4L) %>% # Get the chrs in these positions
    #   as.numeric()
  
  
  
  ## Making a rolling sequence for the 21 year series for which I compute the slope of velocity per series ------------
  
    make_rolling_seq <- function(i, start_yr, end_yr) {
      yr_seq <- seq(start_yr, end_yr, by = 1)
      start <- yr_seq[i]
      end <- yr_seq[i] + 20 # Only + 20 not 21 here because we include the starting year
      roll <- tibble(start, end) # Put it in a tibble
      return(roll)
    }
    
    nums <- seq(1, 86, by = 1) # 86 because anything after this would create a block outside 2100
    rolling_seq <- make_rolling_seq(nums, 1995, 2100) %>% 
      list()
    saveRDS(rolling_seq, file = paste0(helper_fol, "/rolling_sequence.RDS"))
  
  
    
  ## Get only the data in each block ------------
    
    r <- rast(files[1])
    r
    
    yrs <- seq(1995, 2100, by = 1)
    names(r) <- yrs

      
  

# Alice way --------------------------------------------------------------------  
  # Calculating acceleration between t2 and t1 using the equation a = (vf - vi) / ∆t
    # For when I calculate it between 
  
  
  ## Load velocity layers ------------------------------------------------------
    
    files <- dir(mag_fol, full.names = TRUE, pattern = "ACCESS-CM2")
    files  
    
    p1 <- rast(files[22])[[1]] %>% 
      crop(e1) %>% 
      mask(., raus)
    
    p2 <- readRDS(files[21])[[1]] %>% 
      crop(e1) %>% 
      mask(., raus)
    
    p3 <- readRDS(files[19])[[1]] %>% 
      crop(e1) %>% 
      mask(., raus)
    
    p4 <- readRDS(files[20])[[1]] %>% 
      crop(e1) %>% 
      mask(., raus)
    
    
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
  

  ## Compute acceleration for periods ------------------------------------------
    # Midpoints (medians) for each period are 20-years apart, so ∆t = 20
    # For rolling acceleration calc on YEARLY velocity, the midpoints will be 1 year apart (midpoint of 1995-2014 is 2004.5, midpoint of 1996-2015 is 2005.5, so ∆t = 1), so the acceleration calc will be the difference
    acc1 <- (p2-p1)/20
    names(acc1) <- "near to mid"
    # acc1 <- ifel(voccMag <-100)
    acc2 <- (p3-p2)/20
    names(acc2) <- "mid to intermediate"
    
    acc3 <- (p4-p3)/20
    names(acc3) <- "intermediate to long"
  
    plot(c(acc1, acc2, acc3))
  
  
  
  
  
  
  