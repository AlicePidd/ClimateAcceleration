# Plotting climate acceleration by 1° latitude
  # Written by Alice P
    # 6 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 




# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  acc_fol <- make_folder(source_disk, "3_acceleration_aus", "") # Aus files
  plot_fol <- make_folder(source_disk, "4_acceleration_aus_plot", "") # Aus files
  
  # acc_global_fol <- make_folder(source_disk, "3_acceleration_global", "") # Global files
  
  
  
  
# Get files --------------------------------------------------------------------
  
  files <- dir(acc_fol, full.names = TRUE)
  files
  
  f <- files[1]
  
  
# Get median acceleration (slope) per 1° latitude ------------------------------

  files <- dir(acc_fol, full.names = TRUE)

  slope_lat <- function(f) {
    
    r <- readRDS(f)
    
    # Get lat for each cell
    lat_df <- crds(r, df = TRUE, na.rm = FALSE) %>% # Keep NAs so same number of cols
      as_tibble() %>% # don't want a df
      mutate(lat_band = ceiling(y)) # Round up to 1° band (cus we're in negatives)
    
    # Get values for all layers, pivot and summarise
    vals <- values(r, dataframe = TRUE) %>% 
      as_tibble() %>% 
      bind_cols(lat_df) %>% 
      pivot_longer(cols = -c(x, y, lat_band), 
                   names_to = "layer", 
                   values_to = "slope") %>% 
      group_by(lat_band, layer) %>% 
      summarise(median_slope = median(slope, na.rm = TRUE), 
                # median_slope = median(slope[is.finite(slope)], na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(ssp  = str_split_i(layer, "_", 2),
             esm  = str_split_i(layer, "_", 3),
             term = str_split_i(layer, "_", 4)) %>% 
      dplyr::select(lat = lat_band, median_slope, ssp, term, esm)
    
    return(vals)
  }
  
  out <- map(files, slope_lat) %>% 
    bind_rows %>% 
    mutate(term = if_else(term == "historical", "recent", term)) %>% 
    { bind_rows( # ChatGPT helped to duplicate the recent term for each SSP
      filter(., ssp != "OISST"),
      expand_grid(ssp = c("ssp126", "ssp245", "ssp370", "ssp585"),
                  filter(., ssp == "OISST") %>% 
                    dplyr::select(-ssp))
      )}

  
  
# Plot -------------------------------------------------------------------------
  
  # pal <- c(col_ssp126, col_ssp245, col_ssp370, col_ssp585)
  
  out_summary <- out %>% 
    group_by(lat, ssp, term) %>% 
    summarise(
      median_slope = median(median_slope, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  recent_rows <- out_summary %>% 
    filter(ssp == "historical") %>% 
    dplyr::select(-ssp) # Otherwise it plots it as historical, not with the ssps
  
  # out_summary <- out_summary %>% 
  #   filter(ssp != "historical") %>% 
  #   bind_rows(expand_grid(ssp = c("ssp126", "ssp245", "ssp370", "ssp585"), recent_rows))
  
  out_summary <- out_summary %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  ssp_summary <- out_summary %>% 
    filter(ssp != "historical")

  # Checking the spread for the lims
  range(ssp_summary$median_slope) # -0.6827282  0.6266832

  ggplot() +
    geom_tile(data = recent_rows, aes(x = term, y = lat, fill = median_slope)) +
    geom_tile(data = ssp_summary, aes(x = term, y = lat, fill = median_slope)) +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), oob = scales::squish) +
    facet_wrap(~ssp, nrow = 1) +
    scale_y_continuous(breaks = seq(-50, -5, by = 10)) +
    labs(fill = "Acceleration (km/decade²)", y = "Latitude", x = NULL) +
    theme_bw()
  

  