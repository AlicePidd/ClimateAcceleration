# Plotting climate acceleration by 1° latitude
# Written by Alice P
# 6 March 2026



# Questions:
# Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
# Acceleration within MPAs vs. outside MPAs - yearly median timeseries
# Acceleration per SSP, 

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

  
  
# Get it in a tibble format for plotting ---------------------------------------
  
  out_summary <- out %>% 
    group_by(lat, ssp, term) %>% 
    summarise(median_slope_yr = median(median_slope, na.rm = TRUE),
              median_slope_dec = median(median_slope, na.rm = TRUE) * 10,
              .groups = "drop") %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  recent_rows <- out_summary %>% 
    filter(ssp == "historical") %>% 
    dplyr::select(-ssp) # Otherwise it plots it as historical, not with the ssps

  out_summary <- out_summary %>% 
    mutate(term = fct_relevel(term, "recent", "near", "mid", "intermediate", "long"))
  
  ssp_summary <- out_summary %>% 
    filter(ssp != "historical")

  # Checking the spread for the lims
  range(ssp_summary$median_slope_yr) # -0.6827282  0.6266832
  range(ssp_summary$median_slope_dec) # -6.827282  6.266832
  
  
  

# Plot -------------------------------------------------------------------------
  
  heat_pal <- c("#244e57", "#378890", "#70c0ba", "#CDF1EF", "#FFFDF4", "#f4c659", "#d9792e", "#af2213", "#871c0f")
  RdBu_pal <- paletteer::paletteer_c("scico::vik", 11)
  # RdBu_pal <- khroma::color("BuRd")(11)
  
  plot_lat_accel <- function(m, pal, lim, lab, nm) {
    lat_plot <- ggplot() +
      geom_tile(data = recent_rows, aes(x = term, y = lat, fill = .data[[m]])) + 
      geom_tile(data = ssp_summary, aes(x = term, y = lat, fill = .data[[m]])) + 
      scale_fill_gradientn(colours = pal, 
                           limits = lim) +
      facet_wrap(~ssp, nrow = 1) +
      scale_y_continuous(breaks = seq(-50, 0, by = 10)) +
      labs(fill = paste0("Acceleration\n(km/", lab, "²)"), y = "Latitude", x = NULL) + 
      theme_few(base_size = 10)
    
    ggsave(paste0(plot_fol, "median_acceleration_by_1deglatitude_km", lab, "2_", nm,".pdf"),
           lat_plot, width = 14, height = 8, dpi = 300)
  }
  
  params <- list(list(m = "median_slope_dec", lim = c(-10, 10), lab = "decade"),
                 list(m = "median_slope_yr",  lim = c(-1, 1),   lab = "year"))
  
  pals <- list(heatpal = heat_pal, 
               RdBupal = RdBu_pal)
  
  walk(names(pals), function(nm) {
    walk(params, function(p) {
      plot_lat_accel(m = p$m, 
                     pal = pals[[nm]], 
                     lim = p$lim, 
                     lab = p$lab, 
                     nm = nm)
      })
    })
  
  