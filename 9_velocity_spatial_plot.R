# Plotting climate velocity -- median per grid cell
  # Written by Alice P
    # 7 March 2026

# Climate velocity in each grid cell - median across ESMs, per SSP and term

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  # vocc_fol <- make_folder(source_disk, "2_velocity_rolling_annual", "")
  vocc_df_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "dfs")
  plot_fol <- make_folder(source_disk, "6_velocity_aus_plot", "spatial")

  
  files <- dir(vocc_df_fol, full.names = TRUE)
  files
  f <- files[2]
  f

  
  
  
# Lists ------------------------------------------------------------------------
    
  # ssp_cols <- c("historical" = "#5F5E5A",
  #               "ssp126" = col_ssp126,
  #               "ssp245" = col_ssp245,
  #               "ssp370" = col_ssp370,
  #               "ssp585" = col_ssp585)
  term_order <- term_list
  ssp_order <- ssp_list
  
  
  
  
# Files ------------------------------------------------------------------------
  
  ## Load and bind all the median df files ------------
  
  future_files <- dir(vocc_df_fol, full.names = TRUE, pattern = ".RDS") %>% 
    str_subset(., "historical", negate = TRUE) # Gotta keep these out so that it doesn't skew the quantiles
  future_files # Use for computing lims etc.
    
  
  # ## Load and stack all the median rast files ------------
  # 
  #   rast_files <- dir(median_rast_fol, full.names = TRUE, pattern = ".RDS")
  #   rast_files # Use for plotting
  #   
  #   all_r <- map(rast_files, ~{
  #     readRDS(.x)[[1]]
  #   }) %>%
  #     rast()
  #   all_r

    

    
    
# Palettes ---------------------------------------------------------------------
  
  pal_div_RdBu <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
  pal_div_heat <- c("#001219", "#005F73", "#0a9396", "#94d2bd", "#F7FBFF", "#ee9b00", "#ca6702", "#ae2012", "#9b2226") # Manual version
  # pal_div_RdBualt <- c("#08519C", "#4393C3", "#92C5DE", "#D1E5F0", "#F7FBFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
  # pal_div_BrBG <- rev(RColorBrewer::brewer.pal(11, "BrBG"))
  # pal_div_BlOr <- c("#005F73", "#208f99", "#52c4cc", "#99faff", "#ccfeff", "#e6ffff", "#ffe6cc", "#ffad66", "#ff8f32", "#cc5801", "#994000") # Manual version
  pal_div_coco <- rev(c("#881d00", "#af6125", "#f4e3c7", "#F7FBFF", "#1bb5af", "#0076bb", "#172869")) # Manual version
  # pal_div_geyser <- c("#018080", "#70a494", "#b4c8a8", "#F7FBFF", "#edbb8a", "#de8a5a", "#ca562d") # Manual version
    

  
  
  
# Calculate global quantiles by which to scale the plots -----------------------
    
  ## Projected data
    future_df <- map(future_files, ~{
      readRDS(.x)
      }) %>% 
      bind_rows()
    future_df
    
    future_range <- range(future_df$median_velocity, na.rm = TRUE) # no historical: -408.2199 4669.6462
    future_range
    
    ### IQR lims -------
      q25q75 <- quantile(future_df$median_velocity, c(0.25, 0.75), na.rm = TRUE)
      q25q75
        # No Historical
          #          25%       75% 
          #     22.02140 94.98958 

      future_lims <- round(max(abs(q25q75)), digits = 1)  # ~94.98958 so will go with ± 100 around 0
      future_lims
      
  
    ### 5th-9th percentile lims -------
      q5q95 <- quantile(future_df$median_velocity, c(0.05, 0.95), na.rm = TRUE)
      q5q95
        # No Historical
          #          5%       95% 
          #     1.79268 361.06305  

      future_lims <- round(max(abs(q5q95)))  # 361.0631, so will go with ± 360 around 0
      future_lims
      
  
  

# Plot spatially per SSP-term combination --------------------------------------

  plot_velocity_future <- function(ssp, files, lims, lim_method, pal, pal_name) {
    
    files_ssp <- files %>% 
      str_subset(ssp) %>% 
      .[order(map_int(., ~ detect_index(term_order, \(t) grepl(t, .x))))]
    
    r <- map(files_ssp, readRDS) %>% 
      bind_rows() %>% 
      mutate(term = factor(term, levels = paste0(term_order, "-term")))
    
    p <- ggplot() +
      geom_raster(data = r, aes(x = lon, y = lat, fill = median_velocity)) +
      scale_fill_gradientn(colours = pal,
                           limits = c(-lims, lims),
                           na.value = "transparent",
                           name = expression(paste("Median climate velocity (km decade"^{-1}, ")")),
                           oob = squish,
                           guide = guide_colorbar(barwidth  = 12,
                                                  barheight = 0.5,
                                                  title.position = "top",
                                                  title.hjust    = 0.5,
                                                  direction = "horizontal")) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.3) +
      geom_sf(data = oceania_stanford_shp, fill = "grey70", colour = NA) +
      coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
      facet_wrap(~ term, ncol = 1) +
      labs(title = paste0("Median decadal velocity -- ", ssp)) +
      theme_classic(base_size = 10) +
      theme(legend.position = "bottom",
            legend.title    = element_text(size = 8),
            legend.text     = element_text(size = 7),
            plot.subtitle   = element_text(size = 9, colour = "grey30"))
    
    o_nm <- paste0(plot_fol, "/spatial_median_velocity_decadal_", ssp, 
                   "_pal-", pal_name, "_", lim_method, "_lims", lims, ".png")
    ggsave(filename = o_nm, plot = p, width = 8, height = 20, dpi = 600)
  }
      
  # Heat div palette
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 360, lim_method = "Q5-Q95",
                                    pal = pal_div_heat, pal_name = "div_heat"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                       lims = 100, lim_method = "IQR",
                                       pal = pal_div_heat, pal_name = "div_heat"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 200, lim_method = "arbitrary",
                                    pal = pal_div_heat, pal_name = "div_heat"))
    
  # RdBu div palette
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 360, lim_method = "Q5-Q95",
                                    pal = pal_div_RdBu, pal_name = "div_RdBu"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                       lims = 100, lim_method = "IQR",
                                       pal = pal_div_RdBu, pal_name = "div_RdBu"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 200, lim_method = "arbitrary",
                                    pal = pal_div_RdBu, pal_name = "div_RdBu"))
  
  # Coco div palette
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 360, lim_method = "Q5-Q95",
                                    pal = pal_div_coco, pal_name = "div_coco"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                       lims = 100, lim_method = "IQR",
                                       pal = pal_div_RdBu, pal_name = "div_RdBu"))
    walk(ssp_list, ~ plot_velocity_future(.x, future_files, 
                                    lims = 200, lim_method = "arbitrary",
                                    pal = pal_div_coco, pal_name = "div_coco"))
      
      
      
      
# Calculate global quantiles by which to scale the plots -----------------------
      
  ## Historical data
    hist_files <- dir(vocc_df_fol, full.names = TRUE, pattern = "historical")
      
    hist_file <- readRDS(hist_files)
    hist_file # Use for computing lims etc.
  
    ### IQR lims -------
      q25q75_hist <- quantile(hist_file$median_velocity, c(0.25, 0.75), na.rm = TRUE)
      q25q75_hist
      #         25%       75% 
      #   -1.701465  1.714993 
      
      hist_lims <- round(max(abs(q25q75_hist)), digits = 1)  # ~1.71 so will go with ± 2 around 0
      hist_lims
    
    
    ### 5th-9th percentile lims -------
      q5q95_hist <- quantile(hist_file$median_velocity, c(0.05, 0.95), na.rm = TRUE)
      q5q95_hist
      #          5%       95% 
      #   -4.659361  9.744097 
      
      hist_lims <- round(max(abs(q5q95_hist)), digits = 1)  # 9.74, so will go with ± 10 around 0
      hist_lims
      
      
      
    
# Plot spatially for historical only -------------------------------------------
  
  
  plot_accel_hist <- function(file, lims, lim_method, pal, pal_name) {
    
    r <- readRDS(file) %>%
      mutate(term = factor(term, levels = paste0(term_order, "-term")))
    
    p <- ggplot() +
      geom_raster(data = r, aes(x = lon, y = lat, fill = median_velocity)) +
      scale_fill_gradientn(colours = pal,
                           limits = c(-lims, lims),
                           na.value = "transparent",
                           name = expression(paste("Median climate acceleration (km decade"^{-2}, ")")),
                           oob = squish,
                           guide = guide_colorbar(barwidth  = 12,
                                                  barheight = 0.5,
                                                  title.position = "top",
                                                  title.hjust    = 0.5,
                                                  direction = "horizontal")) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.3) +
      geom_sf(data = oceania_stanford_shp, fill = "grey70", colour = NA) +
      coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
      labs(title = "Median decadal acceleration -- historical (OISST)") +
      theme_classic(base_size = 10) +
      theme(legend.position = "bottom",
            legend.title    = element_text(size = 8),
            legend.text     = element_text(size = 7),
            plot.subtitle   = element_text(size = 9, colour = "grey30"))
    
    o_nm <- paste0(plot_fol, "/spatial_median_velocity_decadal_historical",
                   "_pal-", pal_name, "_", lim_method, "_lims", lims, ".png")
    ggsave(filename = o_nm, plot = p, width = 8, height = 6, dpi = 600)
  }
  
  # Do it
  plot_accel_hist(hist_files, 
                  lims = 10, lim_method = "Q5-Q95", 
                  pal = pal_div_heat, pal_name = "div_heat")
  plot_accel_hist(hist_files, 
                  lims = 1.7, lim_method = "IQR", 
                  pal = pal_div_heat, pal_name = "div_heat")
  plot_accel_hist(hist_files, 
                  lims = 25, lim_method = "arbitrary", 
                  pal = pal_div_heat, pal_name = "div_heat")
  
  plot_accel_hist(hist_files, 
                  lims = 10, lim_method = "Q5-Q95", 
                  pal = pal_div_RdBu, pal_name = "div_RdBu")
  plot_accel_hist(hist_files, 
                  lims = 1.7, lim_method = "IQR", 
                  pal = pal_div_RdBu, pal_name = "div_RdBu")
  plot_accel_hist(hist_files, 
                  lims = 25, lim_method = "arbitrary", 
                  pal = pal_div_RdBu, pal_name = "div_RdBu")

  plot_accel_hist(hist_files, 
                  lims = 10, lim_method = "Q5-Q95", 
                  pal = pal_div_coco, pal_name = "div_coco")
  plot_accel_hist(hist_files, 
                  lims = 1.7, lim_method = "IQR", 
                  pal = pal_div_coco, pal_name = "div_coco")
  plot_accel_hist(hist_files, 
                  lims = 25, lim_method = "arbitrary", 
                  pal = pal_div_coco, pal_name = "div_coco")


  