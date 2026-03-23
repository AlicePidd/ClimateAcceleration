# Plotting climate acceleration spatially
  # Written by Alice P
    # 6 March 2026



# Questions:
  # Acceleration at each 1° of latitude - median of each band, per SSP (no time aspect)
  # Acceleration within MPAs vs. outside MPAs - yearly median timeseries
  # Acceleration per SSP, 



# Ideas: 
  # Plotting acceleration spatially as normal, but only show 245 mid term, with rest in sups
  # Acceleration divergence from previous term? Like an anomaly
  # Latitudinal acceleration per SSP - median of all ESMs as the main line, ribbons as min-max across ESMs, lines per SSP, plot per term?
  # 



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  # esm_fol <- make_folder(source_disk, "5_accleration_aus_df_ESMs_terms", "")
  # accel_fol <- make_folder(source_disk, "5_accleration_aus_df_median_terms", "")
  median_dfs_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "dfs")
  median_rast_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "7_acceleration_aus_plot", "spatial") # Aus files
  
  
  
  
# Lists ------------------------------------------------------------------------
    
  ssp_cols <- c("historical" = "#5F5E5A",
                "ssp126" = col_ssp126,
                "ssp245" = col_ssp245,
                "ssp370" = col_ssp370,
                "ssp585" = col_ssp585)
  
  ssp_lty <- c("historical" = "dashed",
               "ssp126" = "solid", 
               "ssp245" = "solid",
               "ssp370" = "solid",
               "ssp585" = "solid")
  
  ssp_labels <- c("historical" = "Historical",
                  "ssp126" = "SSP1-2.6",
                  "ssp245" = "SSP2-4.5",
                  "ssp370" = "SSP3-7.0",
                  "ssp585" = "SSP5-8.5")
  
  term_labels <- c("near" = "Near-term (2021–2040)",
                   "mid" = "Mid-term (2041–2060)",
                   "intermediate" = "Intermediate-term (2061–2080)",
                   "long" = "Long-term (2081–2090)")

  term_order <- term_list[2:5]
  ssp_order <- ssp_list
  
  
  
  
# Files ------------------------------------------------------------------------
  
  ## Load and bind all the median df files ------------
  
    df_files <- dir(median_dfs_fol, full.names = TRUE, pattern = ".RDS")
    df_files # Use for computing lims etc.
    
    all_df <- map(df_files, ~{
      readRDS(.x)
    }) %>% 
      bind_rows()
    all_df
  
  
  ## Load and stack all the median rast files ------------
  
    rast_files <- dir(median_rast_fol, full.names = TRUE, pattern = ".RDS")
    rast_files # Use for plotting
    
    all_r <- map(rast_files, ~{
      readRDS(.x)[[1]]
    }) %>%
      rast()
    all_r
  


  
# Palettes ---------------------------------------------------------------------

  pal_div_RdBu <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
  pal_div_heat <- c("#001219", "#005F73", "#0a9396", "#94d2bd", "#F7FBFF", "#ee9b00", "#ca6702", "#ae2012", "#9b2226") # Manual version
  pal_div_RdBualt <- c("#08519C", "#4393C3", "#92C5DE", "#D1E5F0", "#F7FBFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
  pal_div_BrBG <- rev(RColorBrewer::brewer.pal(11, "BrBG"))
  # pal_div_BlOr <- c("#005F73", "#208f99", "#52c4cc", "#99faff", "#ccfeff", "#e6ffff", "#ffe6cc", "#ffad66", "#ff8f32", "#cc5801", "#994000") # Manual version
  pal_div_coco <- rev(c("#881d00", "#af6125", "#f4e3c7", "#F7FBFF", "#1bb5af", "#0076bb", "#172869")) # Manual version
  pal_div_geyser <- c("#018080", "#70a494", "#b4c8a8", "#F7FBFF", "#edbb8a", "#de8a5a", "#ca562d") # Manual version
  
  
  
  
  
# Calculate global quantiles by which to scale the plots -----------------------
  
  range_df <- range(all_df$median_accel, na.rm = TRUE) # -203.9491  111.5277
  
  
  q5q95 <- quantile(all_df$median_accel, c(0.05, 0.95), na.rm = TRUE)
  q5q95
    #          5%       95% 
    #   -5.488570  3.071301 
    ## Roughly -5 to 5
  abs_lim <- max(abs(q5q95))  # 5.48857
  abs_lims <- round(abs_lim, digits = 1) # 5.5

    # Qs <- quantile(all_df$median_accel, c(0.25, 0.75), na.rm = TRUE)
    # Qs
    #   #          25%        75% 
    #   #   -1.4510885  0.7507432 

  ## Make it absolute - this makes sense in my mind?
    abs_lim <- max(abs(q5q95))  # 5.48857
    abs_lims <- round(abs_lim, digits = 1) # will go with ± 5.5 around 0

  
  

# Plot spatially per SSP-term combination --------------------------------------
    
  ## Plot -------------
    
    plot_accel_ssp <- function(ssp, df_files, lims, pal, pal_name) {
      
      # Subset and reorder files for this SSP
      files_ssp <- df_files %>% 
        str_subset(ssp) %>% 
        .[map_int(., ~ detect_index(term_order, \(t) grepl(t, .x)))]  # reorder by term_order
      
      # Load and combine
      r <- map(files_ssp, readRDS) %>% 
        bind_rows()
      
      # Make term a factor so facets respect term_order
      r <- r %>% 
        mutate(term = factor(term, levels = paste0(term_order, "-term")))
      
      p <- ggplot() +
        geom_raster(data = r, aes(x = lon, y = lat, fill = median_accel)) +
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
        # geom_sf(data = aus_detailed_shp, fill = "grey70", colour = NA) +
        geom_sf(data = oceania_stanford_shp, fill = "grey70", colour = NA) +
        coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
        facet_wrap(~ term, ncol = 1) +
        labs(title = paste0("Median decadal acceleration across all 11 ESMs -- ", ssp)) +
        theme_classic(base_size = 10) +
        theme(legend.position = "bottom",
              legend.title    = element_text(size = 8),
              legend.text     = element_text(size = 7),
              plot.subtitle   = element_text(size = 9, colour = "grey30"))
      
      o_nm <- paste0(plot_fol, "/spatial_median_acceleration_decadal_", ssp, "_pal-", pal_name, "_lims", lims, ".png")
      ggsave(filename = o_nm, plot = p, width = 8, height = 20, dpi = 600)
    }
    
    
    ## Projected
      # Heat div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_heat, pal_name = "div_heat"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_heat, pal_name = "div_heat"))
      # RdBu div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_RdBu, pal_name = "div_RdBu"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_RdBu, pal_name = "div_RdBu"))
      
      # Coco div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_coco, pal_name = "div_coco"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_coco, pal_name = "div_coco"))
      
      
    ## Historic period
      # Heat div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_heat, pal_name = "div_heat"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_heat, pal_name = "div_heat"))
      # RdBu div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_RdBu, pal_name = "div_RdBu"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_RdBu, pal_name = "div_RdBu"))
      
      # Coco div palette
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = abs_lims, 
                                      pal = pal_div_coco, pal_name = "div_coco"))
      walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
                                      lims = 25, 
                                      pal = pal_div_coco, pal_name = "div_coco"))
    
    
    # # Earth div palette
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = abs_lims, 
    #                                 pal = pal_div_geyser, pal_name = "div_geyser"))
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = 25, 
    #                                 pal = pal_div_geyser, pal_name = "div_geyser"))
    
    # # Alternate RdBu div palette
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = abs_lims, 
    #                                 pal = pal_div_RdBualt, pal_name = "div_RdBu-Alt"))
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = 25, 
    #                                 pal = pal_div_RdBualt, pal_name = "div_RdBu-Alt"))
    
    # # BrBG div palette
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = abs_lims, 
    #                                 pal = pal_div_BrBG, pal_name = "div_BrBG"))
    # walk(ssp_list, ~ plot_accel_ssp(.x, df_files, 
    #                                 lims = 25, 
    #                                 pal = pal_div_BrBG, pal_name = "div_BrBG")) # Lowkey looks like a shit smear.
    
    
    
    
    
