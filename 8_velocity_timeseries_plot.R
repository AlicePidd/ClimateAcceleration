# Plotting median climate velocity timeseries per SSP-term combo
  # Written by Alice P
    # 2 April 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") # Use the rasts so can crop them
  df_fol <- make_folder(source_disk, "3_velocity_decadal_median_timeseries", "dfs") # Use the rasts so can crop them
  plot_fol <- make_folder(source_disk, "6_velocity_aus_plot", "timeseries")
  fig_fol <- make_folder(source_disk, "_figures", "")
  

  all_files <- dir(vocc_fol, full.names = TRUE)
  # esm_files <- dir(vocc_fol, full.names = TRUE) %>% 
  #   str_subset(., pattern = "historical", negate = TRUE)
  # esm_files
  # hist_files <- dir(vocc_fol, full.names = TRUE, pattern = "historical")
  # hist_files

  
  
  
# Get the extent into thirds to split into 3 regions ---------------------------
  
  f <- all_files[2]
  d <- readRDS(f) # Can use any file
  ext(d) # SpatExtent : 105, 175, -50, -5 (xmin, xmax, ymin, ymax)
  
  q <- quantile(c(ext(d)[3], ext(d)[4]), c(1/3, 2/3))
  q 
    # 33.33333% 66.66667% 
    #       -35       -20 
  ## Roughly splitting data into regions based on these lats
    # North: -5 to -20
    # Mid: -20 to -35
    # South: -35 to -50
  
  
  
  
# Get rasters cropped and in df format -----------------------------------------
  
  do_mask_df <- function(f){
    
    ssp <- basename(f) %>%
      str_split_i(., "_", 3)
    esm <- basename(f) %>%
      str_split_i(., "_", 4)
    term <- basename(f) %>%
      str_split_i(., "_", 5)
    
    d <- readRDS(f) # Read it in
    m <- mask(d, eez_shp) # Mask it
    crs(m) <- "EPSG:4326"

    cat_df <- m %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      pivot_longer(cols = -c(x, y), # Multilayer files so need to pivot 
                   names_to = "year",
                   values_to = "velocity") %>%
      mutate(year = str_split_i(year, "_", 1) %>% as.numeric(),
             ssp  = ssp,
             term = term,
             esm  = esm,
             cat  = case_when(y <= q[[1]] ~ "south",
                              y > q[[1]] & y <= q[[2]] ~ "mid",
                              y > q[[2]] ~ "north"))
    
    return(cat_df)
  }
  
  df_comb <- map(all_files, do_mask_df) %>%
    bind_rows()
  df_comb
  saveRDS(df_comb, file = paste0(df_fol, "/velocity_decadal_timeseries-median_df_combined_ssp-esm-term.RDS"))
  
  
  

# Create plotting df -----------------------------------------------------------
  
  df_comb <- readRDS(paste0(df_fol, "/velocity_decadal_timeseries-median_df_combined_ssp-esm-term.RDS"))
  
  df_comb %>% 
    filter(term != "recent")
  
  plot_df <- df_comb %>%
    mutate(cat = factor(cat, levels = c("north", "mid", "south"))) %>%
    group_by(year, cat, ssp, term) %>%
    summarise(
      med_velocity = median(velocity, na.rm = TRUE),
      lo_velocity  = quantile(velocity, 0.1, na.rm = TRUE),
      hi_velocity  = quantile(velocity, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  ssp_cols <- c(#"historical" = "#5F5E5A",
    "ssp126" = col_ssp126,
    "ssp245" = col_ssp245,
    "ssp370" = col_ssp370,
    "ssp585" = col_ssp585)
  
  

  ## Maybe should weight it by number of grid cells (rows?) in each cat, if they are very different?
  df_comb %>%
    distinct(x, y, cat) %>%
    count(cat)
  # # A tibble: 3 × 2
  #     cat       n
  #     <chr> <int>
  #   1 mid    4640
  #   2 north  3604
  #   3 south  3201
  
  
  
  
# Plot timeseries --------------------------------------------------------------
  
  p <- ggplot() +
    geom_ribbon(data = plot_df %>% filter(term == "recent"),
                aes(x = year, ymin = lo_velocity, ymax = hi_velocity),
                alpha = 0.1, colour = NA) +
    geom_line(data = plot_df %>% filter(term == "recent"),
              aes(x = year, y = med_velocity),
              colour = "grey40", lwd = 0.8) +
    geom_ribbon(data = plot_df %>% filter(term != "recent"),
                aes(x = year, ymin = lo_velocity, ymax = hi_velocity, fill = ssp),
                alpha = 0.1, colour = NA) +
    geom_line(data = plot_df %>% filter(term != "recent"),
              aes(x = year, 
                  y = med_velocity, 
                  colour = ssp),
              lwd = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", lwd = 0.5, col = "grey50", alpha = 0.6) +
    scale_colour_manual(values = ssp_cols, name = NULL) +
    scale_fill_manual(values = ssp_cols, name = NULL) +
    facet_wrap(~cat, ncol = 1) +
    labs(y = "Median climate velocity (km/decade)", x = "Year") +
    theme_classic(base_size = 10)
  p
  
  ggsave(p, file = paste0(plot_fol, "/velocity_decadal_timeseries-median_north-mid-south_ssp.pdf"),
         width = 6, height = 10, dpi = 300)
  ggsave(p, file = paste0(plot_fol, "/velocity_decadal_timeseries-median_north-mid-south_ssp.png"),
         width = 6, height = 10, dpi = 300)
  
  
  
  
  
# Plot an Oceania with graticules ----------------------------------------------

  ## Make hlines at the latitude splits ------------
    lonmin <- ext(oceania_stanford_shp)[1]
    lonmax <- ext(oceania_stanford_shp)[2]
    target_lat1 <- -20
    target_lat2 <- -35

    line1 <- st_linestring(matrix(c(lonmin, target_lat1, lonmax, target_lat1), 
                                  ncol = 2, byrow = TRUE))
    line1_sf <- st_sfc(line1, crs = 4326)
    
    line2 <- st_linestring(matrix(c(lonmin, target_lat2, lonmax, target_lat2),
                                  ncol = 2, byrow = TRUE))
    line2_sf <- st_sfc(line2, crs = 4326)
  
    
    
  ## Plot it  ------------
    a <- ggplot() +
      geom_sf(data = eez_shp,
              lwd = NA,
              fill = "grey85",
              alpha = 0.5) +
      geom_sf(data = oceania_stanford_shp,
              lwd = NA,
              fill = "grey80") +
      geom_sf(data = line1_sf, color = "grey30", linetype = "dashed", size = 0.5) +
      geom_sf(data = line2_sf, color = "grey30", linetype = "dashed", size = 0.5) +
      theme_void()
    
    ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.pdf"),
           width = 8, height = 10, dpi = 300)
    ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.png"),
           width = 8, height = 10, dpi = 300)
  
  
  
  # ## Cut EEZ to be at each region for the figure (didn't end up using this, bit of tomfoolery I couldn't be bothered to fix) -----------
  #   bb <- st_bbox(eez_shp)
  #   
  #   # helper to build a band
  #   make_band <- function(ymin, ymax) {
  #     st_as_sfc(st_bbox(c(
  #       xmin = as.numeric(bb["xmin"]),
  #       xmax = as.numeric(bb["xmax"]),
  #       ymin = as.numeric(ymin),
  #       ymax = as.numeric(ymax)
  #     ), crs = st_crs(eez_shp)))
  #   }
  #   
  #   band_north <- make_band(-20, bb["ymax"])
  #   band_mid   <- make_band(-35, -20)
  #   band_south <- make_band(bb["ymin"], -35)
  #   
  #   # intersection instead of crop
  #   eez_north <- st_crop(eez_shp, band_north)
  #   eez_mid   <- st_crop(eez_shp, band_mid)
  #   eez_south <- st_crop(eez_shp, band_south)
  # 
  # 
  # a <- ggplot() +
  #   geom_sf(data = eez_north,
  #           lwd = NA,
  #           fill = "yellow",
  #           alpha = 0.5) +
  #   geom_sf(data = eez_mid,
  #           lwd = NA,
  #           fill = "dodgerblue2",
  #           alpha = 0.5) +
  #   geom_sf(data = eez_south,
  #           lwd = NA,
  #           fill = "green",
  #           alpha = 0.5) +
  #   geom_sf(data = oceania_stanford_shp,
  #           lwd = NA,
  #           fill = "grey80") +
  #   geom_sf(data = line1_sf, color = "grey30", linetype = "dashed", size = 0.5) +
  #   geom_sf(data = line2_sf, color = "grey30", linetype = "dashed", size = 0.5) +
  #   theme_void() # No graticules
  #   # theme_minimal() # To include graticules
  # 
  # ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.pdf"),
  #        width = 8, height = 10, dpi = 300)
  # ggsave(a, file = paste0(fig_fol, "/oceania_graticules_regions.png"),
  #        width = 8, height = 10, dpi = 300)
  
  
  
  