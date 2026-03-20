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

  esm_fol <- make_folder(source_disk, "5_accleration_aus_df_ESMs_terms", "")
  median_fol <- make_folder(source_disk, "5_accleration_aus_df_median_terms", "")
  

  # pal_accel_div <- RColorBrewer::brewer.pal(11, "BrBG")
  pal_accel_div <- c("#001219", "#005F73", "#0a9396", "#94d2bd", "#E6DDC5", "#ee9b00", "#ca6702", "#ae2012", "#9b2226") # Manual version
  # pal_accel_div <- c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7FBFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
  pal_vel_seq <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1","#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
  
  
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
  
# Get 95th percentile of the data and truncate by that value -------------------
  
  a_files <- dir(accel_fol, full.names = TRUE)
  a_files
  
  m_files <- dir(median_fol, full.names = TRUE)
  m_files
  

  all_r <- map(files, ~{
    readRDS(.x)[[1]]
  }) %>% 
    rast()
  

  r <- readRDS("/Volumes/AliceShield/acceleration_data/5_accleration_aus_df_median_terms/acceleration_median_df_ssp245_mid-term.RDS")
  
  # Symmetric limits for diverging scale
  abs_max_a <- max(r$median_accel, na.rm = TRUE) %>% 
    ceiling()
  abs_min_a <- min(r$median_accel, na.rm = TRUE) %>% 
    floor()
  
  
  ggplot() +
    geom_raster(data = r, 
                aes(x = lon, y = lat, fill = median_accel)) +
    scale_fill_gradientn(colours = pal_accel_div,
                         limits = c(-5, abs_max_a),
                         na.value = "transparent",
                         name = expression(paste("Acceleration (km dec"^{-2}, ")")),
                         oob = squish) +
    geom_sf(data = aus_detailed_shp, fill = "grey40", colour = NA) +
    # geom_sf(data = eez_shp, fill = "grey90", colour = "black", lwd = 0.2) +
    # facet_wrap(~esm) +
    coord_sf(expand = FALSE) +
    theme_minimal()
  
  
  
  
  

# Plot spatially per SSP-term combination --------------------------------------

  load_median <- function(path) {
    r <- readRDS(path)
    # layer 1 is always the median; layers 2-3 are min/max
    r[[1]]
  }
  
  r_a_med <- load_median(
    m_files %>% 
      str_subset(., "ssp245") %>% 
      str_subset(., "mid-term") 
  )
  r_a_med_eez <- mask(r_a_med, reez)
  
  # Symmetric limits for diverging scale
  abs_max_a <- max(abs(minmax(r_a_med_eez)), na.rm = TRUE) %>% 
    ceiling()
   
  panel_A <- ggplot() +
    tidyterra::geom_spatraster(data = r_a_med_eez) +
    scale_fill_gradientn(
      colours   = pal_accel_div,
      # limits    = c(-abs_max_a, abs_max_a),
      limits    = c(-5, 5),
      na.value  = "transparent",
      name      = expression(paste("Acceleration (km dec"^{-2}, ")")),
      oob = squish,
      guide = guide_colorbar(
        barwidth  = 12,
        barheight = 0.5,
        title.position = "top",
        title.hjust    = 0.5,
        direction = "horizontal"
      )
    ) +
    geom_sf(data = eez_shp, fill = NA, colour = "black",
            linewidth = 0.3) +
    geom_sf(data = aus_detailed_shp, fill = "grey10", colour = NA) +
    coord_sf(expand = FALSE) +
    labs(title = NULL, subtitle = "SSP2-4.5 | mid-term (2041–2060)") +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 7),
      axis.title       = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      plot.subtitle    = element_text(size = 9, colour = "grey30")
    )
   
  panel_A
  
  
  
