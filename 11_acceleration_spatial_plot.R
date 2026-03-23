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
  accel_fol <- make_folder(source_disk, "5_accleration_aus_df_median_terms", "")
  median_dfs_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "dfs")
  median_rast_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "rasts")
  
  
  
  
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

  
  
  
# Files ------------------------------------------------------------------------
  
  ## Load and bind all the median df files ------------
  
    df_files <- dir(median_dfs_fol, full.names = TRUE, pattern = ".RDS")
    df_files
    
    all_df <- map(df_files, ~{
      readRDS(.x)
    }) %>% 
      bind_rows()
    all_df
  
  
  ## Load and stack all the median rast files ------------
  
    rast_files <- dir(median_rast_fol, full.names = TRUE, pattern = ".RDS")
    rast_files
    
    all_r <- map(rast_files, ~{
      readRDS(.x)[[1]]
    }) %>%
      rast()
    all_r
  


  
# Palettes ---------------------------------------------------------------------

  pal_accel_div <- RColorBrewer::brewer.pal(11, "RdBu")
  pal_accel_div <- c("#001219", "#005F73", "#0a9396", "#94d2bd", "#E6DDC5", "#ee9b00", "#ca6702", "#ae2012", "#9b2226") # Manual version
  pal_accel_div <- c("#08519C", "#4393C3", "#92C5DE", "#D1E5F0", "#F7FBFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
  pal_vel_seq <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1","#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
  
  
  
  
# Calculate global quantiles by which to scale the plots -----------------------
  
  lims <- quantile(all_df$median_accel, c(0.05, 0.95), na.rm = TRUE)
  lims
    #          5%       95% 
    #   -5.488570  3.071301 
    ## Roughly -5 to 5

  Qs <- quantile(all_df$median_accel, c(0.25, 0.75), na.rm = TRUE)
  Qs
    #          25%        75% 
    #   -1.4510885  0.7507432 

  
  

# Plot spatially per SSP-term combination --------------------------------------

  load_median <- function(path) {
    r <- readRDS(path)
    }
  
  
  names(all_r)
  
  
  rast_med <- all_df %>% 
    # subset(., "ssp245_mid-term")
    filter(ssp == "ssp245" & term == "mid-term")  

   
  
  # Plot
  panel_A <- ggplot() +
    # tidyterra::geom_spatraster(data = rast_med) +
    geom_raster(data = rast_med,
                aes(x = lon, y = lat, fill = median_accel)) +
    scale_fill_gradientn(
      colours   = pal_accel_div,
      # limits    = c(-abs_max_a, abs_max_a),
      limits    = c(-25, 25),
      na.value  = "transparent",
      name      = expression(paste("Median climate acceleration (km dec"^{-2}, ")")),
      oob = squish, # Truncates the values outside of the limits
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
    # geom_sf(data = aus_detailed_shp, fill = "grey30", colour = NA) +
    coord_sf(expand = FALSE) +
    labs(title = NULL, subtitle = "SSP2-4.5 | mid-term (2041–2060) | Median acceleration across 11 ESMs") +
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
  
  
  
