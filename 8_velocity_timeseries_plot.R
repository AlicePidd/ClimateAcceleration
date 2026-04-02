# Plotting median climate velocity per SSP-term combo
  # Written by Alice P
    # 19 March 2026

  

# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")


  
# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "2_velocity_rolling_terms", "decadal") # Use the rasts so can crop them
  plot_fol <- make_folder(source_disk, "6_velocity_aus_plot", "timeseries")

  
  esm_files <- dir(vocc_fol, full.names = TRUE) %>% 
    str_subset(., pattern = "historical", negate = TRUE)
  esm_files
  hist_files <- dir(vocc_fol, full.names = TRUE, pattern = "historical")
  hist_files

  
  
  
# Get the extent into thirds to split into 3 regions ---------------------------
  
  f <- esm_files[7]
  d <- readRDS(f) # Can use any file
  ext(d) # SpatExtent : 105, 175, -50, -5 (xmin, xmax, ymin, ymax)
  
  q <- quantile(c(ext(d)[3], ext(d)[4]), c(1/3, 2/3))
  q 
    # 33.33333% 66.66667% 
    #       -35       -20 

  

  
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
  
  df_comb <- map(esm_files, do_mask_df) %>%
    bind_rows()
  df_comb
  
  

  
  plot_df <- df_comb %>%
    group_by(year, cat, ssp, term) %>%
    summarise(
      med_velocity = median(velocity, na.rm = TRUE),
      lo_velocity  = quantile(velocity, 0.1, na.rm = TRUE),
      hi_velocity  = quantile(velocity, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(plot_df, 
         aes(x = year, 
             y = med_velocity, 
             colour = ssp, fill = ssp)) +
    geom_ribbon(aes(ymin = lo_velocity, 
                    ymax = hi_velocity), 
                alpha = 0.2, colour = NA) +
    geom_line() +
    facet_grid(~cat) +
    labs(y = "Median decadal velocity (km/yr)", x = "Year") +
    theme_bw()
  
  
  
  
  
  
# Get median velocity and min/max medians per cat ------------------------------
  
  df_cat <- df_comb %>% 
    group_by(ssp, term, cat) %>% 
    summarise(med_cat = median(med_velocity),
              min_cat = min(med_velocity),
              max_cat = max(med_velocity))
  
  
  
  
  
  
# Plot -------------------------------------------------------------------------
  
  ssp_cols <- c(#"historical" = "#5F5E5A",
    "ssp126" = col_ssp126,
    "ssp245" = col_ssp245,
    "ssp370" = col_ssp370,
    "ssp585" = col_ssp585)
  
  
  
  

  esm_ribbon <- esm_meds %>%
    filter(ssp != "historical") %>%
    group_by(lat, ssp, term) %>%
    summarise(
      lo_5 = quantile(esm_med, 0.05, na.rm = TRUE),
      hi_95 = quantile(esm_med, 0.95, na.rm = TRUE),
      lo_25 = quantile(esm_med, 0.25, na.rm = TRUE),
      hi_75 = quantile(esm_med, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  esm_ribbon
  
  max(esm_ribbon$hi_95)
  
  
  p <- ggplot() +
    # geom_ribbon(data = esm_ribbon,
    #             aes(x = lat,
    #                 ymin = lo_5, ymax = hi_95,
    #                 fill = ssp),
    #             alpha = 0.10, colour = NA) +
    geom_ribbon(data = esm_ribbon,
                aes(x = lat, ymin = lo_25, ymax = hi_75, fill = ssp),
                alpha = 0.2, colour = NA) +
    geom_line(data = future_med,
              aes(x = lat, y = med, 
                  colour = ssp),
              linewidth = 0.5) +
    geom_line(data = hist_line,
              aes(x = lat, y = med),
              colour = "grey20", linewidth = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3, linetype = "dotted") +
    scale_x_continuous(breaks = seq(-50, -5, by = 10),
                       labels = function(x) paste0(abs(x), "°S")) +
    coord_flip() +
    facet_wrap(~ term, nrow = 1) +
    scale_colour_manual(values = ssp_cols, name = NULL) +
    scale_fill_manual(values = ssp_cols, name = NULL) +
    labs(x = "Latitude",
         y = expression("Decadal acceleration (km decade"^{-2}*")")) +
    theme_linedraw(base_size = 10) +
    theme(#title = "",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(4, "pt"))
  
  p
  o_nm <- paste0(plot_fol, "/median_IQR_acceleration_across_ESMs_latitude_ssp-term.pdf") 
  ggsave(filename = o_nm, plot = p, height = 6, width = 6, dpi = 300)
  
  
  