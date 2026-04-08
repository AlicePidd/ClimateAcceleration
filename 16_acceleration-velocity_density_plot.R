# Plotting density plots of velocity and acceleration inside and outside
  # Written by Alice P
    # 8 April 2026



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



  
# Folders ----------------------------------------------------------------------

  density_fol <- make_folder(source_disk, "9_density_aus_dfs", "")
  png_fol <- make_folder(source_disk, "10_density_aus_plot", "pngs") # Aus files
  pdf_fol <- make_folder(source_disk, "10_density_aus_plot", "pdfs") # Aus files
  
  

    
# Load data --------------------------------------------------------------------
  
  plot_df <- readRDS(paste0(density_fol, "median_values_velocity-accel_combined_df_allSSP_allterms.RDS"))
  plot_df
  hist_df <- readRDS(paste0(density_fol, "median_values_velocity-accel_combined_df_historical.RDS"))
  hist_df
  
  
# Find data range --------------------------------------------------------------
  
  range(plot_df$value) # -408.2199 3565.2945
  range(hist_df$value) # -401.0729 1937.1861
  
  
  
# Truncate data at 5-95th percentile -----------
  
  q_future <- plot_df %>%
    group_by(var) %>%
    summarise(q5 = round(quantile(value, 0.05)),
              q95 = round(quantile(value, 0.95)))
  q_future
    # # A tibble: 2 × 3
    #     var             q5   q95
    #     <chr>        <dbl> <dbl>
    #   1 acceleration    -5     3
    #   2 velocity         1   298
  
  q_hist <- hist_df %>%
    group_by(var) %>%
    summarise(q5  = round(quantile(value, 0.05)),
              q95 = round(quantile(value, 0.95)))
  q_hist
    # # A tibble: 2 × 3
    #     var             q5   q95
    #     <chr>        <dbl> <dbl>
    #   1 acceleration    -5    10
    #   2 velocity         9   216
  
  
  # Truncate per var
    plot_df1 <- plot_df %>%
      left_join(q_future, by = "var") %>%
      mutate(value = pmin(pmax(value, q5), q95)) %>%
      dplyr::select(-q5, -q95)
  
    hist_df1 <- hist_df %>%
      left_join(q_hist, by = "var") %>%
      mutate(value = pmin(pmax(value, q5), q95)) %>%
      dplyr::select(-q5, -q95)
  

    
  
# Plot -------------------------------------------------------------------------
  
  ssp_labels  <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
  term_order  <- c("near", "mid", "intermediate", "long")
  zone_cols   <- c("mpas" = "#2196F3", "nonmpas" = "#FF5722")
  
  make_plot <- function(df) {
    df %>%
      mutate(ssp = ssp_labels[ssp],
             var = factor(var, levels = c("velocity", "acceleration"))) %>%
      ggplot(aes(x = value, fill = zone, colour = zone)) +
      geom_density(alpha = 0.35, linewidth = 0.6) +
      facet_grid(ssp ~ var, scales = "free_x") +
      # facet_grid(ssp ~ var) +
      scale_fill_manual(values = zone_cols) +
      scale_colour_manual(values = zone_cols) +
      labs(x = NULL, y = "Density", fill = NULL, colour = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top",
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(face = "bold"))
  }
  
  # Main text (mid-term only)
  p_main <- plot_df1 %>% 
    filter(term == "mid") %>% 
    make_plot()
  p_main
  
  # Supplement (all terms, facet by term too)
  p_supp <- plot_df1 %>%
    mutate(term = factor(term, levels = term_order)) %>%
    make_plot() +
    facet_grid(ssp ~ var + term, scales = "free_x")
  p_supp
  
  # ggsave("figures/density_mid.pdf",  p_main, width = 8,  height = 10)
  # ggsave("figures/density_supp.pdf", p_supp, width = 16, height = 12)  
  
  
  
  
  
  plot_df1 %>%
    filter(var == "velocity") %>%
    pull(value) %>%
    hist(breaks = 50)
  
  
  