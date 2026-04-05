# Plotting climate acceleration by latitude - spatial spread of median acceleration per 1°, with 1° min and max\
  # Written by Alice P
    # 5 Apr 2026

# Median acceleration at each 1° of latitude for each ESM - plotting the median across all ESM medians for each 1°, and a ribbon with the min and max value at each 1° lat.

# This plot the SPATIAL SPREAD of acceleration by latitude


# Need:
  # ensemble median per lat
  # spread of values across each lat (min max of the median)


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
# Folders ----------------------------------------------------------------------

  acc_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "decadal")
  # median_dfs_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "dfs")
  # median_rast_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "7_acceleration_aus_plot", "latitude") # Aus files
  
  
  
# Read all files, extract values + lat --------------------------
  
  files <- dir(acc_fol, full.names = TRUE, pattern = ".RDS")
  f <- files[2]
  
  raw_dat <- map(files, function(f) {
    r <- readRDS(f)
    
    # vals <- values(r, dataframe = TRUE) %>%
    #   as_tibble() %>%
    #   bind_cols(crds(r, df = TRUE, na.rm = FALSE) %>% 
    #               as_tibble()) %>%
    #   pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "accel") %>%
    #   mutate(
    #     ssp  = str_split_i(layer, "_", 3),
    #     esm  = str_split_i(layer, "_", 4),
    #     term = str_split_i(layer, "_", 5)
    #   ) %>%
    #   dplyr::select(lat = y, accel, ssp, esm, term)
    # 
    # return(vals)
    
    vals <- values(r, dataframe = TRUE) %>%
      as_tibble() %>%
      bind_cols(crds(r, df = TRUE, na.rm = FALSE) %>% 
                  as_tibble()) %>%
      pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "accel") %>%
      mutate(
        ssp  = str_split_i(layer, "_", 3),
        esm  = str_split_i(layer, "_", 4),
        term = str_split_i(layer, "_", 5)
      ) %>%
      dplyr::select(lat = y, accel, ssp, esm, term)
    
    med_vals <- vals %>% 
      mutate(lat_cat = ceiling(lat)) %>% 
      group_by(ssp, term, lat_cat) %>% 
      mutate(med_lat_accel = median(accel, na.rm = TRUE))
    
    return(med_vals)
  }) %>%
    bind_rows() %>%
    # filter(!is.na(accel))
    filter(!is.na(med_lat_accel))
  
  raw_dat
    # Should have some accel values that are NA while corresponding med_accel values are not NA. This is cus I'm computing med accel across the whole lat_cat (1°), so there will be some NA accels within in each band.
  
  # Factor terms in both datasets
  term_levels <- term_list
  term_labels  <- c("Recent-term", "Near-term", "Mid-term", "Intermediate-term", "Long-term")
  
  raw_dat <- raw_dat %>%
    mutate(term = factor(term, levels = term_levels, labels = term_labels))
  raw_dat
  
  # esm_meds <- raw_dat %>% 
  #   group_by(lat, esm, ssp, term) %>%
  #   summarise(esm_med = median(accel, na.rm = TRUE),
  #             .groups = "drop")
  # esm_meds
  #   
  # all_med <- raw_dat %>%
  #   group_by(lat, ssp, term) %>%
  #   summarise(
  #     med = median(accel, na.rm = TRUE),
  #     .groups = "drop")
  # all_med
  
  
# Get median per lat across all ESMs (per ssp-term) ----------------------------
  
  all_med <- raw_dat %>%
    group_by(lat_cat, ssp, term) %>%
    summarise(
      med = median(med_lat_accel, na.rm = TRUE),
      min = min(med_lat_accel, na.rm = TRUE),
      max = max(med_lat_accel, na.rm = TRUE),
      .groups = "drop")

  range(all_med$med)
  
  esm_range <- raw_dat %>%
    filter(ssp != "historical") %>% 
    group_by(lat_cat, ssp, term) %>%
    summarise(
      # med_lat_accel = med_lat_accel,
      lo_5 = quantile(med_lat_accel, 0.05, na.rm = TRUE),
      hi_95 = quantile(med_lat_accel, 0.95, na.rm = TRUE),
      lo_25 = quantile(med_lat_accel, 0.25, na.rm = TRUE),
      hi_75 = quantile(med_lat_accel, 0.75, na.rm = TRUE),
      .groups = "drop")
  esm_range

  

  
  
# Plot -------------------------------------------------------------------------
  
  ssp_cols <- c(#"historical" = "#5F5E5A",
                "ssp126" = col_ssp126,
                "ssp245" = col_ssp245,
                "ssp370" = col_ssp370,
                "ssp585" = col_ssp585)
  
  future_med <- all_med %>% filter(ssp != "historical")
  future_med
  hist_line <- all_med %>% 
    filter(ssp == "historical") %>%
    dplyr::select(lat_cat, med) %>% # Won't have any min/max because it is only the one "ESM" as this is OISST data
    expand_grid(term = unique(future_med$term))
  hist_line
  

  p <- ggplot() +
      # geom_ribbon(data = esm_ribbon,
      #             aes(x = lat,
      #                 ymin = lo_5, ymax = hi_95,
      #                 fill = ssp),
      #             alpha = 0.10, colour = NA) +
      geom_ribbon(data = esm_range,
                  aes(x = lat_cat, ymin = lo_25, ymax = hi_75, fill = ssp), # IQR
                  alpha = 0.2, colour = NA) +
      # geom_ribbon(data = esm_range,
      #           aes(x = lat_cat, ymin = lo_5, ymax = hi_95, fill = ssp), # 5-95th percentiles
      #           alpha = 0.2, colour = NA) +
      geom_line(data = future_med,
                aes(x = lat_cat, y = med, 
                  colour = ssp),
                linewidth = 0.5) +
      geom_line(data = hist_line,
                aes(x = lat_cat, y = med),
                colour = "grey20", linewidth = 0.5, linetype = "dashed") +
      geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3, linetype = "dotted") +
      scale_x_continuous(breaks = seq(-50, -5, by = 10),
                         labels = function(x) paste0(abs(x), "°S")) +
      coord_flip() +
      facet_wrap(~ term, nrow = 1) +
      scale_colour_manual(values = ssp_cols, name = NULL) +
      scale_fill_manual(values = ssp_cols, name = NULL) +
      labs(x = "Latitude",
           y = expression("Climate acceleration (km decade"^{-2}*")")) +
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
  o_nm <- paste0(plot_fol, "/median_IQR_acceleration_across_ESMs_latitude_ssp-term.png") 
  ggsave(filename = o_nm, plot = p, height = 6, width = 6, dpi = 300)
  
    
    
