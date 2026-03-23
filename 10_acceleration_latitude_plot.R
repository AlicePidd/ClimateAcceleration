# Plotting climate acceleration by 1° latitude
  # Written by Alice P
    # 6 March 2026

# Acceleration at each 1° of latitude - median of each band, per SSP and term


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")
  
  
  
  
# Folders ----------------------------------------------------------------------

  acc_fol <- make_folder(source_disk, "4_acceleration_aus_terms", "decadal")
  median_dfs_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "dfs")
  median_rast_fol <- make_folder(source_disk, "5_accleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "7_acceleration_aus_plot", "latitude") # Aus files
  
  
  
# Read all files, extract values + lat --------------------------
  
  files <- dir(acc_fol, full.names = TRUE, pattern = ".RDS")
  
  raw_dat <- map(files, function(f) {
    r <- readRDS(f)
    
    vals <- values(r, dataframe = TRUE) %>%
      as_tibble() %>%
      bind_cols(crds(r, df = TRUE, na.rm = FALSE) %>% as_tibble()) %>%
      pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "accel") %>%
      mutate(
        ssp  = str_split_i(layer, "_", 3),
        esm  = str_split_i(layer, "_", 4),
        term = str_split_i(layer, "_", 5)
      ) %>%
      dplyr::select(lat = y, accel, ssp, esm, term)
    
    return(vals)
  }) %>%
    bind_rows() %>%
    filter(!is.na(accel))
  
  raw_dat
  
  
  # Factor terms in both datasets
  term_levels <- term_list
  term_labels  <- c("Recent-term", "Near-term", "Mid-term", "Intermediate-term", "Long-term")
  
  raw_dat <- raw_dat %>%
    mutate(term = factor(term, levels = term_levels, labels = term_labels))
  raw_dat
  
  esm_meds <- raw_dat %>% 
    group_by(lat, esm, ssp, term) %>%
    summarise(esm_med = median(accel, na.rm = TRUE),
              .groups = "drop")
  esm_meds
    
  all_med <- raw_dat %>%
    group_by(lat, ssp, term) %>%
    summarise(
      med = median(accel, na.rm = TRUE),
      .groups = "drop")
  all_med
  
  
  
  
  
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
    dplyr::select(lat, med) %>%
    expand_grid(term = unique(future_med$term))
  hist_line
  
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
  
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# Get median acceleration (slope) per 1° latitude ------------------------------

  files <- dir(acc_fol, full.names = TRUE, pattern = ".RDS") #%>% # Hast to be rasts
    # str_subset(., "historical", negate = TRUE)
  files
  f <- files[2]
  f

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
                .groups = "drop") %>% 
      mutate(ssp = str_split_i(layer, "_", 3),
             esm = str_split_i(layer, "_", 4),
             term = str_split_i(layer, "_", 5)) %>% 
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
  out
  out %>% distinct(ssp, term, esm) %>% count(ssp, term)
  
  out %>% 
    filter(!is.na(median_slope)) %>% 
    summarise(lat_min = min(lat), lat_max = max(lat), n = n())
  
  out %>% filter(lat == -25, term == "mid", ssp == "ssp245")
  
  
  
