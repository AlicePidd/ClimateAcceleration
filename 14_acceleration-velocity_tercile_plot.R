# Plotting velcoity and acceleration overlap spatially as terciles
  # Written by Alice P
    # 30 March 2026
  # Inspired by Brito-Morales et al. 2020



# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/acceleration_data"
  source("Background_data.R")



# Folders ----------------------------------------------------------------------

  vocc_fol <- make_folder(source_disk, "3_velocity_decadal_median_terms", "rasts") 
  accel_fol <- make_folder(source_disk, "5_acceleration_aus_median_terms", "rasts")
  plot_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial") # Aus files
  pdf_fol <- make_folder(source_disk, "8_tercile_aus_plot", "spatial/pdfs") # Aus files
  
  
  
# Load rasters -----------------------------------------------------------------
  
  vel_files   <- dir(vocc_fol, full.names = TRUE, pattern = ".RDS") %>%
    str_subset("historical", negate = TRUE)
 
  accel_files <- dir(accel_fol,  full.names = TRUE, pattern = ".RDS") %>%
    str_subset("historical", negate = TRUE)
  
    load_stack <- function(files) {
      f <- files
      readRDS(f)[[1]]
    }

  vel_stack <- map(vel_files, load_stack)
  accel_stack <- map(accel_files, load_stack)
  
  
  
  
  
# Bivariate classification and terciles ----------------------------------------
  # Splits into terciles, combines into codes 11-33
  # (velocity tercile * 10 + acceleration tercile; 1 = low, 3 = high)
 
  ## Pool values across every combo to get global breaks
    all_vel_vals <- map(vel_stack, \(r) values(r, na.rm = TRUE)) %>% 
      unlist()
    all_accel_vals <- map(accel_stack, \(r) values(r, na.rm = TRUE)) %>% 
      unlist()
    
    vel_breaks <- quantile(all_vel_vals, c(1/3, 2/3))  # two cut points
    accel_breaks <- quantile(all_accel_vals, c(1/3, 2/3))
  
  
  ## Classify each rast using fixed global breaks
    # ≤ 1st tercile = 1
    # > 1st and ≤ 2nd = 2
    # > 2nd = 3
    
    tercile_fixed <- function(r, breaks) {
      classify(r, rcl = matrix(c(-Inf, breaks[1], 1,
                                 breaks[1], breaks[2], 2,
                                 breaks[2], Inf, 3),
                               ncol = 3, byrow = TRUE))
      }
  
    bivar_classify_global <- function(r_vel, r_acc) {
      tercile_fixed(r_vel, vel_breaks) * 10 + tercile_fixed(r_acc, accel_breaks)
    }
    
    bivar_rasts <- map2(vel_stack, accel_stack, bivar_classify_global)

  
  
  
  
# Plot -------------------------------------------------------------------------
  
  term_order <- c("near", "mid", "intermediate", "long")
  combos <- expand_grid(ssp = ssp_list, term = term_list[2:5])  # adjust indices as needed
  combos
  ssp = "ssp245"
  
  plot_bivariate <- function(ssp, pal_name) {
 
    # Order panels by term_order
    panel_idx <- which(combos$ssp == ssp)
 
    # Maps
      map_dfs <- map(bivar_rasts[panel_idx], function(r) {
        as.data.frame(r, xy = TRUE) %>% # Turn it into a df
          rename(code = 3) %>% # rename 3rd column to code
          drop_na() %>%
          mutate(code = factor(code)) # change it to a factor
      })
    
      make_map <- function(df, ssp, term) {
        ggplot(data = df) + 
          geom_raster(aes(x, y, 
                          fill = code)) +
          scale_fill_manual(values = bivar_pal, drop = FALSE, guide = "none") +
          geom_sf(data = eez_shp, fill = NA, 
                  colour = "black", linewidth = 0.3, 
                  inherit.aes = FALSE) +
          geom_sf(data = oceania_stanford_shp, 
                  fill = "grey80", colour = NA, 
                  inherit.aes = FALSE) +
          coord_sf(expand = FALSE, xlim = c(105, 180), ylim = c(-50, -5)) +
          labs(title = toupper(ssp), subtitle = term) +
          theme_void(base_size = 9) +
          theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, colour = "grey40"))
      }
      
      combos_ssp <- combos %>%
        filter(ssp == !!ssp) # Filter by ssp, need !! so it knows its a separate variable that has been passed
      combos_ssp$r <- bivar_rasts[panel_idx]
      combos_ssp <- combos_ssp %>%
        mutate(df = map(r, \(r) as.data.frame(r, xy = TRUE) %>%
                          rename(code = 3) %>%
                          drop_na() %>%
                          mutate(code = factor(code))))
      
      map_panels <- pmap(list(combos_ssp$df, combos_ssp$ssp, combos_ssp$term), make_map)

    # Legend
      legend <- expand_grid(vel = 1:3, acc = 1:3) %>%
        mutate(code  = factor(vel * 10 + acc),
               vel_l = factor(vel, 1:3, c("Slow", "Med", "Fast")),
               acc_l = factor(acc, 1:3, c("Low",  "Med", "High"))) %>%
        ggplot(aes(acc_l, vel_l, fill = code)) +
        geom_tile(colour = "white", linewidth = 0.8) +
        scale_fill_manual(values = bivar_pal, guide = "none") +
        labs(x = "Acceleration \u2192", y = "Velocity \u2192") +
        theme_minimal(base_size = 8) +
        theme(panel.grid = element_blank(), aspect.ratio = 1)
 
    # Bar chart
      corner_codes <- c("Slow vel, low acc"  = 11, "Slow vel, high acc" = 13,
                        "Fast vel, low acc"  = 31, "Fast vel, high acc" = 33)
      
      bar_data <- combos_ssp %>%
        mutate(term = factor(term, levels = term_order)) %>%
        rowwise() %>%
        reframe(
          term     = term,
          category = names(corner_codes),
          prop     = map_dbl(corner_codes, \(code) mean(values(r, na.rm = TRUE)[, 1] == code) * 100)
        )
   
      bar <- bar_data %>%
        mutate(category = factor(category, levels = names(corner_pal))) %>%
        ggplot(aes(term, prop, fill = category)) +
        geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
        scale_fill_manual(values = corner_pal, name = NULL) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = NULL, y = "Proportion of cells (%)") +
        theme_classic(base_size = 9) +
        theme(axis.text.x     = element_text(angle = 35, hjust = 1),
              legend.position = "bottom",
              legend.text     = element_text(size = 7)) +
        guides(fill = guide_legend(ncol = 1))
 
    # Assemble and save
      fig <- wrap_plots(
        wrap_plots(map_panels, ncol = 1),
        wrap_plots(legend, bar, plot_spacer(), ncol = 1, heights = c(1, 1, 2)),
        ncol = 2, widths = c(4, 1)
      ) +
        plot_annotation(tag_levels = "a") &
        theme(plot.margin = margin(10, 10, 10, 10))
   
      o_nm <- paste0(plot_fol, "/velocity_acceleration_bivariate_", ssp, "_pal-", pal_name, ".png")
      ggsave(filename = o_nm, plot = fig, width = 12, height = 20)
      
      o_nm_pdf <- paste0(pdf_fol, "/velocity_acceleration_bivariate_", ssp, "_pal-", pal_name, ".pdf")
      ggsave(filename = o_nm_pdf, plot = fig, width = 12, height = 20)
      
      message("Saved: ", basename(o_nm))
  }
 

    
# Palettes ----------------------------------------------------------------------
  # First digit = velocity tercile (1 = slow, 3 = fast)
  # Second digit = acceleration tercile (1 = low, 3 = high)
 
  # ## tealochre
  #   bivar_pal <- c("11"="#F8F0D0","12"="#E8B830","13"="#A07800",
  #                  "21"="#60C8D0","22"="#789080","23"="#604800",
  #                  "31"="#004858","32"="#003038","33"="#001810")
  #   corner_pal <- c("Slow vel, low acc" ="#F8F0D0",
  #                   "Slow vel, high acc"="#A07800",
  #                   "Fast vel, low acc" ="#004858",
  #                   "Fast vel, high acc"="#001810")
  #   walk(ssp_list, ~ plot_bivariate(.x, pal_name = "tealochre"))
  
  
  ## tealochre1
    ##**THIS ONE**
    bivar_pal <- c("11"="#F8F4E4","12"="#F0C050","13"="#DA9500",
                   "21"="#60C8D0","22"="#789080","23"="#513700",
                   "31"="#008089","32"="#003F5A","33"="#001911")
    corner_pal <- c("Slow vel, low acc" ="#F8F4E4",
                    "Slow vel, high acc"="#DA9500",
                    "Fast vel, low acc" ="#008089",
                    "Fast vel, high acc"="#001911")
    walk(ssp_list, ~ plot_bivariate(.x, pal_name = "tealochre1"))
  
  
  ## tealcoral
    bivar_pal <- c("11" = "#f2f2f2", "12" = "#f0b89a", "13" = "#d4522a", # #f0b89a
                   "21" = "#7abfbf", "22" = "#9a9a8a", "23" = "#8a3a2a",
                   "31" = "#0a4a4a", "32" = "#3a3a3a", "33" = "#6a0a0a")
    
    corner_pal <- c("Slow vel, low acc"  = "#f2f2f2",
                    "Slow vel, high acc" = "#d4522a",
                    "Fast vel, low acc"  = "#0a4a4a",
                    "Fast vel, high acc" = "#6a0a0a")
    walk(ssp_list, ~ plot_bivariate(.x, pal_name = "tealcoral"))
  
  
  ## ambernavy
    bivar_pal <- c("11"="#E9E9D8","12"="#F0C050","13"="#C77100",
                   "21"="#88C8E0","22"="#80a070","23"="#7D7466",
                   "31"="#095B7F","32"="#223153","33"="#0A1808")
    corner_pal <- c("Slow vel, low acc" ="#E9E9D8",
                    "Slow vel, high acc"="#C77100",
                    "Fast vel, low acc" ="#223153",
                    "Fast vel, high acc"="#0A1808")
    walk(ssp_list, ~ plot_bivariate(.x, pal_name = "ambernavy"))
  
  
  ## ambernavy1
    bivar_pal <- c("11"="#E9E9D8","12"="#F0C050","13"="#D66305",
                   "21"="#79B9BF","22"="#6A7C82","23"="#5D3636",
                   "31"="#095B7F","32"="#223153","33"="#0A1808")
    corner_pal <- c("Slow vel, low acc" ="#E9E9D8",
                    "Slow vel, high acc"="#D66305",
                    "Fast vel, low acc" ="#223153",
                    "Fast vel, high acc"="#0A1808")
    walk(ssp_list, ~ plot_bivariate(.x, pal_name = "ambernavy1"))
  
  
  
  
  
  # bivar_pal <- c("11"="#FFFACC","12"="#FFD000","13"="#C06000",
  #                "21"="#50A8FF","22"="#6080A0","23"="#804000",
  #                "31"="#001880","32"="#001050","33"="#100800")
  # corner_pal <- c("Slow vel, low acc" ="#FFFACC",
  #                 "Slow vel, high acc"="#C06000",
  #                 "Fast vel, low acc" ="#001880",
  #                 "Fast vel, high acc"="#100800")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "electricsaffron"))
    
    
  # ## rothko
  # bivar_pal <- c("11"="#f2e9d8", "12"="#d4c5a9", "13"="#b5a68a",
  #                "21"="#4a4e69", "22"="#c9a96e", "23"="#e07b39",
  #                "31"="#1a1a2e", "32"="#8b3a3a", "33"="#c0392b")
  # corner_pal <- c("Slow vel, low acc" ="#f2e9d8",
  #                 "Slow vel, high acc"="#b5a68a",
  #                 "Fast vel, low acc" ="#1a1a2e",
  #                 "Fast vel, high acc"="#c0392b")
  
  # ## hopper
  # bivar_pal <- c("11"="#EEEEEE", "12"="#d6c9b0", "13"="#c9a96e",
  #                "21"="#5b8db8", "22"="#c4b49a", "23"="#e8936a",
  #                "31"="#0d3b6e", "32"="#1a6b8a", "33"="#d4542a")
  # corner_pal <- c("Slow vel, low acc" ="#EEEEEE",
  #                 "Slow vel, high acc"="#c9a96e",
  #                 "Fast vel, low acc" ="#0d3b6e",
  #                 "Fast vel, high acc"="#d4542a")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "hopper"))
  # 
  
  # ## navycream
  # bivar_pal <- c("11"="#f5f0e8", "12"="#d4c9a8", "13"="#a8c5a0",
  #                "21"="#6b9ab8", "22"="#b8a882", "23"="#5b9e8a",
  #                "31"="#0d2b4e", "32"="#1a5276", "33"="#117a65")
  # corner_pal <- c("Slow vel, low acc" ="#f5f0e8",
  #                 "Slow vel, high acc"="#a8c5a0",
  #                 "Fast vel, low acc" ="#0d2b4e",
  #                 "Fast vel, high acc"="#117a65")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "navycream"))
  
  
  # ## blurddiv
  # bivar_pal <- c("11"="#e8eef5", "12"="#d4c5b0", "13"="#f0d4a8",
  #                "21"="#7bafd4", "22"="#c8b89a", "23"="#e8956a",
  #                "31"="#1a4d7a", "32"="#7a4a2a", "33"="#b03a1a")
  # corner_pal <- c("Slow vel, low acc" ="#e8eef5",
  #                 "Slow vel, high acc"="#f0d4a8",
  #                 "Fast vel, low acc" ="#1a4d7a",
  #                 "Fast vel, high acc"="#b03a1a")
  
  # ## navymint
  # bivar_pal <- c("11"="#e8f5f0", "12"="#b8e0d0", "13"="#6ecfb0",
  #                "21"="#5b9eb8", "22"="#3a8a8a", "23"="#1a7a6a",
  #                "31"="#1a2a5e", "32"="#1a3a6a", "33"="#0a5a5a")
  # corner_pal <- c("Slow vel, low acc" ="#e8f5f0",
  #                 "Slow vel, high acc"="#6ecfb0",
  #                 "Fast vel, low acc" ="#1a2a5e",
  #                 "Fast vel, high acc"="#0a5a5a")
  
  # ## hiroshige
  # bivar_pal <- c("11"="#FFE6B7","12"="#F7AA58","13"="#E76254",
  #                "21"="#AADCE0","22"="#a0a080","23"="#905040",
  #                "31"="#376795","32"="#528FAD","33"="#1E466E")
  # corner_pal <- c("Slow vel, low acc" ="#FFE6B7",
  #                 "Slow vel, high acc"="#E76254",
  #                 "Fast vel, low acc" ="#376795",
  #                 "Fast vel, high acc"="#1E466E")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "hiroshige"))
  
  
  # ## ustwo
  # bivar_pal <- c("11"="#FFE6B7","12"="#F7AA58","13"="#D7433B",
  #                "21"="#95CAA6","22"="#9a9a8a","23"="#a03030",
  #                "31"="#008D98","32"="#3a3a3a","33"="#702020")
  # corner_pal <- c("Slow vel, low acc" ="#FFE6B7",
  #                 "Slow vel, high acc"="#D7433B",
  #                 "Fast vel, low acc" ="#008D98",
  #                 "Fast vel, high acc"="#702020")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "ustwo"))
  
  # ## sagelav
  # bivar_pal <- c("11"="#f0ede4","12"="#b8d4b0","13"="#4a8a5a",
  #                "21"="#c8b8d8","22"="#9a9aaa","23"="#3a6a4a",
  #                "31"="#6a3a8a","32"="#4a3a6a","33"="#1a2a1a")
  # corner_pal <- c("Slow vel, low acc" ="#f0ede4",
  #                 "Slow vel, high acc"="#4a8a5a",
  #                 "Fast vel, low acc" ="#6a3a8a",
  #                 "Fast vel, high acc"="#1a2a1a")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "sagelav"))
  
  
  # ## limegrape
  # bivar_pal <- c("11"="#f4f2e4","12"="#b8d468","13"="#4a7010",
  #                "21"="#c8a0c0","22"="#907a58","23"="#304a08",
  #                "31"="#780a60","32"="#4a0a38","33"="#181008")
  # corner_pal <- c("Slow vel, low acc" ="#f4f2e4",
  #                 "Slow vel, high acc"="#4a7010",
  #                 "Fast vel, low acc" ="#780a60",
  #                 "Fast vel, high acc"="#181008")
  # walk(ssp_list, ~ plot_bivariate(.x, pal_name = "limegrape"))
  
  # beep(2)
  