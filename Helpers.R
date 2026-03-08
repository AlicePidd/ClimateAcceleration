# Helper functions for thesis chapter 3: Climate connectivity
	# For working with climate velocity tracers
    # Adapted from Chp2 Helpers.R
  	# Updated by Alice, Feb 2025



# Packages ---------------------------------------------------------------------

pacman::p_load(pacman, tidyverse, purrr, furrr, ncdf4, terra, sf, tmap, beepr, tictoc,  parallel, parallelly, patchwork, ggrepel, ggthemes, rmapshaper, lwgeom, progressr, data.table, vegan, vctrs, gplots, mgcv, mgcViz, glmmTMB, gratia,  
               viridis, viridisLite, MoMAColors, PNWColors, NatParksPalettes, rcartocolor, ltc, scico, rcartocolor, MetBrewer, feathers, futurevisions, paletteer,
               CircStats
               # gglm, boot, GGally, MASSExtra, visreg, marginaleffects, 
               # glmmTMB, sdmTMB, gstat, spdep, DHARMa, performance, inlabru, modelbased # From the R workshop on spatiotemporal autocorrelation
               )

library("pushoverr")


# Palettes ----------------------------------------------------------------

  # col_ssp119 <- rgb(84, 39, 143, maxColorValue = 255)
  col_ssp126 <- rgb(0, 52, 102, maxColorValue = 255)
  col_ssp245 <- rgb(112, 160, 205, maxColorValue = 255)
  col_ssp370 <- rgb(196, 121, 0, maxColorValue = 255)
  # col_ssp534_over <- rgb(196, 121, 0, maxColorValue = 255)
  col_ssp585 <- rgb(153, 0, 2, maxColorValue = 255)
  
  IPCC_pal <- c("ssp126" = col_ssp126, "ssp245" = col_ssp245, "ssp370" = col_ssp370, "ssp585" = col_ssp585)




# Folder functions -------------------------------------------------------------
  	
	make_folder <- function(d, m, fol_dir_name) {
	  
	  folder_path <- file.path(paste0(d, "/", m, "/", fol_dir_name))
	  if (!dir.exists(folder_path)) {
	    dir.create(folder_path, recursive = TRUE)
	    message("✅ Folder created: ", folder_path)
	  } else {
	    message("📂 Folder already exists: ", folder_path)
	  }
	  return(folder_path)
	}
	


# Get filename bits ----------------------
  
  get_CMIP6_bits_dave <- function(file_name) {
    bits <- str_split(basename(file_name), "_") %>% 
      unlist()
    date_start_stop <- bits[7] %>% 
      str_split("[.]") %>%
      map(1) %>% 
      unlist() %>% 
      str_split("-") %>%
      unlist()
    if(str_detect(file_name, "_.mon_")) {
      date_start_stop <- paste0(date_start_stop, c("01", "31"))
    } # Fix dates for monthly data
    # if(str_detect(file_name, "_.year_")) {
    if(str_detect(file_name, "_.day_")) {
      date_start_stop <- paste0(date_start_stop, c("0101", "1231"))
    } # Fix dates for annual data
    date_start_stop <- as.Date(date_start_stop, format = "%Y%m%d")
    output <- list(Variable = bits[1],
                   Frequency = bits[2],
                   Model = bits[3],
                   Scenario = bits[4],
                   Variant = bits[5],
                   Grid = bits[6],
                   Year_start = date_start_stop[1],
                   Year_end = date_start_stop[2])
    return(output)
    # e.g., map_df(dir(folder), get_CMIP6_bits)
  }
  
		