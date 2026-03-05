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
	


		