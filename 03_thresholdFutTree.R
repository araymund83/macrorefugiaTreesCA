# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, rasterVis, stringr,tidyverse, terra)
g <- gc(reset = TRUE)
rm(list = ls())


# Load data  --------------------------------------------------------------
pathPres <- './inputs/tree_spp/pres_thresholded3/west'
pathFut <- './inputs/tree_spp/tree_fut3/west'

sp_codes <- readr::read_csv('./inputs/treeSp_codes.csv')
thrs <- read_csv('./inputs/treeCutoff.csv')

filesPres <- list.files(pathPres, pattern = '.tif$', full.names = TRUE)

dirsFut <- fs::dir_ls(pathFut, type = 'directory')
flsFut <- list.files (dirsFut, pattern =  '.tif$', full.names = TRUE)
#filter the files with Normal
# Exclude files containing "Normal"
flsFut <- flsFut[!stringr::str_detect(flsFut, "Normal")]

yrs <- c('2040', '2070', '2100')


# Extract species names (directory name after 'west/')
species <- unique(stringr::str_extract(flsFut, "(?<=west/)[^/]+"))


# Extract the GCM (first part of the model name)
gcms <- unique(stringr::str_extract(flsFut, "(?<=/)[^/]+(?=_ssp)"))


# Extract the SSP values (e.g., ssp126, ssp245)
ssps <- unique(stringr::str_extract(flsFut, "ssp[0-9]+"))



reclass_Ras <- function(sp, gcms){
 # sp <- species[1] # for testing
  message(crayon::blue('Starting with:', sp, '\n'))
  
  # Check if the species exists in the sp_codes tibble
  species_code <- sp_codes %>%
    filter(spcs == sp) %>%
    pull(code)
  
  if (length(species_code) == 0) {
    message(crayon::red('Species code not found for species:', sp, '\n'))
    return(NULL)
  }
  
  ssps<- ssps
  yrs <- c('2040', '2070', '2100')
  targetCRS <- '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  # Loop over GCMs (gcms)
  reclass <- map(.x = gcms, function(gcm) {
    message(crayon::blue('Processing for GCM:', gcm, '\n'))
    
    # Loop over SSPs
    map(.x = ssps, function(ssp) {
      message(crayon::blue('Processing for SSP:', ssp, '\n'))
     # browser()
      # Filter files based on species, SSP, and GCM
      flsFut_filtered <- grep(sp, flsFut, value = TRUE)  # Filter by species
      flsFut_filtered <- grep(ssp, flsFut_filtered, value = TRUE)  # Filter by SSP
      flsFut_filtered <- grep(gcm, flsFut_filtered, value = TRUE)  # Filter by GCM
      
      # Loop over years
      map(.x = yrs, .f = function(yr) {
        message(crayon::blue('Applying to year', yr, '\n'))
        
        # Filter by year
        fl <- grep(yr, flsFut_filtered, value = TRUE)  # Filter by year
      
        # Get the threshold value from thrs using the species code
        thr <- thrs %>%
          filter(Code == species_code) %>%
          pull(CutOffZhao)
        
        if (length(thr) == 0) {
          message(crayon::yellow('No threshold found for species code:', species_code, 'using default threshold 0.3'))
          val <- 0.3  # Default threshold if not found
        } else {
          val <- unique(thr)
        }
        
        # Process the raster
        rs <- terra::rast(fl)
        rs <- terra::project(rs, targetCRS, method = 'near')
        rs[rs < val] <- 0
        rs[rs >= val] <- 1
        
        # Write the raster
        out <- glue('./inputs/tree_spp/future_thresholded3/west')
        ifelse(!file.exists(out), dir.create(out), print('Already exists'))
        terra::writeRaster(rs, glue('{out}/{sp}_{gcm}_{ssp}_{yr}_thresholded_proj.tif'),
                           filetype = 'GTiff', datatype = 'INT4U', overwrite = TRUE,
                           gdal = c('COMPRESS=ZIP'))
        cat('=====Done========! \n')
      })
    })
  })
}
# Apply the function ------------------------------------------------------
reclass <- map(.x = species[4], .f = ~ reclass_Ras(sp = .x, gcms = gcms))

# Set up parallel processing (adjust the number of workers as needed)
future::plan(future::multisession, workers = 8)  # 
# Apply the function with parallelization for species 10 to 15
reclass <- furrr::future_map(.x = species[2], .f = ~ reclass_Ras(sp = .x, gcms = gcms))


reclass_Ras <- function(sp) {
  message(crayon::blue('Starting with:', sp, '\n'))
  
  # Filter files related to species and SSP only once
  flsFut <- filesFut[grepl(sp, filesFut)]
  if (length(flsFut) == 0) {
    message(crayon::yellow("No future files found for species:", sp))
    return(NULL)
  }
  
  # Loop through each SSP
  reclass_ssp <- purrr::map(ssp, function(s) {
    message(crayon::blue('Starting with SSP:', s))
    
    # Filter the files for the current SSP
    flsFut_ssp <- flsFut[grepl(s, flsFut)]
    if (length(flsFut_ssp) == 0) {
      message(crayon::yellow("No matching files found for SSP:", s))
      return(NULL)
    }
    
    # Loop through each year
    purrr::map(yrs, function(year) {
      message(crayon::blue('Processing year:', year))
      
      # Get the relevant file and threshold for the year
      fl <- flsFut_ssp[grepl(year, flsFut_ssp)]
      if (length(fl) == 0) {
        message(crayon::yellow("No matching file found for year:", year))
        return(NULL)
      }
      
      thr <- thrs %>% filter(Code == sp)
      val <- if (nrow(thr) == 0) {
        message(crayon::yellow('No threshold found for species:', sp, 'using default threshold 0.3'))
        0.3
      } else {
        unique(thr$CutOff)
      }
      
      # Load and project the raster
      rs <- tryCatch({
        terra::rast(fl) %>% terra::project('+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs', method = 'near')
      }, error = function(e) {
        message(crayon::yellow('Error loading raster:', fl))
        return(NULL)
      })
      
      if (is.null(rs)) return(NULL)
      
      # Apply threshold to raster
      rs[rs < val] <- 0
      rs[rs >= val] <- 1
      
      # Write the output
      output_dir <- './inputs/tree_spp/future_thresholded3/'
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      
      output_file <- glue('{output_dir}/{sp}_{s}_{year}_thresholded_proj.tif')
      terra::writeRaster(rs, output_file, filetype = 'GTiff', datatype = 'INT4U', overwrite = TRUE, gdal = c('COMPRESS=ZIP'))
      
      message(crayon::blue('Completed processing for', sp, s, year))
    })
  })
}

# Apply the function to all species
purrr::map(species, reclass_Ras)

