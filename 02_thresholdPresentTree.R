#This script get the files form the species-consensus folder
# Load libraries ----------------------------------------------------------
library(pacman)
p_load(dplyr, fs,glue, qs, readr, stringr)
g <- gc(reset = TRUE)
rm(list = ls())

# Load data  --------------------------------------------------------------
path <- './inputs/tree_spp/pres_tree_3/west'
thrs <- readr::read_csv('./inputs/treeCutoff.csv')
sp_codes <- readr::read_csv('./inputs/treeSp_codes.csv')
files <- list.files(path, pattern= '.tif', full.names = TRUE)
# Extract species names
#species <- unique(str_extract(files, "(?<=/pres_tree_3/)[A-Z]+"))
species <- gsub(".*/|_Normal.*", "", files)
species <- unique(species)


baseline <- c('1961','1991' )
targetCRS <- '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

reclass_raster <- function(sp){
  #sp <- species[1]
  cat('Start ', sp, '\n')
  fl <- grep(sp, files, value = TRUE)
  # Check if any files are found for the species
  if (length(fl) == 0) {
    message(crayon::red('No files found for species:', sp, '\n'))
    return(NULL)
  }
  # Load the species code mapping (replace 'species_code_df' with your actual dataframe or list)
  # Check if the species exists in the sp_codes tibble
  species_code <- sp_codes %>%
    filter(spcs == sp) %>%
    pull(code)
  
  if (length(species_code) == 0) {
    message(crayon::red('Species code not found for species:', sp, '\n'))
    return(NULL)
  }
  
      rs <- map(.x = 1:length(baseline), function(i){
        message(crayon::blue('Applying to year', baseline[i], '\n'))
        #browser()
        flPres <- grep(baseline[i], fl, value = TRUE)
       
        # Check if the file exists for the given baseline year
        if (length(flPres) == 0) {
          message(crayon::red('No files found for year:', baseline[i], '\n'))
          return(NULL)
        }
        
        # Check if the file path exists before attempting to read it
        if (!file.exists(flPres)) {
          message(crayon::red('File does not exist:', flPres, '\n'))
          return(NULL)
        }
        
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
        rs <- terra::rast(flPres)
        rs2 <- terra::project(rs, targetCRS, method = 'near')
        rs2[rs2 < val] <- 0
        rs2[rs2 >= val] <- 1
        # Write the rasters
        out <- glue('./inputs/tree_spp/tree_spp3/pres_thresholded3/west')
        ifelse(!file.exists(out), dir_create(out), print('Already exists'))
        terra::writeRaster(rs2,glue('{out}/{sp}_{baseline[i]}_tresholded_proj.tif'),
          filetype = 'GTiff', datatype = 'INT4U', overwrite = TRUE)

        message(crayon::blue('DONE', '\n'))
       })
 
}

# Apply the function ------------------------------------------------------
reclass<- map(.x = species, reclass_raster)


