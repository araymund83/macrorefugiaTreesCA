# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, furrr, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra, yaImpute,fs)
g <- gc(reset = TRUE)
rm(list = ls())

# Functions ---------------------------------------------------------------
source('./R/fatTail.R')
# Load data ---------------------------------------------------------------
pathFut <- 'inputs/tree_spp/future_thresholded3/west'
pathPres <- 'inputs/tree_spp/pres_thresholded3/west'

flsPres <- list.files(pathPres, pattern = '.tif', full.names = TRUE)
flsFut <- list.files(pathFut, pattern = '.tif', full.names = TRUE)
species <- sub(".*/([^_]+)_.*", "\\1", flsFut)
species <- unique(species)


# Extract GCM and SSP 
gcms <- gsub(".*_([A-Za-z0-9-]+(?:\\.[0-9]+)?)_ssp.*", "\\1", flsFut)# Extract GCM
gcms<- unique(gcms)

ssps <- gsub(".*_(ssp[0-9]+)_.*", "\\1", flsFut)  # Extract SSP
ssps <- unique(ssps)

yrs <- c('2040', '2070', '2100')

baseline <- c('1961','1991')

# Velocity metric ---------------------------------------------------------
get_backward_velocity <- function(sp, baseline){
 #sp <- species[2] # use for testing
  message(crayon::blue('Starting with:', sp))
  flFut <- grep(sp, flsFut, value = TRUE)
  flPres <- grep(sp, flsPres, value = TRUE)
  

 
  rsltdo <- map(.x = 1:length(ssps), function(k){
    message(crayon::blue('Applying to ssp', ssps[k]))
    flFut <- grep(ssps[k], flFut, value = TRUE)
  #browser()
    rs <- map(.x = 1:length(yrs), .f = function(i){
      message(crayon::blue('Applying to year',yrs[i], '\n'))
      
      
      flPres <- grep(baseline, flPres, value = TRUE)
      flFut_filter <- grep(yrs[i], flFut, value = TRUE)
      flFut_filter <- grep(ssps[k], flFut_filter, value = TRUE)
     
       vel <- map(.x = 1:length(gcms), .f = function(j){
        message(crayon::blue('Applying to gcm',gcms[j], '\n'))
        flFut_filter <- grep(gcms[j],  flFut_filter, value = TRUE)
        rstPres <- terra::rast(flPres)
        rstFut <- terra::rast(flFut_filter)
        emptyRas <- rstPres * 0 + 1
     
        
        tblPres <- terra::as.data.frame(rstPres, xy = TRUE)
        colnames(tblPres)[3] <- 'prev'
        tblFut <- terra::as.data.frame(rstFut, xy = TRUE)
        colnames(tblFut)[3] <-  'prev'
        
        p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>%
          dplyr::select(pixelID, x, y, prev)
        f.xy <- mutate(tblFut, pixelID = 1:nrow(tblFut)) %>% 
          dplyr::select(pixelID, x, y, prev)
        
        p.xy2 <- filter(p.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
        f.xy2 <- filter(f.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
        
        if (nrow(f.xy) > 0) {
          d.ann <- as.data.frame(ann(
            as.matrix(p.xy2[, -1, drop = FALSE]),
            as.matrix(f.xy2[, -1, drop = FALSE]),
            k = 1,
            verbose = F)$knnIndexDist)
          d1b <- as.data.frame(cbind(f.xy2, round(sqrt(d.ann[, 2]))))
          names(d1b) <- c("ID", "X", "Y", "bvel")
        } else {
          print(spec[i])
        }
        f.xy <- as.data.frame(f.xy)
        colnames(f.xy) <- c('ID', 'X', 'Y', 'Pres')
        f.xy <- as_tibble(f.xy)
        d1b <- left_join(f.xy, d1b, by = c('ID', 'X', 'Y'))
        d1b <- mutate(d1b, fat = fattail(bvel, 8333.3335, 0.5))
        sppref <- rast(d1b[, c(2, 3, 6)])
        sppref[is.na(sppref)] <- 0
        crs(sppref) <- crs(emptyRas)
        refstack <- sppref
        # rstFut <- crop(rstFut,emptyRas)
        futprevstack <- rstFut 
       return(list(refstack, futprevstack))
      })
  # Getting the Refugia rasters
    ftr.stk <- map(1:length(vel), function(h) vel[[h]][[1]])
    names (ftr.stk) <- gcms
    ftr.stk <- rast(ftr.stk)
  # average among GCMs
    ftr.mean <- app(ftr.stk, fun=mean, na.rm=TRUE)
    ftr.mean <- ftr.mean * 100  ## multiply the values for 100 to reduce file size.
    names(ftr.mean) <- glue('y{yrs[i]}')
    #ext(ftr.mean) <- ext
   
  ## obtain mean for the reference stack
    # ref.stk <- map(1:length(vel), function(h) vel[[h]][[2]])
    # ref.stk <- rast(ref.stk)
    # ref.mean <- app(ref.stk, fun = mean, na.rm = TRUE)
    # Write these rasters
    out <- glue('./outputs/velocity/tree_spp3/west')
    ifelse(!file.exists(out), dir_create(out), print('Already exists'))
    terra::writeRaster(ftr.mean, glue('{out}/backwardVel_{sp}_refugia_{baseline}_{ssps[k]}_{yrs[i]}.tif'),
                       filetype = 'GTiff', datatype = 'INT4U',  overwrite = TRUE)
    
     cat('Finish!\n')  
  
  cat('Done ', flsFut[i], '\n')
  
cat('Finish!\n')  
  })
    })
}
   
  
# Apply the function velocity ---------------------------------------------
# Set up parallel processing (adjust the number of workers as needed)
future::plan(future::multisession, workers = 10)  # 
# Apply the function with parallelization for species 10 to 15
reclass <- furrr::future_map(.x = species, .f = ~ get_backward_velocity(sp = .x, baseline = '1991'))




