require(pacman)
pacman::p_load(ggplot2, glue, googledrive, ggspatial, qs, 
               RColorBrewer, terra, sf, tidyterra, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ----------------------------
path <- './outputs/velocity/tree_spp3/west'
files<- list.files(path, pattern = '.tif', full.names = TRUE)
sp_codes <- readr::read_csv('./inputs/treeSp_codes.csv')


# Extract the species codes
species <- unique(stringr::str_extract(files, "(?<=backwardVel_)[^_]+"))

# Extract  SSP 
ssps <- unique(stringr::str_extract(files, "ssp[0-9]+"))


yrs <- c('2040', '2070', '2100')

baseline <- c('1961','1991')

type <- 'backward'
#load the shapefile for north america
#limt <- terra::vect('./inputs/boundaries/AlaskaCA.shp')
limt <- terra::vect('./inputs/boundaries/north_america_mainland.shp')


makeMap <- function(sp, type, baseline ){
  #sp <- species[1]
  
  scientific_name <- if (!is.null(sp_codes)) {
    sp_codes %>% filter(spcs == sp) %>% pull(ScientificName) %>% first() %||% sp
  } else {
    sp
  }
  
  message(crayon::blue('Creating map for species', sp))
  fls <- grep(sp, files, value = TRUE)
  
  spMap <- map(.x = 1:length(ssps), function(k){
    message(crayon::blue('Applying to', ssps[k] ,'\n'))
    sp_fls <- grep(ssps[k], fls, value = TRUE)
    sp_fls <- grep(baseline, sp_fls, value = TRUE)
    
    #read rasters 
    rst <- terra::rast(sp_fls)
    names(rst) <- yrs
    
    # Get the extent of the raster
    rst_extent <- terra::ext(rst)
    
    # Create a formatted SSP string with the correct decimal point
    formatted_ssp <- str_replace(ssps[k], "(\\d)(\\d)(\\d)", "\\1-\\2.\\3")
    
    # Create the subtitle label with the baseline on a second line
    subtitle_label <- glue("{formatted_ssp}\nBaseline: {baseline}")
    
    # Crop the shapefile to the raster extent
    limt_crop <- crop(limt, ext(rst_extent))
    
    
    ##make ggplot
spMap<- ggplot() +
  geom_spatvector(data = limt_crop, fill = NA) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_gradientn(colours = c('transparent', brewer.pal(n = 5, name = 'BuPu')), 
                       na.value = "transparent",  # Make 0 values transparent
                       limits = c(min(rst[], na.rm = TRUE), max(rst[], na.rm = TRUE))) +
  theme_bw() + 
  theme(legend.position = 'bottom',legend.key.width = unit(2, 'line'),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = 'bold'),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10,face = 'bold')) +  
  labs(x = 'Longitude', y = 'Latitude', fill = 'Refugia',
       title =  bquote(.(str_to_title(type)) ~ "refugia index" ~ italic(.(scientific_name))),
       subtitle = subtitle_label) +
  coord_sf(
    xlim = c(rst_extent[1], rst_extent[2]), # xmin and xmax
    ylim = c(rst_extent[3], rst_extent[4]), # ymin and ymax
    expand = FALSE)  
out <- glue('./maps/trees3/west')
ifelse(!file.exists(out), dir_create(out), print('Already exists'))

ggsave(plot = spMap, filename = glue('{out}/{type}_refugia_{sp}_{ssps[k]}_{baseline}.png'), 
       units = 'in', width = 7, height = 4, dpi = 300)
 })
  message(crayon::green('Done!'))
}

# Apply the function ------------------------------------------------------
dfrm <- map(.x = species[1], type = 'forward', baseline = '1961', .f = makeMap)

# Upload to Drive----------------------------------------------------------
outputFolder <- './maps/trees3/west'
googleFolderID <- 'https://drive.google.com/drive/folders/1ZKGBWi4bhinJaEpuXQT-2rPk1_RexU2T' #Trees

fl <- list.files(outputFolder, full.names = TRUE)
lapply(X = fl, FUN = drive_upload, path = as_id(googleFolderID))

