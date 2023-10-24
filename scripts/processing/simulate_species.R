
## simulating species

library(terra)
library(sf)
library(dismo)


## prepare environmental data
# Run once, takes a while!
{
  # function to sample a raster
  sample_rast <- function(samplerast, n_vals) {
    
    # function to convert between 0-1
    scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
    
    # convert to raster
    sample_dat <- as.data.frame(samplerast, xy = TRUE)
    
    # get an index for sampling - probability final column
    row_ind <- sample(1:nrow(sample_dat), size = n_vals, prob = scale_values(sample_dat[,3]))
    
    # return sampled rows
    samples <- sample_dat[row_ind,]
    
    return(samples)
    
  }
  
  #### create environmental raster
  # uk outline
  countries <- ne_download(scale = 'large', type = "countries", returnclass = 'sf')
  uk <- st_transform(st_geometry(countries[countries$ADMIN == 'United Kingdom',]), crs = 27700)
  # plot(st_geometry(uk))
  
  # elevation - bioclim elevation wc2.1 30s
  elev <- rast('data/wc2.1_30s_elev/wc2.1_30s_elev.tif')
  elev_uk <- project(terra::crop(elev, y = ext(-8, 2.5, 49.5, 61)), y = "epsg:27700")
  elevcrp <- mask(elev_uk, vect(uk))
  names(elevcrp) <- gsub('wc2.1_30s_', replacement = '', names(elevcrp))
  
  # plot(elevcrp)
  
  # get some climate data - bioclim 2.1 30s
  bioclim <- do.call(c, lapply(list.files('data/wc2.1_30s_bio/', full.names = TRUE),
                               rast))
  bio_uk <- project(terra::crop(bioclim, y = ext(-8, 2.5, 49.5, 61)), y = "epsg:27700")
  biocrp <- mask(bio_uk, vect(uk))
  names(biocrp) <- gsub('wc2.1_30s_', replacement = '', names(biocrp))
  
  # plot(biocrp)
  
  # combine elevatiom and bioclim
  clim_elev <- c(elevcrp, biocrp)
  
  # get some habitat data - landcover map 2015
  lcm2015 <- rast('data/lcm2015_1km_raster/data/LCM2015_GB_1km_percent_cover_target_class.tif')
  lcmmask <- mask(lcm2015, vect(uk))
  names(lcmmask) <- c('broad_wood', 'conif_wood', 'arable', 'impr_grass', 'neutr_grass', 'calc_grass', 'acid_grass',
                      'fen_marsh_swamp', 'heather', 'heather_grass', 'bog', 'inland_rock', 'saltwater', 'freshwater',
                      'sup_lit_rock', 'sup_lit_sed', 'lit_rock', 'lit_sed', 'saltmarsh', 'urban', 'suburban')
  # plot(lcmmask)
  
  # project climate and elevation data to land cover
  climelev_proj <- terra::project(clim_elev, lcmmask,
                                  method = 'bilinear')
  # plot(climelev_proj)
  
  # combine
  envdat <- c(lcmmask, climelev_proj)
  plot(envdat)
  
  terra::writeRaster(envdat, file = 'data/environmental_data.tif')
}

# read environmental data
envdat <- terra::rast('data/environmental_data.tif')

##### simulate true distribution of a single species
# combination of precipitation seasonailty, elevation and broadleaf woodland
species_layer <- envdat[["bio_15"]]* # PrecipSeasonality
  envdat[["bio_2"]]* # MeanDiRange
  envdat[["broad_wood"]]
plot(species_layer)

# sample n locs to get true species' distribution
true_species_locs <- sample_rast(species_layer, n_vals = 1000)[,c('x','y')]

# convert to sf object
true_species_dist <- st_as_sf(true_species_locs, coords = c("x", "y"), crs = 27700)
plot(st_geometry(true_species_dist), pch = 4, add = TRUE) ## species distribution


#### simulate biassed sampling of the species
# sample according to suburban layer - biassed sampling
# areas with high suburban values will be more likely to be sampled
true_species_suburban <- terra::extract(envdat[['suburban']], true_species_dist)

# get biassed sampling of true species distribution
samp_spp_ind <- sample(1:nrow(true_species_locs), size = 350, prob = true_species_suburban[,2])
sampled_species_distrib <- true_species_locs[samp_spp_ind, c('x', 'y')]

points(x = sampled_species_distrib$x, y = sampled_species_distrib$y, 
       pch = 4, col = "red")

head(sampled_species_distrib)

#### write out data
# true species distribution
write.csv(true_species_locs, 
          file = 'outputs/simulated_data/true_species_distribution.csv')

# sampled species distribution
write.csv(sampled_species_distrib, 
          file = 'outputs/simulated_data/sampled_species_distrib.csv')
