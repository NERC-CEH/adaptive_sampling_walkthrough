
## simulating species

library(terra)
library(sf)
library(dismo)
library(rnaturalearth)

# function to scale values between 0 and maxval
scalevals <- function(x, maxval=100) {
  if(class(x) == "SpatRaster"){
    nx <- c(minmax(x))
  } else {
    nx <- range(x)
  }
  (x - nx[1]) / (nx[2] - nx[1])*maxval
}



## prepare environmental data
# Run once, takes a while!
{
  #### create environmental raster
  # get some habitat data - landcover map 2015
  lcm2015 <- rast('data/lcm2015_1km_raster/data/LCM2015_GB_1km_percent_cover_target_class.tif')
  
  # create mnask of GB by summing all the layers together
  gbmask <- sum(lcm2015)
  
  # mask to convert 0s to NAs
  lcmmasked <- as.numeric(mask(lcm2015, gbmask, maskvalues = 0))
  
  # rename
  names(lcmmasked) <- c('broad_wood', 'conif_wood', 'arable', 'impr_grass', 'neutr_grass', 'calc_grass', 'acid_grass',
                        'fen_marsh_swamp', 'heather', 'heather_grass', 'bog', 'inland_rock', 'saltwater', 'freshwater',
                        'sup_lit_rock', 'sup_lit_sed', 'lit_rock', 'lit_sed', 'saltmarsh', 'urban', 'suburban')
  # plot(lcmmasked)
  
  
  # elevation - bioclim elevation wc2.1 30s
  elev <- rast('data/wc2.1_30s_elev/wc2.1_30s_elev.tif')
  elev_uk <- project(terra::crop(elev, y = ext(-8, 2.5, 49.5, 61)), y = "epsg:27700")
  names(elev_uk) <- gsub('wc2.1_30s_', replacement = '', names(elev_uk))
  
  # plot(elev_uk)
  
  # get some climate data - bioclim 2.1 30s
  bioclimvar <- do.call(c, lapply(list.files('data/wc2.1_30s_bio/', full.names = TRUE),
                                  rast))
  bio_uk <- project(terra::crop(bioclimvar, y = ext(-8, 2.5, 49.5, 61)), y = "epsg:27700")
  names(bio_uk) <- gsub('wc2.1_30s_', replacement = '', names(bio_uk))
  
  # plot(bio_uk)
  
  # combine elevatiom and bioclim
  clim_elev <- c(elev_uk, bio_uk)
  
  # project climate and elevation data to land cover
  climelev_proj <- terra::project(clim_elev, lcmmasked,
                                  method = 'bilinear')
  # plot(climelev_proj[[1]])
  
  # mask to GB
  climelev_proj <- mask(climelev_proj, gbmask, maskvalues = 0, updatevalue = NA)
  # plot(climelev_proj[[1]], colNA="blue")
  
  # combine
  envdat <- c(lcmmasked, climelev_proj)
  plot(envdat)
  
  terra::writeRaster(envdat, file = 'data/environmental_data.tif',
                     overwrite = TRUE)
  
  # select subset of variables
  envdat <- envdat[[c("impr_grass", "heather", "urban", "suburban", "elev", 
                      "bio_1", "bio_2", "bio_12", "bio_15")]]
  
  terra::writeRaster(envdat, file = 'data/environmental_data_subset.tif',
                     overwrite = TRUE)
}

# read environmental data
envdat <- terra::rast('data/environmental_data_subset.tif')
# plot(envdat)

##### simulate true distribution of a single species
# combination of precipitation seasonailty, elevation and broadleaf woodland
species_layer <- scalevals(envdat[["bio_1"]])* # Annual mean temp
  scalevals(envdat[["bio_15"]])+ # PrecipSeasonality
  scalevals(envdat[["elev"]])+
  scalevals(envdat[["impr_grass"]]) # improved grassland
names(species_layer) <- 'species_probability'
plot(species_layer)

## convert to presence/absence
# scale betweeen 0-100
scaled_distrib_rast <- scalevals(species_layer)
prob_pres_spp <- as.data.frame(scaled_distrib_rast, xy = TRUE)
head(prob_pres_spp)

# look at quantiles
quantile(prob_pres_spp$species_probability, seq(0, 1, 0.10))

# convert everything > 90% to presence/absence
prob_pres_spp$species_presence <- ifelse(prob_pres_spp$species_probability >= 
                                           quantile(prob_pres_spp$species_probability, seq(0, 1, 0.10))[9],
                                         1, 0)
true_species_locs <- prob_pres_spp[prob_pres_spp$species_presence==1,]

# plot
plot(scaled_distrib_rast)
points(x=true_species_locs$x, y=true_species_locs$y, pch = 19, cex = 0.1)

# save true species locations
true_distrib <- rasterize(as.matrix(true_species_locs[,1:2]), envdat[["bio_1"]], background = 0)
true_distrib <- terra::mask(true_distrib, envdat[["bio_1"]])
plot(true_distrib)


#### simulate biassed sampling of the species
# sample according to suburban layer - biassed sampling
# areas with high suburban values will be more likely to be sampled

# convert to sf object for extract 
true_species_dist <- st_as_sf(true_species_locs, coords = c("x", "y"), crs = 27700)
plot(st_geometry(true_species_dist), pch = 4, cex = 0.2, add = TRUE) ## species distribution

# extract values of urban and suburban
true_species_urban <- terra::extract(envdat[['suburban']]+envdat[['urban']], 
                                     true_species_dist)

# get biassed sampling of true species distribution
latitude_bias <- scalevals(true_species_locs$species_probability/
                             true_species_locs$y)
urban_bias <- scalevals(true_species_urban[,2]^2)

# sample
samp_spp_ind <- sample(1:nrow(true_species_locs), size = nrow(true_species_locs)*0.1, # sample 10% of the distribution
                       prob = latitude_bias * urban_bias)

sampled_species_distrib <- true_species_locs[samp_spp_ind, c('x', 'y')]

plot(scaled_distrib_rast)
points(x = sampled_species_distrib$x, y = sampled_species_distrib$y, 
       pch = 4, col = "blue")

#### write out data
# true species distribution raster
writeRaster(true_distrib, 
            filename = 'outputs/simulated_data/true_species_distribution.tif',
            overwrite = TRUE)

# true species distribution csv
write.csv(true_species_locs, 
          file = 'outputs/simulated_data/true_species_distribution.csv')

# sampled species distribution
write.csv(sampled_species_distrib, 
          file = 'outputs/simulated_data/sampled_species_distrib.csv')
