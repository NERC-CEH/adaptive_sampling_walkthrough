
## simulating species

library(terra)
library(sf)

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

# load environmental data
envdat <- rast("data/envdata_1km_no_corr_noNA.grd")
plot(envdat[[1]])
names(envdat)


##### simulate true distribution of a single species
# combination of precipitation seasonailty, elevation and broadleaf woodland
species_layer <- envdat[["PrecipSeasonality"]]*envdat[["elevation_UK"]]*envdat[["broad_wood"]]
plot(species_layer)

# sample n locs to get true species' distribution
true_species_locs <- sample_rast(species_layer, n_vals = 1000)[,c('x','y')]

# convert to sf object
true_species_dist <- st_as_sf(true_species_locs, coords = c("x", "y"), crs = 27700)
plot(st_geometry(true_species_dist), pch = 4, add = TRUE) ## species distribution


#### simulate biassed sampling of the species
# sample according to suburban layer - biassed sampling
true_species_suburban <- extract(envdat[['suburban']], true_species_dist)

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
