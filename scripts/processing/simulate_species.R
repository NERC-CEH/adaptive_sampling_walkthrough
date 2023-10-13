

## simulating species

library(terra)
library(sf)

list.files('data',
           full.names = TRUE)

butterflyeff <- rast('data/butterfly_1km_effort_layer.grd')
plot(log(butterflyeff))

envdat <- rast("data/envdata_1km_no_corr_noNA.grd")
plot(envdat[[1]])
envdat
names(envdat)

uk <- st_read('data/uk_counties2023_ONS/CTY_MAY_2023_EN_BFC.shp')
uk
lancs <- st_geometry(uk[uk$CTY23NM == 'Lancashire',])
plot(lancs)

# crop layers
env_lancs <- terra::crop(envdat, vect(lancs), mask = TRUE)
plot(env_lancs)

buttlancs <- terra::crop(butterflyeff, vect(lancs), mask = TRUE)
plot(log(buttlancs))

beff <- as.data.frame(butterflyeff, xy = TRUE)
head(beff)

row_ind <- sample(1:nrow(beff), size = 1000, prob = beff$butterfly_1km_effort)

samples <- beff[row_ind,]

recsuk <- st_as_sf(samples, coords = c("x", "y"), crs = 27700)
plot(log(butterflyeff))
plot(recsuk, add = TRUE, pch = 20)

records_lancs <- mask(recsuk, vect(lancs))

plot(log(buttlancs))
plot(recsuk, add = TRUE, pch = 20)

