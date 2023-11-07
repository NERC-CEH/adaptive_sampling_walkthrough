

## empirical criterion generation - to get absences

find_empirical_absences <- function(recordsdf, templaterast) {
  
  # convert species records to raster
  locations_raster <- rasterize(x = as.matrix(recordsdf), y = templaterast, 
                                values = 1, background = 0)
  locations_raster_masked <- mask(species_rast, templaterast) # mask to GB
  
  # convert to data frame
  raster_df <- as.data.frame(locations_raster_masked, xy = TRUE)
  colnames(raster_df) <- c('x', 'y', 'observations')
  
  # Unvisited locations are any 0s!
  empirical_criterion_absences <- raster_df[raster_df$observations == 0,]
  
  return(empirical_criterion_absences)
  
}


## get Pseudoabsences for model

get_pseudos <- function(n_pseudos, absence_dataframe) {
  
  # randomly sample
  psedoabs_index <- sample(1:nrow(absence_dataframe), 
                           size = n_pseudos)
  
  # extract pseudoabsences
  pseudoabs <- absence_dataframe[psedoabs_index,]
  
  return(pseudoabs)
  
}


## optimise sampling locations

optimise_sampling_uncertainty <- function(uncertainty_dataframe, 
                                          number_locactions, 
                                          distance_apart = dist_apart){
  
  # order the uncertainty layer according to decreasing uncertainty - se.fit
  uncertaintytoedit <- uncertainty_dataframe[order(uncertainty_dataframe$se.fit,
                                                decreasing = TRUE), ]
  
  uncertainty_outdf <- data.frame() # data frame for storage
  
  i <- 1
  while(i<=number_locactions) {
    
    # if(i%%round(number_locactions/5)==0) print(i)
    
    # get the highest uncertainty point
    highest_uncert <- uncertaintytoedit[1,]
    # print(highest_uncert)
    
    # store that point
    uncertainty_outdf <- rbind(uncertainty_outdf, highest_uncert)
    
    # remove the highest point from the data frame
    uncertaintytoedit <- uncertaintytoedit[-1,]
    
    # find all points within x distance of highest uncert
    uncert_points_nearby <- unlist(st_is_within_distance(highest_uncert, uncertaintytoedit,
                                                         dist = distance_apart))
    
    if(length(uncert_points_nearby)>0){
      # remove those points from the uncertainty dataframe 
      uncertaintytoedit <- uncertaintytoedit[-uncert_points_nearby,]
    }
    
    i <- i+1
  }
  
  return(uncertainty_outdf)
}



## check whether they find new records
check_distrib <- function(new_records, true_species_distrib) {
  
  new_locs <- terra::extract(true_species_distrib, new_records, xy = TRUE)
  
  return(new_locs[new_locs$last == 1,])
  
}


## modelling - gams
model_predict <- function(datato_model, environ_data) {
  
  print("!! modelling")
  
  # model - simple model with few explanatory variables
  gam_mod <- gam(observations ~ s(impr_grass) +
                   s(elev) + 
                   s(bio_1),
                 family = 'binomial',
                 data = datato_model)
  
  print(summary(gam_mod))
  
  # convert environmental raster to data frame
  environ_data_df <- as.data.frame(environ_data[[c("impr_grass", "elev", "bio_1")]],
                                   na.rm = TRUE,
                                   xy = TRUE)
  
  # create a new x variable
  newx <- environ_data_df[, c("x", "y", "impr_grass", "elev", "bio_1")]
  
  print("!! predicting")
  
  # predict onto environment
  newy <- predict(gam_mod, 
                  newx,
                  type = "response",
                  se.fit = TRUE)
  
  modelbased_criterion_out <- na.omit(cbind(newx, newy))
  
  return(modelbased_criterion_out)
}


