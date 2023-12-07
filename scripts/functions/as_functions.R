

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
  
  new_locs <- terra::extract(true_species_distrib, new_records, xy = TRUE)[,c("x", "y", "last")]
  colnames(new_locs) <- c("x", "y", "observations")
  
  return(new_locs)
  
}


## modelling - gams
model_predict <- function(datato_model, environ_data, printsum = FALSE) {
  
  print("!! modelling")
  
  # model - simple model with few explanatory variables
  gam_mod <- gam(observations ~ s(impr_grass) +
                   # s(elev) + 
                   s(bio_1) +
                   s(bio_12) +
                   s(x,y),
                 family = 'binomial',
                 data = datato_model)
  
  if(printsum) print(summary(gam_mod))
  
  # convert environmental raster to data frame
  environ_data_df <- as.data.frame(environ_data[[c("impr_grass", "elev", "bio_1", "bio_12")]],
                                   na.rm = TRUE,
                                   xy = TRUE)
  
  # create a new x variable
  newx <- environ_data_df[, c("x", "y", "impr_grass", "elev", "bio_1", "bio_12")]
  
  print("!! predicting")
  
  # predict onto environment
  newy <- predict(gam_mod, 
                  newx,
                  type = "response",
                  se.fit = TRUE)
  
  modelbased_criterion_out <- na.omit(cbind(newx, newy))
  
  return(modelbased_criterion_out)
}



# Function to run rounds of adaptive sampling
adaptive_sampling_rounds <- function(modelbased_crit_sf,
                                     initial_species_records,
                                     distance_apart,
                                     nrounds,
                                     total_number_locs,
                                     true_spp_distrib) {
  
  # convert initial points to sf object
  initial_species_records_sf <- st_as_sf(initial_species_records[,c("x", "y")], 
                                         coords = c("x", "y"), 
                                         crs = 27700) 
  
  # output data frames
  new_locs_out <- data.frame() # includes new locs where species wasn't
  new_species_observations <- data.frame() # only locations where species was seen
  modeluncertainty_out <- data.frame() # storing model uncertainty
  
  # print statement
  print(paste("Sampling", round(total_number_locs/nrounds), "per round. Number of rounds =", nrounds))
  
  ## start of for loop
  for(round_num in 1:nrounds) {
    
    print(paste("Round", round_num))
    
    #### remove sampling occasions close to existing records (from adaptive sampling)
    # ideally would include the old locations too but would take AGES to run.
    if(round_num>1){
      print(paste("Removing potential sampling occasions within", distance_apart, "m of previous adaptively sampled locations"))
      
      # convert new records to sf
      new_species_observations_sf <- st_as_sf(new_locs_out[,c("x", "y")], # use dataframe with all selected locations in
                                              coords = c("x", "y"), 
                                              crs = 27700)
      
      # find all points that are in the same location - that intersect
      points_intersect <- unlist(st_intersects(new_species_observations_sf, 
                                               modelbased_crit_sf,
                                               dist = distance_apart))
      
      # find all points within x distance of highest uncert
      points_within_dist <- unlist(st_is_within_distance(new_species_observations_sf, 
                                                         modelbased_crit_sf,
                                                         dist = distance_apart))
      
      if(length(c(points_within_dist, points_intersect))>0){
        # remove those points from the uncertainty dataframe 
        modelbased_crit_sf <- modelbased_crit_sf[-c(points_within_dist, points_intersect),]
      }
      
      # # bind to initial points
      # initial_species_records_sf <- rbind(initial_species_records_sf, new_species_observations_sf)
    }
    
    
    #### Get new sampling occasions
    print(paste("Getting sampling occasions"))
    {
      
      ## sample locations optimally from model uncertainty
      as_round <- optimise_sampling_uncertainty(uncertainty_dataframe = modelbased_crit_sf,
                                             number_locactions = round(total_number_locs/nrounds))
      
      ## check whether they find new records - Go out and sample observations
      new_locs <- check_distrib(new_records = terra::vect(as_round), 
                                true_species_distrib = true_spp_distrib)
      
      ## store new species observations
      new_locs$round <- round_num
      
      # store new locs including locations where species wasn't
      new_locs_out <- rbind(new_locs_out, 
                            new_locs)
      
      # # store new sampling occasions of only locs where species was seen
      # new_species_observations <- rbind(new_species_observations, 
      #                                   new_locs[new_locs$observations == 1,])
    }
    
    #### prepare for modelling
    print(paste("Preparing for modelling"))
    {
      ## bind to initial records
      all_records <- rbind(initial_species_records[,1:2], 
                           new_locs_out[new_locs_out$observations==1, c("x", "y")]) ## should we add round to this so that we can account for it in the modelling?
      
      ## remove duplicates for modelling
      all_records <- all_records[!duplicated(all_records),]
      
      ## add observations column
      all_records$observations <- 1
      
      ## generate pseudoabsences from unvisited locations
      absence_cells <- find_empirical_absences(all_records[,1:2], envdat[[1]])
      
      ## bind presences and pseudos together
      modelling_df <- rbind(all_records, absence_cells)
      
      ## extract environmental data
      modelling_df <- cbind(modelling_df, terra::extract(envdat, modelling_df[,1:2], 
                                                         xy = FALSE, method = "simple"))
    }
    
    ## modelling - gams
    modelpredictions <- model_predict(datato_model = modelling_df, environ_data = envdat)
    
    #### extract uncertainty
    print(paste("Getting uncertainty"))
    {
      # get standard error
      modeluncertainty <- modelpredictions[,c("x", "y", "se.fit")]
      
      # add round number
      modeluncertainty$round <- round_num
      
      # store
      modeluncertainty_out <- rbind(modeluncertainty_out, modeluncertainty)
      
      # convert to sf
      modelbased_crit_sf <- st_as_sf(modeluncertainty, coords = c("x", "y"), crs = 27700)
      
      # order the uncertainty layer according to decreasing uncertainty - se.fit
      # this is the layer that is updated for the next round of sampling
      modelbased_crit_sf <- modelbased_crit_sf[order(modelbased_crit_sf$se.fit,
                                                     decreasing = TRUE), ]
    }
    
  }
  
  return(list(all_adaptive_locations = new_locs_out,
              model_uncertainty = modeluncertainty_out))
  
}
