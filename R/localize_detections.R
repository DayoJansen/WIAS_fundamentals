## Script to localize processed detections

## Read in processed detections
multilateration_dets <- readRDS("./field_data/detections/group_tests_dets_multilateration.RDS")

## Empty data frame to fill up with localization estimates
localization_estimates <- data.frame()

## Set progress bar
pb <- txtProgressBar(min = 0, max = length(unique(paste(multilateration_dets$point,
                                                        multilateration_dets$tag))), 
                     style = 3)

## For each test point
for(point_f in unique(multilateration_dets$point)){
  
  ## Filter by point
  multilateration_dets_point <- multilateration_dets[multilateration_dets$point == point_f,]
  
  for(tag_f in unique(multilateration_dets_point$tag)){
    
    ## Progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, which(unique(paste(multilateration_dets$point,
                                             multilateration_dets$tag)) == paste(point_f, tag_f)))
    
    ## Filter by tag
    multilateration_dets_point_tag <- multilateration_dets_point[multilateration_dets_point$tag == tag_f,]
    
    ## Try multilateration procedure
    tryCatch(
      expr = {
        
        # Determine the node with the strongest avg.RSSI value to be used as starting values
        max_rssi <- multilateration_dets_point_tag[which.max(multilateration_dets_point_tag$mean_rssi),]
        
        # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on RSSI values (distance) and the pairwise distance between all nodes
        nls_mod <- suppressWarnings(nls(distance ~ geosphere::distm(data.frame(node_x, node_y),
                                                                    c(x_solution, y_solution),
                                                                    fun = distHaversine), 
                                        data = multilateration_dets_point_tag,
                                        start = list(x_solution = max_rssi$node_x,
                                                     y_solution = max_rssi$node_y),
                                        control = nls.control(warnOnly = T,
                                                              minFactor=1/30000)))
        
        
        ## Distance between estimated location and actual location
        est_error <- as.numeric(geosphere::distm(data.frame(multilateration_dets_point_tag$x[1], 
                                                            multilateration_dets_point_tag$y[1]),
                                                 c(coef(nls_mod)[1], 
                                                   coef(nls_mod)[2])))
        
        ## Get estimated values
        multilateration_est <- data.frame(point = point_f,
                                          tag = tag_f,
                                          description = multilateration_dets_point_tag$description[1],
                                          x = multilateration_dets_point_tag$x[1],
                                          y = multilateration_dets_point_tag$y[1],
                                          x_est = as.numeric(coef(nls_mod)[1]),
                                          y_est = as.numeric(coef(nls_mod)[2]),
                                          error = est_error)
        
        ## Combine data
        localization_estimates <- bind_rows(multilateration_est,
                                            localization_estimates)
        
        
      },
      error = function(e){ 
        cat("Too few detections")
      }
    ) 
    
  }
}

## End progress bar 
close(pb)

## How many localization estimates do you have?
nrow(localization_estimates)

## Save
readr::write_csv(localization_estimates, "./processed_data/detections/localization_estimates.csv")
