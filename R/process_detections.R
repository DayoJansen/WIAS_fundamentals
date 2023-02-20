## Process filtered detections to be used for multilateration
library(tidyverse)
library(lubridate)

## Data
filtered_ddets <- readRDS("./processed_data/detections/group_tests_data_filtered.RDS")

## Process filtered detections
prepared_dets <- filtered_ddets %>%
  dplyr::group_by(point,
                  description,
                  tag,
                  grid_point) %>% 
  dplyr::summarise(mean_rssi = mean(rssi)) %>% 
  dplyr::group_by(point,
                  description,
                  tag) %>% 
  dplyr::mutate(number_grid_point = n())

## Add point coordinates from GPS file. (This is your group's own file with your points)
point_coords <- sf::st_read("./field_data/group_tests/gps_points.GPX")  %>% 
  # sf::st_transform(28992) %>% 
  dplyr::transmute(point = name,
                   test_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                   test_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
  sf::st_drop_geometry()


## Add node coordinates
grid_point_coords <- st_read("./data/field_data/grid_points/grid_points.GPX") %>% 
  # sf::st_transform(28992) %>% 
  dplyr::transmute(grid_point = name,
                   grid_point_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                   grid_point_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
  sf::st_drop_geometry()

## Add node coordinates
grid_points <- readxl::read_excel("./field_data/group_tests/group_tests_nodes.xlsx")


## Add node and test coordinates
multilateration_dets <- prepared_dets %>% 
  
  ## Keep only tests with detections by at least 3 unique grid points
  dplyr::filter(number_grid_point >= 3) %>%
  dplyr::ungroup() %>% 
  
  ## Join node pts
  dplyr::left_join(point_coords,
                   by = "point") %>% 
  ## Join model interval
  dplyr::left_join(grid_point_coords,
                   by = "grid_point")

## Read in the RSS distance model
log_dist_RSSI_mdl <- readRDS("./processed_data/RSS_distance_model.RDS")

## Use the RSS ~ distance model to predict distance
multilateration_dets$distance <- 10^predict(log_dist_RSSI_mdl,
                                            newdata = data.frame(rssi = multilateration_dets$mean_rssi))

## Check out the values of distance
hist(multilateration_dets$distance, breaks = 20)

## Save detections that are now formatted for localization using multilateration
saveRDS(multilateration_dets, "./field_data/detections/group_tests_dets_multilateration.RDS")