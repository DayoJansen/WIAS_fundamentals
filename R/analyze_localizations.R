## Analyze localizations

## Read in localiation estimates
localization_estimates <- readr::read_csv("./processed_data/detections/localization_estimates.csv")

## Project coordinates from lon/lat
localization_estimates <- localization_estimates %>% 
  st_as_sf(coords = c("x_est","y_est"),
           crs = 4326) %>% 
  st_transform(28992)


## Add point coordinates from GPS file. (This is your group's own file with your points)
point_coords <- sf::st_read("./field_data/group_tests/gps_points.GPX")  %>% 
  sf::st_transform(28992) %>%
  dplyr::transmute(point = name,
                   x_test = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                   y_test = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
  sf::st_drop_geometry()


## Join the predicted and true coordinates
localization_estimates <- localization_estimates %>% 
  left_join(point_coords,
            by = "point")

## Add node coordinates
grid_point_coords <- st_read("./data/field_data/grid_points/grid_points.GPX") %>% 
  sf::st_transform(28992) %>%
  dplyr::transmute(grid_point = name,
                   grid_point_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                   grid_point_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
  sf::st_drop_geometry()


## Plot error for each test point
ggplot(localization_estimates) +
  geom_point(aes(x = description,
                 y = error)) +
  theme(axis.text.x = element_text(angle = 45))

## Map of predicted versus true locations
ggplot(localization_estimates) +
  
  ## These are the true locations of each test point
  geom_point(aes(x = x,
                 y = y),
             color = "gold",
             data = point_coords) +
  
  ## These are the predicted locations
  geom_point(aes(x = x_est,
                 y = y_est),
             color = "red") +
  
  ## Connect the points
  geom_segment(aes(x = x_est,
                   y = y_est,
                   xend = x_test,
                   yend = y_test)) +
  
  ## Add the nodes for reference
  geom_point(aes(x = x_est,
                 y = y_est),
             color = black,
             shape = 8,
             data = grid_point_coords)

## What patterns do you see? How did your different tests affect the error?
## - What 'features' of the different tests (number of detections, number of nodes, average RSS etc)
## relate to the different errors? For example, is the error bigger when the RSS is lower?


## Using some of the tools from yesterday, you can try: 
## - Adding satellite imagery with ggmap
## - Making an animation with moveVis
## - Calculating speed/distance between points




