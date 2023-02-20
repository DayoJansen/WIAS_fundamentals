## Model RSS to distance relationship 

## Housekeeping
library(tidyverse)
library(lubridate)

## Get range test detections #####
dets_f <- readRDS("./processed_data/detections/detections_raw.RDS")

## Range test log
range_log <- readxl::read_xlsx("./field_data/rss_distance_test/rssi_distance_calibration_log.xlsx") %>%
  dplyr::transmute(distance,
                   time_start = lubridate::ymd_hm(paste(date, time_start),
                                                  tz = "Europe/Amsterdam"),
                   time_end = lubridate::ymd_hm(paste(date, time_end),
                                                tz = "Europe/Amsterdam"),
                   interval = lubridate::interval(time_start, time_end)) %>% 
  na.omit()

## Test nodes and tags
range_test_nodes <- readxl::read_xlsx("./field_data/rss_distance_test/rssi_distance_calibration_nodes.xlsx") %>%
  pull(node)

## Tags
range_test_tags <- readxl::read_xlsx("./field_data/rss_distance_test/rssi_distance_calibration_tags.xlsx") %>%
  pull(tag)

## Get range test detections
detections <- readRDS("./processed_data/detections/detections_raw.RDS")

## Filter tags and nodes from detections
dets_f <- detections %>%
  filter(tag %in% range_test_tags) %>%
  filter(node %in% range_test_nodes)

## Intervals 
test_intervals <- as.list(range_log$interval)

## Filter to keep range test detections
dets_f <- dets_f %>%
  dplyr::filter(date_time %within% test_intervals) %>%
  dplyr::mutate(date_time_2 = date_time) %>%
  dplyr::select(node, tag, rssi, date_time, date_time_2) %>%
  data.table::data.table()

## Set up field log intervals as data.table to join
range_log_dt <- range_log %>%
  dplyr::select(distance,
                date_time = time_start,
                date_time_2 = time_end) %>%
  data.table::data.table()
data.table::setkey(range_log_dt, date_time, date_time_2)


## Join based on overlap
range_test_dets_j <- data.table::foverlaps(x = dets_f,
                                           y = range_log_dt,
                                           by.x = c("date_time", "date_time_2"),
                                           by.y = c("date_time", "date_time_2"),
                                           type = "within",
                                           mult = "all",
                                           nomatch = 0L) %>%
  
  ## Reformat
  dplyr::select(distance,
                node,
                tag,
                rssi,
                date_time = i.date_time) %>%
  dplyr::mutate(distance = as.numeric(distance))

## Plot the raw data. What do you notice?
ggplot(range_test_dets_j) +
  geom_point(aes(x = distance,
                 y = rssi))

## Model
log_dist_RSSI_mdl <- lm(log10(distance) ~ rssi, 
                        data = range_test_dets_j)

## Predict distances with CIs from our data
preds <- 10^predict(log_dist_RSSI_mdl, interval = "confidence")

## Add to data
model_data <- cbind(range_test_dets_j, preds)

## Plot predicted values
ggplot(model_data,
       aes(x = fit,
           y = rssi)) +
  
  ## Actual data
  geom_point(aes(x = distance,
                 y = rssi),
             color = "red") +
  
  ## Modeled data
  geom_point() +
  geom_ribbon(aes(xmin = lwr, xmax = upr, color = NULL), alpha = .15) +
  theme_minimal() +
  labs(x = "Distance", y = "RSS")

## Save model
saveRDS(log_dist_RSSI_mdl, "./processed_data/RSS_distance_model.RDS")
