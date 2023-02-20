## Filter test detections

library(tidyverse)
library(lubridate)

## Data
dets <- readRDS("./processed_data/detections/detections_raw.RDS")

## Tags
tags <- readxl::read_excel("./field_data/group_tests/group_tests_tags.xlsx") %>%
  pull(tag)

## Nodes
test_nodes <- readxl::read_xlsx("./field_data/grid_points/grid_points.xlsx")

## Filter dets based on tags in the calibration test and deployed nodes
dets_f <- dets %>% 
  filter(node %in% test_nodes$node) %>% 
  filter(tag %in% tags) 

## Read in calibration point data
field_log_intervals <- readxl::read_excel("./field_data/group_tests/group_tests_log.xlsx") %>% 
  transmute(point,
            description,
            time_start = lubridate::ymd_hm(paste(date, time_start), tz = "Europe/Amsterdam"),
            time_end = lubridate::ymd_hm(paste(date, time_end), tz = "Europe/Amsterdam"),
            interval = lubridate::interval(time_start, time_end)) %>% 
  na.omit()

## Intervals 
test_intervals <- as.list(field_log_intervals$interval)

## Filter to keep test detections
dets_f <- dets_f %>%
  dplyr::filter(date_time %within% test_intervals) %>%
  dplyr::mutate(date_time_2 = date_time) %>%
  dplyr::select(node, tag, rssi, date_time, date_time_2) %>%
  data.table::data.table()

## Set up field log intervals as data.table to join detections with corresponding test from the log
field_log_intervals_dt <- field_log_intervals %>%
  dplyr::select(point,
                description,
                date_time = time_start,
                date_time_2 = time_end) %>%
  data.table::data.table()
data.table::setkey(field_log_intervals_dt, date_time, date_time_2)

## Join based on overlap
field_cal_dets <- data.table::foverlaps(x = dets_f,
                                        y = field_log_intervals_dt,
                                        by.x = c("date_time", "date_time_2"),
                                        by.y = c("date_time", "date_time_2"),
                                        type = "within",
                                        mult = "all",
                                        nomatch = 0L) %>%
  
  ## Reformat
  dplyr::select(point,
                description,
                node,
                tag,
                rssi,
                date_time = i.date_time)


## Join grid point name from node file
field_cal_dets <- field_cal_dets %>% 
  left_join(test_nodes, by = "node") %>% 
  select(point, description, grid_point, tag, date_time, rssi)

## Summarize detections: Number of detections, mean RSS value, and sd RSSI value for each tag and each point
det_sum <- field_cal_dets %>% 
  group_by(point,description,tag,grid_point) %>% 
  summarise(num_detections = n(),
            mean_rssi = mean(rssi),
            sd_rssi = sd(rssi))

## Make a plot to visualize some of these patterns for each point. Do you see any patterns?

## Save detections
saveRDS(field_cal_dets, "./processed_data/detections/group_tests_data_filtered.RDS")
