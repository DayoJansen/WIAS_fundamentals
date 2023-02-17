## Script to aggregate all detections

## Houskeeping ########
packages_needed <- c("tidyverse","vroom","lubridate","sf", "data.table", "readxl", "geosphere") 
new_packages <- packages_needed[!(packages_needed %in% 
                                    installed.packages()[,"Package"])]

if(length(new_packages)){
  install.packages(new_packages, repos = "https://cloud.r-project.org")
} 

# Now load all packages
library(tidyverse); library(lubridate); library(vroom); library(data.table); library(readxl); library(geosphere)

## Detections from Sensor Station
dets_ss_raw <- vroom::vroom(list.files(here::here(),
                                   recursive = TRUE,
                                   full.names = T,
                                   pattern = "raw"))

## Check out the structure of the data
str(dets_ss_raw)
head(dets_ss_raw)


## Reformat
dets_ss <-  dets_ss_raw %>%
  transmute(node = toupper(NodeId),
            date_time = lubridate::force_tz(Time, tz = "Europe/Amsterdam"),
            tag = TagId,
            rssi = TagRSSI)


## Filter: Keeping only recent detections
dets_f <- dets_ss %>% 
  dplyr::filter(date_time > lubridate::ymd_hm("2023-02-17 09:00")
                !is.na(node),
                !is.na(tag)) %>% 
  dplyr::distinct(node,
                  tag,
                  date_time,
                  .keep_all = T)

## Save
saveRDS(dets_f,
        "./processed_data/detections/detections_raw.RDS")


