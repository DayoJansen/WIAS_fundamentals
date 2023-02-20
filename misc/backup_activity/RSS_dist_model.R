## Model dist relative to signal strength #####
dets <- readr::read_csv("./misc/backup_activity/rssi_dist_detections.csv")


## Plot raw data
ggplot(dets,
       aes(x = distance,
           y = rssi)) +
  theme_minimal() +
  geom_point() +
  stat_smooth() +
  facet_grid(tag~node) +
  scale_x_continuous(breaks = unique(dets$distance))

## What do you notice about the raw data?


## Plot
ggplot(dets,
       aes(x = log10(distance),
           y = rssi)) +
  theme_minimal() +
  stat_smooth() +
  scale_x_continuous(breaks = unique(dets$distance))

## Model
log_dist_RSSI_mdl <- lm(log10(distance) ~ rssi, 
                        data = dets)


## Predict distances with CIs from our data
preds <- 10^predict(log_dist_RSSI_mdl, interval = "confidence")

## Add to data
model_data <- cbind(dets, preds)

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

## What do you notice about the modeled data?
