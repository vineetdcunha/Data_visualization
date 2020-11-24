library(magrittr)
library(scales)
library(dplyr)
library(tidyr)

# Section a)

AirQuality %>%
  mutate(Wind = rescale(Wind, to = c(0, 1))) %>%
  mutate(Solar = rescale(Solar.R, to = c(0, 1))) %>%
  ggplot(aes(Solar, Wind)) +
  geom_point() +  geom_smooth(method = lm) + theme_bw() +
  labs(x = "Solar",
       y = "Wind",
       title = "Solar vs. Wind")

# Section b)
# Pivot Table
AirQuality_pivot <-
  AirQuality %>% select(Ozone, Solar.R, Wind, Temp, Day, Month) %>% gather(key, value,-Day,-Month)

filter_key <- c("Solar.R", "Wind")

AirQuality_pivot %>% filter(key %in% filter_key) %>%
  ggplot(aes(key, value)) +
  geom_beeswarm() +
  theme_bw() +
  labs(x = "Measurement",
       y = "Value",
       title = "Comparing distributions")

library(cowplot)

# Section d)

qqSolar <- ggplot(AirQuality, aes(sample = Solar.R)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw() +
  labs(title = "QQ plot for Solar")


qqWind <- ggplot(AirQuality, aes(sample = Wind)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw() +
  labs(title = "QQ plot for Wind")


plot_grid(qqSolar, qqWind, labels = "AUTO")

# Section c)

AirQuality_pivot %>%
  ggplot(aes(key, value)) +
  geom_beeswarm() +
  theme_bw() +
  labs(x = "Measurement",
       y = "Value",
       title = "Comparing distributions")

AirQuality_pivot %>%
  ggplot(aes(key, value)) +
  geom_violin() +
  geom_sina() +
  theme_bw() +
  labs(x = "Measurement",
       y = "Value",
       title = "Comparing distributions")
