#Problem 4

library(lubridate)
library(date)

PortlandWaterLevel <- PortlandWaterLevel2003 %>%
  mutate(
    dd = format(as_date(date), "%d"),
    hh = format(as.POSIXct(Time, format = "%H:%M"), "%H"),
    wd = wday(date),
    wday = wday(date, label = TRUE, abbr = TRUE)
  )


WL_dd_AVG <- PortlandWaterLevel %>%
  group_by(dd, hh) %>%
  summarize(avg_by_dd =  sum(WL) / length(WL)) %>%
  arrange(dd, hh)


heatmap <-
  ggplot(WL_dd_AVG, aes(as.numeric(dd), as.numeric(hh), fill = avg_by_dd)) + geom_tile() +  scale_fill_gradient2(
    low = hsv(0.7,0.6,0.7),
    mid = "white",
    high = hsv(1,0.6,0.6),
    midpoint = 1.2,
    space = "Lab"
  ) + scale_x_continuous(breaks = seq(01, 31, 2)) + scale_y_continuous(breaks = seq(00, 23, 2)) + theme_bw() +
  labs(x = "Date",
       y = "Hour",
       title = "Date vs. Hour using Average WL",
       fill = 'Average WL')

heatmap
