library(fpp2)        
library(zoo) 
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(date)
library(forecast)

# section a
PortlandWaterLevel2003$date <- as_date(as.character(PortlandWaterLevel2003$Date),format="%m/%d/%Y") 

WaterMA <- PortlandWaterLevel2003 %>%
  select(date, WLValue = WL) %>%
  mutate(WLValue_01 = ma(WLValue, order = 24, centre = TRUE),
         WLValue_02 = ma(WLValue, order = 168, centre = TRUE),
         WLValue_03 = ma(WLValue, order = 360, centre = TRUE),
         WLValue_04 = ma(WLValue, order = 720, centre = TRUE),
         WLValue_05 = ma(WLValue, order = 1080, centre = TRUE))

head(WaterMA,10)

tail(WaterMA,10)

WL1DayAvg <- WaterMA %>%
  group_by(date) %>%
  summarize(avg_by_date =  sum(WLValue_01)/length(WLValue_01)) %>%
  arrange((date))

head(WL1DayAvg)

WL1WeekAvg <- WaterMA %>%
  group_by(date) %>%
  summarize(avg_by_date =  sum(WLValue_02)/length(WLValue_02)) %>%
  arrange((date))

head(WL1WeekAvg)

WL15DayAVG <- WaterMA %>%
  group_by(date) %>%
  summarize(avg_by_date =  sum(WLValue_03)/length(WLValue_03)) %>%
  arrange((date))

head(WL15DayAVG,10)

timeplot <-ggplot() + 
  geom_line(na.omit(WL1DayAvg), mapping =aes(x=date, y=avg_by_date, color = "red"), show.legend=TRUE,size =0.8) +
  geom_line(na.omit(WL1WeekAvg), mapping =aes(x=date, y=avg_by_date, color = "blue"), show.legend=TRUE,size =1) +
  geom_line(na.omit(WL15DayAVG), mapping =aes(x=date, y=avg_by_date, color = "green"), show.legend=TRUE,size =1.3) +
  theme_bw() +
  scale_x_date(breaks = as.Date(c("2003-01-01", "2003-12-01")),
               date_breaks = "2 months",
                date_labels = "%m %Y") 



timeplot + labs(x = "Date (Month)",
                y = "WL (Average)",
                title = "Average WL vs. Date (Month)") +
  scale_color_manual(name = "Group",
                     values = c(green = "green", red = "red", blue = "blue"),
                     labels = c("7 Day Average", "15 Day Average", "1 Day Average"))


timeplotma <- ggplot() + 
  geom_line(na.omit(WaterMA), mapping =aes(x=date, y=WLValue_01, color = "red"), show.legend=TRUE,size =0.8) +
  geom_line(na.omit(WaterMA), mapping =aes(x=date, y=WLValue_02, color = "blue"), show.legend=TRUE,size =1) +
  geom_line(na.omit(WaterMA), mapping =aes(x=date, y=WLValue_03, color = "green"), show.legend=TRUE,size =1.3) +
  theme_bw() +
  scale_x_date(breaks = as.Date(c("2003-01-01", "2003-12-01")),
               date_breaks = "2 months",
               date_labels = "%m %Y") 

timeplotma + labs(x = "Date (Month)",
                y = "WL (Moving Average)",
                title = "Moving Average WL vs. Date") +
  scale_color_manual(name = "Group",
                     values = c(green = "green", red = "red", blue = "blue"),
                     labels = c("7 Day Average", "15 Day Average", "1 Day Average"))


# section b

PortlandWaterLevel <- PortlandWaterLevel2003 %>%
  mutate(dd = format(as_date(date),"%d"),
         hh= format(as.POSIXct(Time,format="%H:%M"),"%H"),
         wd = wday(date),
         wday = wday(date, label = TRUE, abbr = TRUE))


WL_dd_AVG <- PortlandWaterLevel %>%
  group_by(dd,hh) %>%
  summarize(avg_by_dd =  sum(WL)/length(WL)) %>%
  arrange(dd,hh)


levelplot(as.numeric(avg_by_dd) ~ as.numeric(dd)*as.numeric(hh), data=WL_dd_AVG  ,xlab="Date",
          ylab = "Hours",
          region = TRUE,
          main = "Date vs. Hour using Average WL")


