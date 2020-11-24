library(ggplot2)

library(ggforce)

# Problem 3

# section a)

popplot <- MontanaPopulationData %>%
  ggplot(aes(x = Year, y = Population))

popplot  + geom_line(size = 1.3,col='red') + 
  geom_point(size = 2.2) + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(125000 , max(MontanaPopulationData$Population) , by = 75000)) +
  scale_x_continuous(breaks = seq(1880 , max(MontanaPopulationData$Year) , by = 10)) +
  labs(x = "Year",
       y = "Population",
       title = "Population vs. Year")

# section b)

growth_rate = MontanaPopulationData %>%
  # first sort by year
  arrange(Year) %>%
  mutate(
    Diff_year = Year - lag(Year),# Difference in time (just in case there are gaps)
    Diff_growth = Population - lag(Population),# Difference in route between years
    percent = (Diff_growth) / lag(Population) * 100 # growth rate in percent
  ) 

growth_rate$percent[is.na(growth_rate$percent)] = 0 # Assigning value 0 to NA

growth_rate$diff_percent = abs(growth_rate$percent - lag(growth_rate$percent))

popplot2 <- growth_rate %>%
  ggplot(aes(x = Year, y = percent))

popplot2  + geom_line(size = 1.3,col='red') +  
  geom_point(size = 2.2)  + 
  theme_bw()  + 
  scale_y_continuous(breaks = seq(0 , 100 , by = 10)) +
  scale_x_continuous(breaks = seq(1880 , max(MontanaPopulationData$Year) , by = 10))+
  labs(x = "Year",
       y = "% Change in Population",
       title = "% Change in Population vs. Year") +
  geom_text(aes(label = round(percent, 2)),
            vjust = 'outward', hjust = -0.2,
            show.legend = FALSE)
help(geom_text)
