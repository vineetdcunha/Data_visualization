library(dplyr)

library(timeDate)

library(ggplot2)

library(grid)

Orders_grouped_number = Orders %>%
  mutate(date_col = as.Date(order_purchase_timestamp)) %>%
  group_by(date_col) %>%
  summarize(value = n())

Orders_grouped = Orders %>%
  mutate(date_col = as.Date(order_purchase_timestamp)) %>%
  group_by(date_col) %>%
  summarize(value = sum(price))

Orders_grouped_delivery = Orders %>%
  mutate(date_col = as.Date(order_delivered_customer_date)) %>%
  group_by(date_col) %>%
  summarize(value = n())

Orders_grouped_carrier = Orders %>%
  mutate(date_col = as.Date(order_delivered_carrier_date)) %>%
  group_by(date_col) %>%
  summarize(value = n())

Orders_grouped = filter(Orders_grouped, between(date_col, as.Date("2017-01-01"), as.Date("2019-01-01")))

Orders_grouped_number = filter(Orders_grouped_number, between(date_col, as.Date("2017-01-01"), as.Date("2019-01-01")))

Orders_grouped_delivery = filter(Orders_grouped_delivery,between(date_col, as.Date("2017-01-01"), as.Date("2019-01-01")))

Orders_grouped_carrier = filter(Orders_grouped_carrier,between(date_col, as.Date("2017-01-01"), as.Date("2019-01-01")))


df <-
  dplyr::left_join(Orders_grouped,
                   Orders_grouped_number,
                   by = c("date_col" = "date_col"))

df1 <-
  dplyr::left_join(df, Orders_grouped_delivery, by = c("date_col" = "date_col"))

df2 <-
  dplyr::left_join(df1, Orders_grouped_carrier, by = c("date_col" = "date_col"))

df2 = df2 %>% 
  rename(
    total_sales = value.x,
    num_orders = value.y,
    num_deliveries = value.x.x,
    in_transit = value.y.y,
  )

p1 <-
  ggplot(data = df2, aes(x = date_col, y = log2(total_sales))) +
  geom_line(size = 1) + stat_smooth(color = "#FC4E07",
                                    fill = "#FC4E07",
                                    method = "loess") + labs(y = "Total Sales (log2)", title = "Total sales vs. Date (Month Year)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x =  element_blank())  + scale_x_date(date_breaks = "1 months", date_labels = "%m %Y")

p2 <-
  ggplot() + geom_line(
    data = df2,
    aes(x = date_col, y = num_orders,color = "blue"),
    size = 1.2
  )  +
  geom_line(
    data = df2,
    aes(x = date_col, y = num_deliveries,color = "red"),
    size = 0.7
  )  +
  geom_line(
    data = df2,
    aes(x = date_col, y = in_transit,color = "green"),
    size = 0.75
  ) + theme_minimal() + xlab("Date (Month Year)") +
  labs(y = "Number of orders / deliveries / in transit", title = "Number of orders / deliveries / in transit vs. Date (Month Year)") +
  scale_color_manual(name = "Group",values = c(green = "green", red = "red", blue = "blue"),labels = c("Orders", "In Transit", "Deliveries")) +
  theme(axis.text.x = element_text(angle = 90),legend.position="bottom") + scale_x_date(date_breaks = "1 months", date_labels = "%m %Y") 

grid.newpage()

grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
