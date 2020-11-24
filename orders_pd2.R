head(Orders)

nrow(Orders)

ncol(Orders)

library(dplyr)

count(Orders %>% distinct(product_id))

count(Orders %>% distinct(seller_id))

count(Orders %>% distinct(customer_id))

Orders = Orders[,-c(-5,16,17)]
