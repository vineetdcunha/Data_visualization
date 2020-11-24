install.packages('vcd')
library('vcd')

install.packages('vcdExtra')
library('vcdExtra')

install.packages('tidyverse')
library('tidyverse')

Orders_merged$delivery_days_groups <- ifelse(as.numeric(Orders_merged$`delivery days`) <= 7, '1 week delivery', ifelse(as.numeric(Orders_merged$`delivery days`) <= 15, '15 day delivery', ifelse(as.numeric(Orders_merged$`delivery days`) <= 30, '1 month delivery', ifelse(as.numeric(Orders_merged$`delivery days`) <= 60, '2 month delivery', 'Above 2 months'))))

order_mosaic= table(Orders_merged$review_score,Orders_merged$delivery_days_groups)

order_mosaic = order_mosaic[,c(2,1,4,3,5)]

subs_pal <- colorspace::qualitative_hcl(5)

mosaicplot(order_mosaic, main = "Delivery days vs. Review Score",col = subs_pal, off = 5, las = 1,xlab="Review Score",ylab="Numberof delivery days")
