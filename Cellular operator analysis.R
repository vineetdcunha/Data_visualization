cellPlans = data.frame(
  c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
    "Verizon", "ATT", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "ATT", "ATT", "Sprint"),
  c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 12, 16, 16,
    24, 24, 25, 30, 40),
  c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 80, 60,
    90, 90, 110, 80, 110, 135, 100))
names(cellPlans) = c("Company", "DataGB", "Price")



ggplot( data=cellPlans, aes(x=DataGB, y=Price, color=Company) ) + geom_line(size = 1.2) +  geom_point(size = 3) + ggtitle("Line Graph of DataGB vs. Price") + theme_bw()

ggplot( data=cellPlans, aes(x=Company, y=DataGB, fill=Company) ) + geom_boxplot(alpha=0.7) + stat_summary( fun = mean, geom="point", shape=20, size=6 , color="red", fill="red") + ggtitle("Plot of DataGB Plans by Company") + theme_bw() 

ggplot( data=cellPlans, aes(x=Company, y=Price, fill=Company) ) + geom_boxplot(alpha=0.7) + stat_summary( fun = mean, geom="point", shape=20, size=6, color="red", fill="red") + ggtitle("Plot of Price by Company") + theme_bw()

ggplot( data=cellPlans, aes(x=DataGB, y=Price,col=Company) ) + geom_point(aes(color = Company, size = Price), alpha = 0.7) + ggtitle("Bubble Plot of DataGB vs. Price") + theme_bw()
