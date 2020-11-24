head(shoppers)

nrow(shoppers)

ncol(shoppers)

library(dplyr)

shoppers_num = select_if(shoppers,is.numeric)

summary(shoppers_num)

NAS = colnames(shoppers)[colSums(is.na(shoppers))>0]

head(colSums(is.na(shoppers)))

shoppers = shoppers[complete.cases(shoppers), ]     

shoppers %>% distinct(Month)

shoppers %>% distinct(OperatingSystems)

shoppers %>% distinct(Browser)

shoppers %>% distinct(Region)

shoppers %>% distinct(TrafficType)

shoppers %>% distinct(VisitorType)

shoppers %>% distinct(Weekend)

shoppers %>% distinct(Revenue)

sum(shoppers['VisitorType'] == 'Other')

count(shoppers[shoppers['ProductRelated_Duration'] == -1,])

count(shoppers[shoppers['Informational_Duration'] == -1,])

count(shoppers[shoppers['Administrative_Duration'] == -1,])

shoppers<-shoppers[!(shoppers$Administrative_Duration==-1),]

shoppers_num = select_if(shoppers,is.numeric)
