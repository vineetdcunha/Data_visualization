library(dplyr)

install.packages("mosaic")
library(mosaic)

install.packages("lubridate")
library(lubridate)

library(ggplot2)

ggplot( data=Intel.1998, aes(x=Trading.Day,y=Close) ) + geom_line(size = 1) + theme_bw()

ggplot( data=Intel.1998, aes(x=Date,y=Volume) ) + geom_col( width = 0.5) + theme_classic()

Intel.1998$Range = Intel.1998$High - Intel.1998$Low

ggplot( data=Intel.1998, aes(x=Volume,y=Range) ) + geom_point(col="Red")

