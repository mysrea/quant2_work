library(gapminder)
library(ggplot2)
gapminder<-read.csv("C:/Users/zebra/Pictures/uc3m/SPRING 2026/quant 2/quant2_work/assignment1/data/gapminder.csv")
#
head(gapminder)
str(gapminder)
#
countries<-c("Spain","Brazil","Canada","Chile")
df<-gapminder[gapminder$country%in%countries,]
