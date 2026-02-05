library(gapminder)
library(ggplot2)
gapminder<-read.csv("C:/Users/zebra/Pictures/uc3m/SPRING 2026/quant 2/quant2_work/assignment1/data/gapminder.csv")
#
head(gapminder)
str(gapminder)
#
countries<-c("Spain","Brazil","Canada","Chile")
df<-gapminder[gapminder$country%in%countries,]
#
ggplot(df,aes(x=year,y= lifeExp,color=country))+
  geom_line()+
  geom_point()+
  labs(x ="Year",y="Life Expectancy",
       title="Life Expectancy Over Time",
       subtitle="Across 4 Countries")+
  theme_minimal()
ggsave("assignment1/ass1_plot.png",width=7,height=5)
#