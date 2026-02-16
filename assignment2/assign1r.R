setwd("C:/Users/zebra/Pictures/uc3m/SPRING 2026/quant 2/quant2_work/assignment2")
qog = read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv")

library(tidyverse)
library(modelsummary)

##1.1
df = qog %>%
  select(country=cname,
         epi= epi_epi,
         women_parl= wdi_wip,
         gov_eff = wbgi_gee,
         green_seats = cpds_lg)

dfna = df[complete.cases(df), ]
# Only 36 observations remain.
summary(df)

##1.2

plot(df$epi, df$women_parl)
ggplot(df,aes(x=women_parl, y=epi))+
  geom_point()+
  geom_smooth()
# The data points look scattered without a clear pattern.
# EPI does look to have a decline once the share of women in parliament reaches 40%, but there are few observations of that category.

##1.3

m1 = lm(epi ~ women_parl, data = df)
# (Look at range of values and the distribution to start understanding the magnitude of coefficients.)
summary(m1)
broom::tidy(m1)

nd = data.frame(women_parl = quantile(df$women_parl, c(0.25,0.75),na.rm=TRUE))

predict(m1, newdata=nd)

# Without any women in parliament, the environmental support will be at 39.2% and it will increase by 0.3 as more women are in parliament.
# The EPI at the 25th percentile is 43.9 and the EPI at the 75th percentile is 49.5, so there is a 5.6 increase from 25th to 75th.

##1.4
m2 = lm(epi ~ women_parl + gov_eff, data = df)
summary(m2)
# The magnitude of women in parliament's effect has decreased as we have taking into consideration an additional variable. 
# After adding the control, the intercept has increased to about 44.2%. The share of women in parliament has decreased from 0.3 to 0.09, and government effectiveness has a much greater effect at 8.7.

##1.5
# Bivariate: 0.307
# Multivariate 1: 0.097
# Multivariate 2: 8.710
m3 = lm(gov_eff ~ women_parl, data = df)
summary(m3)
# Delta = 0.026

(0.097+8.710)*0.026
# 0.228 ? Not the same as 0.307
