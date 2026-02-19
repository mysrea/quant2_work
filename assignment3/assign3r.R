library(tidyverse)
library(modelsummary)

raw <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

##ifelse = always by observation of each row
##casewhen=many conditions

##1.1
#a)
df = raw %>%
  transmute(
    voted=ifelse(V202109x<0,NA,V202109x),
    age=ifelse(V201507x<0,NA,V201507x),
    female=case_when(
    V201600==2~1, ##if 2, set one
    V201600==1~0,
    TRUE~NA_real_),
    education=case_when(
      V201511x==1~10,V201511x==2~12,V201511x==3~14,
      V201511x==4~16,V201511x==5~20, TRUE~NA_real_),
    income=ifelse(V201617x<0, NA, V201617x),
    party_id=ifelse(V201231x<0, NA, V201231x))

#b)
summary(df)
df = na.omit(df) 
nrow(df) ## from 8280 -> 6733

#c)
summary(df$voted==1)
5797/6733
#5797; 86%
summary(df)

##1.2
factor(df$education)
#levels=10,12,14,16,20

ggplot(df, aes(x=education, y=voted)) + 
  geom_bar(stat = "identity")