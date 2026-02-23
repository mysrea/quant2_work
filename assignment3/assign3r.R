library(tidyverse)
library(modelsummary)
library(marginaleffects)

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
#df <- subset(df, !is.na(income))
df = na.omit(df)


#c)
summary(df$voted==1)
5797/6733
#5797; 86%
summary(df)

##1.2
turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout=mean(voted))

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")

##1.3

lpm1 = lm(voted ~ age + education + income + female, data = df)
broom::tidy(lpm1)
# Each year of education increases turnout by 0.0193
# Each year of education increases turnout probability by 1.9 percentage points

preds_lpm = predict(lpm1)
sum(preds_lpm<0)
sum(preds_lpm>1)
range(preds_lpm)

##1.4
log1 = glm(voted ~ age + education + income + female,
           family = binomial, data = df)
summary(log1)
broom::tidy(log1)
#rough estimate is exp
#divide estimate by standard error to determine statistical significance, it's in a good percentile if it is above 1.96
-4.05/0.266

#specific predicting
nd = data.frame(age=c(25,50),education=c(10,10),
                income=rep(20,2),female=rep(1,2))
predict(log1, newdata=nd,type="response")

exp(coef(log1))
# An odds ratio higher than one means there is a positive multiplicative change; more education, more turnout.

preds_logit = predict(log1, type = "response")
range(preds_logit)
#Between 0.25-0.99 are between 0 to 1

##1.5, comparison
#marginal effect
#what is the average effect of 5 more years of education?
avg_slopes(log1)
0.023*5 #11.5
#for the lpm
0.0193*5 #9.65
# They are similar predictions; the AMEs are similar to the LPM coefficients

#side by side
modelsummary(list("LPM" = lpm1, "Logit" = log1),
             vcov = list("robust", NULL), output = "markdown")

#interaction
#education:female -> when you are male, this is what education is
log2 = glm(voted ~ age + education*female + income,
           family = binomial, data = df)
summary(log2)
log3 = glm(voted ~ age + education*income + female,
           family = binomial, data = df)

plot_predictions(log3, condition = c("education","income"))


##1.6
plot_predictions(log1, condition = "education")
plot_predictions(log1, condition = c("age","female"))

#The male line starts the lowest point, around 0.7, and goes to around 0.95
#The female line starts around 0.75, and goes to around 0.975
#The education line starts around 0.75,and goes to around 0.975
#They all have positive relationships with turnout and have similar patterns

