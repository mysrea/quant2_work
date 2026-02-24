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
edu_plot <- plot_predictions(log1, condition = "education")
ggsave("edu_plot.png", edu_plot, width = 6, height = 4)
fa_plot <- plot_predictions(log1, condition = c("age","female"))
ggsave("age_gender_plot.png", fa_plot, width = 6, height = 4)

# The male line starts the lowest point, around 0.7, and goes to around 0.95
# The female line starts around 0.75, and goes to around 0.975
# The education line starts around 0.75,and goes to around 0.975
# They all have positive relationships with turnout and have similar patterns

##1.7
plot3 = modelplot(list("LPM" = lpm1, "Logit" = log1),
                       vcov = list("robust", NULL))
plot3
ggsave("coefplot_lpm_logit.png", plot3, width = 6, height = 4)
# They have relatively similar conclusions. Differences matter the most in the cast where rare events are trying to be understood.

#################################

#2.1
rawst <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

classcol <- c("Small", "Regular", "Regular+Aide")
classfac <- factor(classcol, levels=c("Small", "Regular", "Regular+Aide"))
racecol <- c("White", "Black", "Asian", "Hispanic","Native American", "Other")
racefac <- factor(racecol, levels=c("White", "Black", "Asian", "Hispanic","Native American", "Other"))
levels(racefac)
levels(classfac)
small <- ifelse(classtype=="Small",1,0)

raw_na <- rawst %>% drop_na()
summary(rawst)
summary(raw_na)
# Observations go from 6325 to 1600.

#hsgrad: graduated from high school (0/1, our outcome)
table(raw_na$hsgrad)
#158 did not graduate, 1442 did graduate
1442/1600

table(raw_na$classtype)
raw_na %>% count(hsgrad,classtype)

raw_na %>%
  group_by(classtype) %>%
  summarise(hsgrad=mean(hsgrad))


#There is a 90.1% graduation rate overall.
#The graduation rates per classtype are similar, with 1=89%, 2=90%, and 3=91.3%. 

#2.2
raw_na$small <- ifelse(raw_na$classtype ==1,1,0)
lpm1 = lm(hsgrad ~ small, data = raw_na)
summary(lpm1) # not statistically sig. -0.016
logit1 = glm(hsgrad ~ small, family = binomial, data = raw_na)
summary(logit1) # -0.17

#c
