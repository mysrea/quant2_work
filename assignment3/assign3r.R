
# Setup -------------------------------------------------------------------
library(tidyverse)
library(modelsummary)
library(marginaleffects)

raw <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

##ifelse = always by observation of each row
##casewhen=many conditions


# In-class ------------------------------------------------------------------



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


# 2.1 ---------------------------------------------------------------------


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
# 158 did not graduate, 1442 did graduate
1442/1600

table(raw_na$classtype)
raw_na %>% count(hsgrad,classtype)

raw_na %>%
  group_by(classtype) %>%
  summarise(hsgrad=mean(hsgrad))


# There is a 90.1% graduation rate overall.
# The graduation rates per class type are similar, with 1=89%, 2=90%, and 3=91.3%. 


# 2.2 ---------------------------------------------------------------------

raw_na$small <- ifelse(raw_na$classtype ==1,1,0)
lpm1 = lm(hsgrad ~ small, data = raw_na)
summary(lpm1) # not statistically sig. -0.016

logit1 = glm(hsgrad ~ small, family = binomial, data = raw_na)
summary(logit1) # -0.17

# The 'small' coefficient represents the change between when a student goes from a regular class size to a smaller class size.
# Here, as a student goes from a larger to smaller class size, on average fewer students will graduate by about -1.6%

avg_slopes(logit1)
# The estimate is nearly identical to the LPM coefficient.

# 2.3 ---------------------------------------------------------------------

lpm2 = lm(hsgrad ~ small + race + yearssmall, data = raw_na)
summary(lpm2)
# small, -0.009

logit2 = glm(hsgrad ~ small + race + yearssmall, family = binomial, data = raw_na)
summary(logit2)

# For lpm2: the coefficient for small has decreased compared to the model without controls, from -0.016 to -0.009, and yearssmall has an even lower coefficient. The highest coefficient is -0.078 with the race control variable.
# With both of the models, the 'small' variable is not statistically significant and with controls it has decreased to less than 1% of an impact on graduation.
# Given the shift in the estimate, this would suggest improper randomization from omitted variable bias as the estimate shifted by about 0.7 once more controls were added. 

avg_slopes(logit2)

# 'Yearssmall' has an estimate of -0.025 in the logit2 model. The AME estimate is about -0.002 which would suggest a very minimal effect. On average, a one-year increase in time spent with a smaller class would reduce the chance of graduation by roughly 0.2%.


# 2.4 ---------------------------------------------------------------------

sp1 = data.frame(race=c(1,2), small=c(1,0), yearssmall=c(3,0))

predictions(logit2, newdata=sp1,type="response")

# For student 1: estimate of 0.902/90.2% and 97.5 CI is 0.932
# For student 2: estimate of 0.849/84.9% and 97.5 CI is 0.894

logmodel1<-plot_predictions(logit2, condition = c("yearssmall", "small"))
ggsave("yearss_small_plot.png", logmodel1, width = 6, height = 4)


# 2.5 ---------------------------------------------------------------------

logit3 = glm(hsgrad ~ small * race + yearssmall, family = binomial, data = raw_na)
summary(logit3)
avg_slopes(logit3)

# In this model, race is marked as statistically significant and reduces the likelihood of graduation. The AME estimate is -0.06 which suggests a reduction of about 6%.

avg_slopes(logit3, variables = "small", by = "race")
table(rawst$race)
table(raw_na$race)

# The estimates do vary, although not all of the race categories are present as estimates for Native American and Hispanic students are not included.
# For white people, the estimate is small but positive at about 0.2% whereas for students designated as an 'Other' race category, the estimate suggests a -36% change when shifting to a smaller class size.

# This is not especially accurate though because there is only one observation of someone in the 'Other' category and only 5 in the 'Asian' category. There appears to be no complete observations of people who are categorized as Native American or Hispanic. The data is heavily skewed towards white participants. 


# 2.6 ---------------------------------------------------------------------
models <- list(lpm1,lpm2,logit1,logit2)
modelsummary(models, vcov="robust", stars=TRUE, output="stars_lpmlog.html")
co_plot_s = modelplot(models, vcov = list("robust",NULL))
ggsave("coefplot_2_6.png", co_plot_s, width = 6, height = 4)


# The coefficients for 'small' are all negative as are the coefficients for 'yearssmall'. The AME estimates are also negative though small in magnitude. Across the LPMs and the logit models, small class size is negatively associated with graduation rate. They do not have exactly the same coefficients but tell similar stories.
# This suggests a reduction in high school graduation rate for smaller class sizes, although the reduction would not be high in magnitude. That being said, neither of these variables are considered statistically significant across the 4 models and the R-squared for the LPMs is very low, so this data in-and-of itself does not strongly argue for correlation between graduation rate and class size.
# Experimental studies are designed in part to remove the effect of confounding factors and to track causality. This experimental design can allow for an understanding of how small class sizes impact people without being concerned with effects like quality of teaching due to the randomization factor theoretically. I will note two important issues that I previously mentioned, however, regarding the lack of diversity in their participant races and how omitted variable bias may still play a role given the shift in coefficients once controls are added. 

# - Miles Young Schroeder