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
m3 = lm(gov_eff ~ women_parl, data = df)
summary(m3)

beta1_biva = tidy(m1) %>% filter(term == "women_parl") %>% pull(estimate)
beta1_mult = tidy(m2) %>% filter(term == "women_parl") %>% pull(estimate)
beta2_mult = tidy(m2) %>% filter(term == "gov_eff") %>% pull(estimate)
aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate)
round(beta1_mult + beta2_mult * delta, 4)
round(beta1_biva, 4)
# The bivariate estimate is inflated from government efficiency being correlated with the share of women in parliament and the environmental support level.

##1.6
modelsummary(m2)
modelsummary(m2, vcov= "robust")
# 1.338 -> 1.282 / 0.048 -> 0.047 # 0.647 -> 0.67
# The standard errors have changed a small amount/only slightly which would not be likely to impact conclusions.

##1.7
modelsummary(list(m1, m2), vcov = "robust")
modelsummary::modelplot(list(m1, m2), vcov = "robust")
ggsave("a2modelp1.png",plot=get_last_plot())

##1.8, extra
# Standardizing variables, using substantive knowledge/background knowledge, previous research

#######################################

##2.1
star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star.csv")

classcol <- c("Small", "Regular", "Regular+Aide")
classfac <- factor(classcol, levels=c("Small", "Regular", "Regular+Aide"))
racecol <- c("White", "Black", "Asian", "Hispanic","Native American", "Other")
racefac <- factor(racecol, levels=c("White", "Black", "Asian", "Hispanic","Native American", "Other"))
levels(racefac)
levels(classfac)

small <- ifelse(classtype=="Small",1,0)
star$small <- ifelse(star$classtype ==1,1,0)


summary(star)
broom::tidy(star)
3930+2395
2353+3972
# g4math has 2395 valid observations + NA 3930 = total of 6325
# g4reading has 2353 valid observations + NA 3972 = total of 6325

##2.2
aggregate(star$g4reading, list(star$classtype), FUN=mean, na.rm=TRUE)
# Small = 723, regular = 719, small+aide = 720
# The small class scored the highest.

m12 <- lm(g4reading ~ small, data=star)
modelsummary(m12, vcov="robust", stars=TRUE)
# The coefficient of 'small' is 3.1, which would suggest that a smaller class size increases the reading test score by about 3 points.

723.3912-720.7155
# Result is 2.6757 which is relatively close to 3.1.

m22 <- lm(g4math ~ small, data=star)
modelsummary(m22, vcov="robust", stars=TRUE)
# The coefficient of 'small' against g4math is 0.591 which would suggest that a smaller class size does improve math scores, but to a much smaller degree than for reading scores.

##2.3
m32 <- lm(g4reading ~ small+race+yearssmall, data=star)
modelsummary(m32, vcov="robust", stars=TRUE)
# The results are quite different from the first model.
# This model suggests that a smaller class size would reduce the score of a student by about 6 points.
# The randomization seems imperfect as such a different coefficient once controls are added would imply an omitted variable bias.
# Yearssmall is how many years a student spent in a small class. A coefficient of 2.074 would suggest that a student's score would increase by about 2 for each year they spent in a smaller class.

##2.4
m42 <- lm(g4reading ~ small * race + yearssmall, data = star)
modelsummary(m42, vcov="robust", stars=TRUE)
broom::tidy(m4)
# The coefficient estimate for small x race is 11.3 with a standard error of 5.47 & a robust standard error of 10.4, and it is not marked as statistically significant.
751.069+11.299 #=762.368 for White students
751.069+(11.299*2) #=773.667 for Black students
# This would suggest that a smaller class size is more beneficial for different races.
# This is not substantively valuable as (1) it does not account for mixed races and (2) it assigns an ordering of races which suggests that, for example, those of an Other race will always have a score higher than those of a Native American race. 
# Additionally: the interaction term is not statistically significant (nor is the 'small' coefficient by itself).
# The R2 & the R2 adjusted are also very low, suggesting that this model is a poor fit.
# If the randomization was not properly done, as I suggested from the previous addition of controls, then there could still be omitted variable bias.

##2.5
modelsummary(list(m12,m32,m42), vcov="robust", stars=TRUE, output="readingmodels.html")
modelplot(list(m12,m32,m42), vcov = "robust")
ggsave("a2modelp2.png",plot=get_last_plot())

##2.6
# In the first model, just a small class size has a positive effect for reading. It has a noticeably larger negative effect in the second and third models when more controls are added.
# Each year in a small classroom is noted to have a small positive effect on the reading score.
# This would suggest to me that class size alone is not a determining factor in test scores, but that perhaps extended time spent in a smaller class size is what is truly beneficial.
# I'm unfamiliar with typical observational studies of class size, but what I can see the benefit of this study being is that each individual's score was analyzed instead of it just being a classwide average.
# It is particularly beneficial to note how many years a student spent in a classroom since that, it seems, yearssmall has a different effect on student scores than just the general notion that a student has, at some point, spent time in a small class.
# As for caveats, I will mention what I have brought up in previous answers: none of the variables except for race are noted as statistically significant, the randomization seems imperfect, evaluating race in a structured order seems analytically problematic, + R2 & the R2 adjusted are very low.

# - Miles Young Schroeder