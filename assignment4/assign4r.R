
# Setup -------------------------------------------------------------------
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(haven)


# Classwork ---------------------------------------------------------------

##1.1


corruption <- read_dta("corruption.dta")

corn <- subset(corruption, !is.na(ti_cpi))
corn <- subset(corruption, !is.na(undp_gdp))
summary(corruption)
summary(corn)
# all of them remain?

# Right skew: mean > median - yes


##1.2

ggplot(corn,aes(x=undp_gdp, y=ti_cpi))+
  geom_point()+
  geom_smooth(method="lm")

# looks very skewed to the right, roughly linear but with extreme outliers

ggplot(corn,aes(x=log(undp_gdp), y=log(ti_cpi)))+
  geom_point()+
  geom_smooth(method="lm")

# less skewed towards the right side and seems more linear, roughly

##1.3
m1 = lm(ti_cpi ~ undp_gdp, data = corn)
m1 # marginal effect = 0.000173

broom::tidy(m1)
coef(m1)["undp_gdp"] * 10000
# the marginal effect of undp_p on ti_cpi
# a small increase of GDP per capita/PPP will impact the  corruption index by 0.000173
# predicted change of corruption index = 1.729

q25 = quantile(corn$undp_gdp, 0.25)
q75 = quantile(corn$undp_gdp, 0.75)
c(q25,q75)
predictions(m1, newdata=datagrid(undp_gdp=c(q25,q75)))
# 25%, 1974 - estimate is 2.84, 97.5CI is 3.07
# 75%, 10862 - estimate is 4.38, 97.5CI is 4.57

##1.4
m2 = lm(ti_cpi ~ log(undp_gdp), data = corn)
summary(m2)
predictions(m2, newdata=datagrid(undp_gdp=c(1000,1001,1050,2000,2001)))
# as the GDP increases, the corruption increases
coef(m2)["log(undp_gdp)"]*log(1)
coef(m2)["log(undp_gdp)"]*log(2)

#I is used for an operation in a regression- I(undp_gdp^2)

m3 = lm(ti_cpi~undp_gdp + I(undp_gdp^2), data=corn)
summary(m3)
broom::tidy(m3)

models_r4 <- list(m1,m2,m3)
modelsummary(models_r4, vcov="robust", stars=TRUE)

plot_predictions(m1,condition="undp_gdp")
plot_predictions(m2,condition="undp_gdp")
plot_predictions(m3,condition="undp_gdp")

plot(m1)

##1.5

# The R-squared is highest for the quadratic model

avg_slopes(m2,variables="undp_gdp")
#AME = 0.00524 ; the corruption increases by 0.00524 for a small increase in PPP. Average slopes
#it differs from the raw coefficient because it is an average over all of the observed values; average predicted change in the corruption index across all countries in the sample for a 1-dollar increase

slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))

#estimate decreases with a higher GDP, going from 0.0002 @ 2k to 0.001 @ 30k; the effect diminishes

##1.6
p1 = plot_predictions(m2,condition="undp_gdp")
p2 = plot_predictions(m3,condition="undp_gdp")
ggsave("prediction_plot_a3rp1.png",p1,width=6,height=4)
ggsave("prediction_plot_a3rp2.png",p2,width=6,height=4)
p1
p2
# They both increase at the beginning and then have diminishing returns

##1.7
m1_aug = broom::augment(m1)

ggplot(m1_aug,aes(x=.fitted,y=.resid))+
  geom_point()+
  geom_hline(yintercept=0,linetype="dashed")+
  labs(x="Fittedvalues",y="Residuals",title="ResidualsvsFitted:Level-Level(m1)")
m2_aug=broom::augment(m2)
ggplot(m2_aug,aes(x=.fitted,y=.resid))+
  geom_point()+
  geom_hline(yintercept=0,linetype="dashed")+
  labs(x="Fittedvalues",y="Residuals",title="ResidualsvsFitted:Level-Log(m2)")

# log improves, less skewed; spread of residuals becoming less frequent as values increase indicates hetero

n=nrow(corn)
threshold=4/n
cooks_d=cooks.distance(m2)
influential=which(cooks_d>threshold)
corn$cname[influential]
plot(m2,which=4)
# don't automatically remove influential observations, look at them individually and assess how they were collected, etc. robustness checks - compare coefficients if the influential observations are removed

##1.8

modelsummary(
  list("Level-Level"=m1,"Level-Log"=m2,"Quadratic"=m3),
  vcov="robust",
  stars=TRUE,
  gof_map=c("r.squared","nobs"),
  output="markdown")

#model 3: highest r2, best residual diagnostics, clear substantive interpretation
# diminishing returns