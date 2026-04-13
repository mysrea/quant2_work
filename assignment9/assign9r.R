
# Setup -------------------------------------------------------------------

library(carData)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(marginaleffects)
library(ggplot2)
library(survival)
library(broom)
data(BEPS)
data(bioChemists)


# Classwork ---------------------------------------------------------------
##1.1
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)
# The distribution is skewed to the right with the 3 & 4 answers having the highest responses. 
# We do not expect the difference between each numbered answer to be exactly the same for respondents. It's reasonable to assume that the distance of 3 to 4 would not be exactly the same as the distance between 4 to 5 for a person who is ranking their choices- the distance between 'a little' and 'much worse' may be different than 'a little' and 'stayed the same'

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)

# The raw coefficient on Europe is -0.123 which implies that a person's political knowledge decreases their optimism for the economy. 

avg_slopes(m_ologit)

# Europe: Groups 1/2/3 have positive signs; Groups 4/5 have negative signs. 
# Attitude towards European integration
# 0.0029 / 0.0158 / 0.00975
# -0.0222 / -0.0062
# Having more pro-integration attitudes decreases a person's optimism 
(-0.0222-0.0062)/2
# The average impact between these two is -0.0142 


predictions(m_ologit,
            newdata=datagrid(gender=c("female","male")))

# Pessimistic category: female & male estimates are similar.
# Optimistic category: men is slightly higher, but only by a little bit
# The difference is quite low across the board

library(dplyr)
preds = tidy(predictions(m_ologit, by = "gender"))
preds             

women1 = preds %>% filter(group==1 & gender == "female") %>% pull(estimate)

women2 = preds %>% filter(group==2 & gender == "female") %>% pull(estimate)

women1+women2

# factors = numbers are linked to categories

##1.2

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)

# Conservative = reference
# Positive feelings about Tony Blair increases likelihood to vote labour by a lot whereas it has much less of an effect on the likelihood to vote liberal democrat.

# If it was negative ; shifts probability lower than reference

avg_slopes(m_mlogit)

# Blair: Labour: 0.1
# Positive feelings of Blair increases likelihood of voting Labour(especially compared to the negative coefficients on the other parties). 

# For parties, the IIA fails when there are close substitutes. If two parties share very similar ideologies this could violate the assumption.

predictions(m_mlogit, by = "economic.cond.national")
# If you think the economy is very bad, you are 70% likely to vote conservative ?????


pred = tidy(predictions(m_mlogit, by = "economic.cond.national"))

ggplot(pred, aes(x=economic.cond.national, y=estimate, color=group))+ geom_line()

##1.3

summary(bioChemists$art)
var(bioChemists$art)
pdf("art_histogram.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles",
     xlab = "Number of articles", col = "gray80")
dev.off()

# Mean = 1.693
# Variance = 3.709
# The mean and variance are over 2 points away from eachother and the histogram shows a significant left skew which would indicate overdispersion
3.7-1.6

m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)
# ment = 0.025543
exp(0.025543)
# = 1.025
# A one unit increase in mentor articles multiplies student articles by 1.025
# Residual deviance: 1634.4  on 909  degrees of freedom

1634.4/909 # = over 1

dispersiontest(m_pois)

# over 1, 1.82454 ; p value is quite low so it would be significant. It is similar to the calculation above. This indicates significant overdispertion. The Poisson standard errors would be too small.

## 1.4

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)

# The coefficients are quite similar. The theta is 2.264 which suggests that there is overdispersion but it's not too intense.

AIC(m_pois, m_nb)

# The AIC results are relatively similar, 3314.113 and 3135.917. The negative binomial model has a lower result. Overdispersion appears to be an important part of this data which should be taken into account.

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))


# The predictions are 2.05 for men and 1.65 for women; the confidence intervals are 1.8-2.32 & 1.44-1.88 respectively. The predicted number of articles for men is  higher than estimated for women. There is a 0.4 difference between these estimates. The confidence intervals do overlap a bit at the lower end of the men's CI and higher end of the women's CI since 1.88 would be included within the men's CI.


# The Poisson standard errors would be too small. Overdispersion is indicated from the variance-to-mean ratio and the proper dispersion test. There is a lower AIC for the negative binomial model which further indicates Poisson is not the proper model and the negative binomial model would suit it better. The prediction results show that there is a difference in predicted articles for men and women, although the confidence intervals do slightly overlap. The negative binomial model  coefficients show there is a statistically significant effect of being a woman and having a kid as decreasing published articles whereas having a mentor increases it. 


# 2.1 ---------------------------------------------------------------------


