# Setup -------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(spData)
library(spdep)
library(spatialreg)
data(world)
library(ggplot2)


# Classwork ---------------------------------------------------------------

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

nrow(world)
# 160 observations remain after removing NAs. Log-transforming accounts for outliers and "smooths" out the data. so they have less of an effect.

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# Residuals: -20.479 to 8.115; Residuals are the prediction error.
# log_gdp's coefficient is 5.5 of which the implication would be calculated differently than a traditional coefficient because of the log transformation. It is statistically significant, and the R2 is 0.64 which is a relatively high R2. 

world$ols_resid=residuals(ols_fit)
ggplot(world)+
  geom_sf(aes(fill=ols_resid),color="white",linewidth=0.2)+
  scale_fill_gradient2(low="#2166ac",mid="white",high="#d6604d",
                       midpoint=0,name="OLSresidual")+
  theme_void()+
  labs(title="OLSresiduals:lifeexpectancy~logGDPpercapita")
ggsave("ols_residuals_map.pdf",width=10,height=5)

# The residuals are lowest in Russia and in many parts of Africa, specifically Central Africa. The most red parts are in South America and China. There is the greatest magnitude of prediction error, in a negative direction, for Central Africa. 
# Prediction error: We are over-predicting Africa. They live shorter than we should expect them to. This could be due to disease & weather which may not be particularly related to GDP. There are factors that cluster in space. 

##1.2

nb=poly2nb(world,queen=TRUE)
listw=nb2listw(nb,style= "W",zero.policy=TRUE)
summary(nb)

# 16 countries do not have borders. These would generally be islands.

moran.test(world$ols_resid,listw=listw,zero.policy=TRUE)

# Moran's I determines the degree of spatial correlation. 0 would mean that neighbors do not impact the result. Here, there is a 0.44 which would indicate that similar values tend to cluster. Therefore the OLS assumption of independent observations is violated. 

##1.3
lm_tests=lm.LMtests(ols_fit,listw=listw,
                    test=c("LMerr","LMlag","RLMerr","RLMlag"),
                    zero.policy=TRUE)
summary(lm_tests)

# RLMerr = RSerr = 52.17 ***
# RLMlag = RSlag = 0.06 []
# Only LMerr is statistically significant. 

# Robust err = 54.3 ***
# Robust lag = 2.19 
# Since only LMerr is significant, this suggests using SEM. 

##1.4
# Fitting spatial error model
sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

# log gdp here is 3.95 
# It has a very low P value, indicating significance. The lambda is 0.76 which, since it is positive & significant, indicates that there is spatial correlation and using this model is the 'right idea'. The coefficient, compared to previously, is lower. 
# Lambda represents the error term considering spatial autocorrelation. A positive and significant lambda indicates that there is a geographic connection which must be taken into consideration. Without using a spatial model, there are unmeasured spatially correlated factors. The area in which an independent variable shifts is connected to the outcome.

world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# The p-value is not below 0.05, it is  0.88. The Moran I statistic is -0.08 as compared to 0.44 which would suggest that values tend to alternate a bit, but not much and much less than calculated with OLS. This removes much of the spatial autocorrelation.

## 1.5

coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)


# Many more countries do not have neighbors here at 114 as compared to 16. This could be due to much larger countries having a further apart centroid from their edge. 

listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)

# Here, lambda is 0.43 and statistically significant. This indicates that there is spatial correlation. The lambda is lower, 0.43 compared to 0.76. The neighborhood defined is clearly important to the way that 

world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw_dist, zero.policy = TRUE)

# The test statistic is -0.02 which shows that there is very little spacial correlation, even less than beforehand but it is not significant since the p-value is 0.49.


# 2.1 ---------------------------------------------------------------------




