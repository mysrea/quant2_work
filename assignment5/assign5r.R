
# Setup -------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(haven)
library(fixest)

# Classwork ---------------------------------------------------------------

##1.1
pa = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

length(unique(pa$State)) # 50 states
length(unique(pa$Year)) # 32 years
table(table(pa$State))
pana <- na.omit(pa)

# Unbalanced panel: not one observation per state-year

summary(pa$PresApprov)
summary(pa$UnemPct)

pa_sub = pa %>%
  filter(State %in% c("California","Texas","NewYork"))

ggplot(pa_sub, aes(x = Year, y = PresApprov, color = State))+
  geom_line()+
  theme_minimal()+
  labs(x = "Year", y = "Presidential approval (%)", color = "State")

# Similar trends

ggplot(pa, aes(x = UnemPct, y = PresApprov, color = South))+
  geom_point(alpha = 0.4) +
  #geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position="none")+
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")

# Higher unemployment, lower presidential approval

##1.2

mpooled = lm(PresApprov ~ UnemPct, data = pa)
modelsummary(mpooled)
broom::tidy(mpooled)
mpooled2 = lm(PresApprov ~ UnemPct+South, data = pa)
modelsummary(list(mpooled,mpooled2), stars = TRUE)

# Higher unemployment decreases approval
# Variables that don't change within this timespan may be correlated with unemployment; state minimum wage policies, state discrimination policies, state unionization levels

##1.3 - entity fixed effects
#feols = fixed effects OLS

m_fe = feols(PresApprov ~ UnemPct | State, data = pa)
summary(m_fe)


modelsummary(list("Pooled OLS" = mpooled, "State FE" = m_fe),
             vcov = ~State,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"))

# State FE removes time-invariant differences, so South (a geographic variable) is dropped
# for FE: -0.451 , within-state effect. 
# for OLS: compares states with different unemployment levels to each other

##1.4
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = pa)
broom::tidy(m_twfe)

modelsummary(list("Pooled OLS" = mpooled,
                  "State FE" = m_fe,
                  "Two-Way FE" = m_twfe),
             vcov = ~State,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"))

# TWFE : significant, and steeper.
# Controlling for time & unit: one unit increase in unemployment (1%) decreases presidential approval by -1.4% 
# Two-way fixed effects accounts for common shocks; across time & the net of common trends. 'removing' the effect of common shocks shifts the coefficient.

#---- 
#note: factoring a variable = makes it categorical
#factoring in a regression formula includes a dummy variable that creates a reference category

# look at variable spans when interpreting - if ind. & dep. are both on a scale of 0-100 then a coefficient for OLS will not be a percentage

#r2 is about accountability of variation, doesn't give causality


# 2.1 ---------------------------------------------------------------------


