
# Setup -------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(haven)
library(fixest)
library(plm)

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


#note: factoring a variable = makes it categorical
#factoring in a regression formula includes a dummy variable that creates a reference category

# look at variable spans when interpreting - if ind. & dep. are both on a scale of 0-100 then a coefficient for OLS will not be a percentage

#r2 is about accountability of variation, doesn't give causality


# 2.1 ---------------------------------------------------------------------

df <- read_dta("teaching_evals.dta")

n_distinct(df$InstrID) # 48
n_distinct(df$CourseID) # 254
n_distinct(df$Year) # 9

254/48 # = 5.29

# There are about 5.3 courses for each professor.
# There are 9 years that are accounted for; each observation is an individual-time pair so it is a 'long' panel.


# 2.2 ---------------------------------------------------------------------

m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
broom::tidy(m1)
modelsummary(m1, stars = TRUE)

# A one point increase in the share of A grades (going from 0 students to all students) increases the teacher's evaluation score by .359 points.

# Instructors may be genuinely good at teaching which would improve student ability and students may show their appreciation for this in their scores.
# Instructors may be generally kind and give their students higher scores, and this general kindness could be why students give them a higher evaluation.
# These examples would make it seem like the evaluations and grading generosity were closer in connection than they may be in reality.


# 2.3 ---------------------------------------------------------------------

m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

modelsummary(list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
             vcov = ~InstrID,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"),
             coef_rename = c("Apct" = "% with an A"))

# The instructor fixed effect is controlling for teachers' characteristics that do not change over time & across the different classes they may teach.
# The coefficient for both of the fixed effects models are slightly lower. There suggests a lower correlation between the amount of students with an A and their evaluation. The pooled OLS overestimates this correlation, meaning that the unobsereved characteristics of teachers do play a part in evaluations. 


# 2.4 ---------------------------------------------------------------------
pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")

m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

# The null hypothesis would be that there is not a correlation between the unit effects and the regressors. Under the null hypothesis, both models would be consistent. The Hausman test result is that one model is inconsistent. The p-value is over 0.05, so Random effects or Fixed effects are both valid to use.  
# The previous section's results suggested that accounting for teachers' consistent characteristics changed the result. Fixed effects account for similarities over time. There are certain to be qualities of a teacher whom would stay consistent over the years of teaching and would not randomly vary. Since fixed effects were not discounted from being used in the Hausman test, there are logically examples of fixed effects which could be taken into account, and FE is generally more credible, I would err on the side of used fixed effects. 

             