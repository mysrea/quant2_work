
# Setup -------------------------------------------------------------------
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(haven)
library(fixest)
library(plm)
library(did)
data(mpdta)

# Class -------------------------------------------------------------------

## 1.1

d = read.csv("minwage.csv")


d = d %>% mutate(NJ=ifelse(location !="PA",1, 0))
table(d$NJ)

df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm = TRUE))

# Before policy change: similar at 4.65 v 4.61
# After policy change: NJ is noticeably higher, 5.08 v 4.65

means=d%>%
  group_by(NJ)%>%
  summarise(
    before=mean(fullBefore,na.rm= TRUE),
    after = mean(fullAfter, na.rm=TRUE),
    change=after-before)
means

# PA: 10.7 to 8.1 ; -2.49
# NJ: 7.9 to 8 ; 0.435

nj_change=means$change[means$NJ==1]
pa_change=means$change[means$NJ==0]
did_est =nj_change-pa_change
did_est

# DiD estimates the difference of within group differences. Employment rose after treatment.

df_long=d%>%
  mutate(id=row_number())%>%
  pivot_longer(
    cols=c(fullBefore,fullAfter),
    names_to="period",
    values_to="full_emp")%>%
  mutate(
    post=ifelse(period=="fullAfter",1,0),
    NJ =ifelse(location!="PA",1,0))
nrow(d)
nrow(df_long)

# The long format means that each row is a time point for a subject. The unit of analysis has shifted to each measurement at a point in time. This allows for an analysis which considers timing.

## 1.2

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE)
# postxNJ is the treatment effect. Employment increases when minimum wage increases.
# postxNJ matches previous calculation
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(m_did_fe)

modelsummary(list(m_did, m_did_fe))

# They are approximately the same, meaning that chain-specific characteristics do not impact the relationship between minimum wage & employment rate. The difference between two chains is not relevant for how employment is approached based on this model, but it is important to control for as chains may have different but consistent practices for hiring.

# Before the treatment, it would be important to look at the employment trends. Did their hiring pattern have highly similar trends? A confounding difference between the two states could be a law preventing discrimination in hiring patterns in one state. 

## 1.3
df_long_wage = d %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

modelsummary(m_wage, stars = TRUE)
# 0.5 -> Treatment leads to a 0.5 increase in the wage. It is statistically significant. It is a relatively small difference considering that the minimum wage increased, but it makes sense that it is positive. 

# The wage result and the employment result show that after treatment, the amount of people hired and the money those people received both increased. If the wage had not risen, then that would suggest the treatment did not do anything; minimum wage increasing did not affect the wage. 


# 2.1 ---------------------------------------------------------------------

summary(mpdta)
length(unique(mpdta$countyreal))
length(unique(mpdta$first.treat))
table(mpdta$first.treat)

# There were 500 counties and 4 cohorts. There are more observations than counties because it is over time. Staggered treatment means that not everyone who gets treated is treated at the same time. This means that the "pre" and "post" period will be varied between groups of observations and this must be taken into consideration for analyses.

mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

mpdt_plot<-ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("teen employment over years.png", mpdt_plot, width=6,height=4)

# The staggered treatment effect complicates the data but I would say there are inconsistencies in pre-treatment behavior. The 2007 cohort stays flat where the never treated cohort has an upwards trajectory from 2004-2007. 
# The 2004 cohort declines after treatment until 2006. The 2006 cohort slightly declines after treatment. The graph cuts off before post-treatment for the 2007 cohort. 


# 2.2 ---------------------------------------------------------------------


