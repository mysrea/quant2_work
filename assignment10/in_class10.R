
# Setup -------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(readr)
data(world)
library(modelsummary)

conflict_events <- read_csv("conflict_events.csv")

events_sf <- st_as_sf(conflict_events,
                      coords=c("longitude", "latitude"),
                      crs = 4326)

world_events <- st_join(events_sf, world)


# OLS --------------------------------------------------------------------

lm_lifegdp <- lm(lifeExp~log(gdpPercap), data=world_events)
summary(lm_lifegdp)

lm_areapop <- lm(log(pop)~log(area_km2), data=world_events)
summary(lm_areapop)


# Table and Plot -------------------------------------------------------------------

modelsummary(lm_areapop, output="areapop_output.tex")

ggplot(world_events,aes(x=pop, y=area_km2))+
  geom_point()
ggsave("areapop_plot.pdf",plot=get_last_plot())
