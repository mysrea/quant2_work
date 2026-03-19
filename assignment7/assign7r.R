
# Setup -------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(haven)
library(fixest)
library(plm)
library(did)
library(sf)
library(spData)


# Classwork ---------------------------------------------------------------
data(world)

##1.1
class(world)
names(world)
nrow(world)

st_crs(world)
# EPSG:4326 , need to check CRS to make sure projection is same across what you're using & that you're in the right region, etc. 

unique(st_geometry_type(world))
# Multipolygon, 18 levels. Many countries have smaller islands, not every country is just one landmass

pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()
plot(world["gdpPercap"], main = "GDP per capita by country")

# GDP per capita is highest in North America, Europe and Australia. There are much darker areas in South America, Africa, and Asia. 

##1.2
africa = filter(world, continent == "Africa")
# africa = world %>% filter(continent == "Africa")
nrow(africa)
plot(africa["gdpPercap"], main = "GDP per capita-- Africa")

# Overall darker blue but distinct outliers that shows intense inequality across the continent.
# The number of UN-recognized states is different from the number that are in the dataset

world=world%>%
  mutate(pop_millions=pop/1e6)
gdp_by_continent=world%>%
  group_by(continent)%>%
  summarise(mean_gdpPercap=mean(gdpPercap,na.rm=TRUE))
print(gdp_by_continent)

nogeom_gdp <- st_drop_geometry(gdp_by_continent)
# Use st_drop_geometry() to get the plain data frame

africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(st_drop_geometry(africa_sorted), 5))

# Arranging in an order
# Equatorial Guinea, Gabon, Libya, Botswana, Algeria

##1.3
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf",width=10,height=5)

# North America, Europe, Australia have the warmest colors. Africa, South America have the darkest colors with slightly warmer colors for Asia.

# plot(africa["gdpPercap"], main = "GDP per capita-- Africa")
# africa = filter(world, continent == "Africa")

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita, Africa")
ggsave("africa_gdp.pdf", width = 10, height = 5)
# The wealth is concentrated in the north and the south, except for a small part in the midwest.

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita, Africa")
ggsave("africa_gdp.pdf", width = 10, height = 5)

# Border layers increase contrast and allow for country differences to be better understood especially if they have similar colors.


# 2.1 ---------------------------------------------------------------------

hw
