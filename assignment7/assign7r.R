
# Setup -------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(haven)
library(fixest)
library(plm)
library(sf)
library(spData)
options(scale_colour_continuous = c("red", "blue", "green", "purple", "yellow"))


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

events <- read.csv("conflict_events.csv")
class(events)
events_sf <- st_as_sf(events,
                   coords=c("longitude", "latitude"),
                   crs = 4326)
class(events_sf)
st_crs(events_sf)

# CRS 4326 is the coordinate system that our data uses. It's important to check the projection system since they take the 3D nature of the Earth into consideration in different areas/plots. Coords is telling R what to turn into a geometry column.


nrow(events_sf)
table(events_sf$event_type)
# The state-based event type is most common. 



p <- ggplot() +
  geom_sf(data=world, color = "grey") +
  geom_sf(data=events_sf, aes(fill = event_type, color=event_type))

print(p)
ggsave("world_color.png",width=10,height=5)

# Conflict events are only visible in Africa. The conflict is spread across the continent although there are some larger gaps in the southwest and northwest. 


# 2.2 ---------------------------------------------------------------------

st_crs(events_sf)
st_crs(world)

evwo <- st_join(events_sf, world)

nrow(events_sf)
nrow(evwo)

# st_join looks at the intersection of each dataframe to match them. It's important to check the projection system since they take the 3D nature of the Earth into consideration in different areas/plots in different ways.

sum(is.na(evwo$name_long))
1576/68354
#  There are 1576 without a matching country, or about 2.3% of the events.
# They may not match because (1) Borders can shift due to conflict (2) Events that are primarily fought on water

# Count the number of events and total fatalities per country. Hint: filter out events with no matching country, then use group by() and summarise() with n() and sum(). Arrange by descending event count and print the top 10 (use st drop geometry() to get a clean table). In a comment, are the results consistent with your knowledge of contemporary armed conflicts?

evwo2 = evwo %>% 
  drop_na() %>%
  group_by(name_long)%>%
  summarise(number=n(),total=sum(fatalities))%>%
  arrange(desc(total))

print(head(st_drop_geometry(evwo2), 10))

# I genuinely don't have much knowledge of contemporary armed conflicts, but I have heard of conflict in Rwanda so the high fatality count makes sense to me. 


# 2.3 ---------------------------------------------------------------------


world_count <- world %>%
  st_drop_geometry(evwo2) %>%
  left_join(select(evwo2, name_long, number), by="name_long") %>%
  replace_na(list(number=0))

nrow(world)
nrow(world_count)

world_count2 <- world %>%
  st_join(select(evwo2, name_long, number), left=TRUE) %>%
  replace_na(list(number=0))

ggplot(world_count2)+
  geom_sf(data=world_count2, aes(fill=number))+
  scale_fill_distiller(palette="Reds")
ggsave("world_reds.png",width=10,height=5)
# Overall the map does match the previous one considering that the events all take place in/around Africa. The only glaring difference is that there is a very light spot in the center which is mostly covered in the first event-level map.      

ggplot(world_count2)+
  geom_sf(data=world_count2, aes(fill=log1p(number)))+
  scale_fill_distiller(palette="YlOrRd", direction = 1, name = "Log(events+1)")
ggsave("world_logged.png",width=10,height=5)

# Log transformations tend to reduce the impact of outliers. There is more nuance/variation in the colors with the log transformation. 


# 2.5  --------------------------------------------------------------------

# Spatial data is complicated by the fact that conflicts between countries are not unlikely to occur around borders. Borders can change over time and can be unintuitive, making the decision of where to place a conflict difficult. I imagine that a researcher should make sure at the start of an analysis to check if there are border conflicts, and one possible way of dealing with this could be to name these events after the bordering countries when assigning their geometries.

# These joins are made for different types of dataframes. St_join requires a geometry column and left_join must be for a regular dataframe. The joins allow you to combine dataframes together based on similar information- st joins based on geometry and left joins based on a provided characteristic. You should use them depending on what type of dataframe that you have. 

# Miles Young Schroeder