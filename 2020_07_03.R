library(tidyverse)
library(ggmap)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial) #package to add scale bar to the map
library(tidyverse)
library(ggmap)
library(viridis)
library(maps)
library(gganimate)
library(glue)
library(leaflet)
library(rworldmap)
library(maptools)
library(ggrepel)
library(ggsflabel)

map2009df <- geopko %>% filter(year==2009) %>%
  select(Mission, month, location, latitude, longitude, No.troops, HQ)
class(geopko$No.troops)
geopko$No.troops <- as.numeric(geopko$No.troops)

#aggregating and averaging

map2009df1 <- map2009df %>% group_by(location, latitude, longitude, Mission, HQ) %>%
  summarize(ave = mean(No.troops, na.rm=TRUE)) %>% ungroup()
summary(map2009df1)

# This is from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
str(world)

Africa <- world %>% filter(region_un == "Africa")


p2 <- ggplot(data=Africa) + geom_sf() +
  geom_point(data=map2009df1, aes(x=longitude, y=latitude, color=ave), alpha=.7)+
  scale_color_continuous()+
  geom_point(data=par2009, aes(x=longitude, y=latitude, size=best_est), shape = 4, color="red")+
  labs(title="UN Peacekeeping Deployment in Africa - 2009")+
  theme(panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.background=element_blank())

p2


geom_point(data=par2009, aes(x=longitude, y=latitude, size=best_est), shape = 4, color="red")+
#Sort par
par2009 <- par %>% filter(year==2009)

p3 <- ggplot(data=Africa) +
  geom_sf()+
  geom_point(data=par2009, aes(x=longitude, y=latitude, size=best_est), shape = 4, color="red")
p3

p4 <- ggplot(data=Africa) + geom_sf() +geom_point(data=)
