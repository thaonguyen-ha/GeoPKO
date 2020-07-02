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

map2018df <- geopko %>% filter(year==2018) %>%
  select(Mission, month, location, latitude, longitude, No.troops, HQ)
class(geopko$No.troops)
geopko$No.troops <- as.numeric(geopko$No.troops)

#aggregating and averaging

map2018df1 <- map2018df %>% group_by(location, latitude, longitude, Mission, HQ) %>%
  summarize(ave = mean(No.troops, na.rm=TRUE)) %>% ungroup()
summary(map2018df1)

# This is from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
str(world)

Africa <- world %>% filter(region_un == "Africa")


p2 <- ggplot(data=Africa) + geom_sf() +
  geom_point(data=map2018df1, aes(x=longitude, y=latitude, size=ave, color=ave), alpha=.7)+
  scale_color_continuous()+
  geom_point(data=map2018df1 %>% filter(HQ==3), aes(x=longitude, y=latitude),
             color="red", shape=4, size=10)+
  geom_label_repel(data=map2018df1 %>% filter(HQ==3), aes(x=longitude, y=latitude, label=Mission))+
  labs(title="UN Peacekeeping Deployment in Africa - 2018")+
  theme(panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.background=element_blank())

p2

# Data from rworldmap
data(countryExData)
str(countryExData)

Africa <- countryExData %>% filter(str_detect(EPI_regions, "Africa")) %>%
  filter(str_detect(GEO_subregion, "Africa"))

p1 <- ggplot(data=Africa)
p1
