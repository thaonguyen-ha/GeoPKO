library(tidyverse)
library(readr)
library(ggthemes)
library(knitr)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(viridis)
library(gganimate)
library(gifski)
library(lubridate)

#### create the Africa dataframe
geopko$No.troops <- as.numeric(geopko$No.troops)

gifdf <- geopko %>% select(Mission, year, location, latitude, longitude, No.troops) %>%
  group_by(Mission, year, location) %>%
  mutate(ave.no.troops = as.integer(mean(No.troops, na.rm=TRUE))) %>% select(-No.troops) %>% distinct()

gifdf <- geopko %>% group_by(year, location) %>% mutate(ave2 = sum(ave.no.troops))

gifdf

gifdf$year <- as.factor(gifdf$year)

world <- ne_countries(scale = "medium", returnclass = "sf")
Africa <- world %>% filter(region_un == "Africa")

gifdf <- gifdf %>% mutate(yearparsed=parse_number(year))

gifdf <- gifdf %>% drop_na(ave.no.troops)
p2 <-  ggplot(data=Africa) + geom_sf() +
  geom_point(data = gifdf, aes(x=longitude, y=latitude, size= ave.no.troops, color=ave.no.troops, group=year), alpha=.7)+
  guides( colour = guide_legend()) +
  labs(color='Average Troop Deployment') +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.8, l = 4, unit = "cm")),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank()
  )+
  transition_states(states=year, transition_length = 1, state_length=3)+
  labs(title="UNPKO: {closest_state}")+
  enter_fade()


animate(p2)



geom_point(data = gifdf %>% filter(HQ==3), aes (x=longitude, y=latitude), color = "red", shape = 4, size=7)+
  geom_label_repel(data = gifdf %>% filter(HQ==3), aes(x=longitude, y=latitude, label=Mission))

  ####testing for months?####

test1 <- gifdf %>% filter(year=="2007") %>% group_by(location) %>% filter(n()>1)


scale_size_continuous(name="Average Troop Deployment", trans="log", range=c(1,12), breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000)) +
  scale_alpha_continuous(name="Average Troop Deployment", trans="log", range=c(0.1, .9), breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000))+
  scale_color_viridis(option="cividis", trans="log", breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000), name="Average Troop Deployment" ) +



  #### bubble plot ####

