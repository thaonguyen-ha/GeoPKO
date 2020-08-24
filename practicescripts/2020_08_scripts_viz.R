#### report

#### setting up ####
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial) #package to add scale bar to the map
library(ggmap)
library(viridis)
library(maps)
library(gganimate)
library(glue)
library(leaflet)
library(rworldmap)
library(maptools)
library(maps)
library(ggthemes)
library(ggnewscale)
#### import the dataset ####

geopko <- Geo_PKO_NH_2020_08_04

#obtaining TCC list

tccdf <- geopko %>% select(Source, Mission, Location, 51:84)
colnames(TCCdf) <- sub("nameofTCC4", "nameofTCC_4", colnames(TCCdf))
colnames(TCCdf) <- sub("notroopsperTCC4", "notroopsperTCC_4", colnames(TCCdf))

tccdf <- tccdf %>% pivot_longer(c(4:37), names_to=c(".value", "TCC_id"), names_sep="_")
tccdf <- tccdf %>% filter(nameofTCC!="NA")
write.csv(tccdf, "tccdf.csv")

listtcc <- tccdf %>% distinct(nameofTCC)
write.csv(listtcc, "List_of_Unique_TCC.csv")

#making world map
world_mission <- geopko %>% select(Mission, Country) %>% distinct() %>% filter(Country!="Ethiopia/Eritrea") %>%
  group_by(Country) %>% mutate(mission.per.country=n()) %>% ungroup()
world_mission <- world_mission %>% group_by(Country, mission.per.country) %>% 
  summarize(list.mission=str_c(Mission, collapse=", "))

world <- ne_countries(scale = "medium", returnclass = "sf")
p1 <- ggplot(data=world) + geom_sf() + theme_void()+
  coord_cartesian(ylim = c(-50, 90))
p1

worldplot <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  coord_cartesian(ylim = c(-50, 90)) 
worldplot

worlddf <- map_data("world")
world.sf <- sf::st_as_sf(worlddf, coords = c("long", "lat"), crs = 4326) %>% 
  group_by(group) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

worldplot <- ggplot() +
  geom_sf(data = world.sf, colour = "gray85", fill = "gray80") + 
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_blank()) +
  geom_sf(data=world_mission, aes(fill=Country))
worldplot

####other desc plots####
# no missions
NoMission <- geopko %>% select(Year, Mission) %>% distinct(Year, Mission) %>% count(Year) %>% filter(!is.na(Year)) %>%
  filter(Year!=1993) %>% filter(Year<2019)
Plot1 <- ggplot(NoMission, aes(x=as.factor(Year), y=n, group=1)) + geom_point() + geom_line(size=0.5) +
  scale_x_discrete("Year", breaks=seq(1994,2018, 1))+
  theme_classic()+
  scale_y_continuous("Number of missions", breaks=seq(6,18,2)) +
  theme(panel.grid=element_blank(),
        axis.text.x = element_blank(),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())
Plot1

# number of troops

troopsize <- geopko %>% select(Mission, Year, Location, No.troops) %>%
  group_by(Mission, Year, Location) %>%
  summarize(ave.per.location=round(mean(No.troops))) %>%
  group_by(Mission, Year) %>%
  summarize(total.per.missions=sum(ave.per.location)) %>% filter(!is.na(total.per.missions)) %>%
  group_by(Year) %>% summarize(final=sum(total.per.missions)) %>% filter(Year!=1993) %>%
  filter(Year <2019)

griddf <- left_join(troopsize, NoMission, by="Year")

Plot2 <- ggplot(troopsize, aes(x=(as.numeric(Year)), y=final)) + geom_bar(stat="identity")+
  theme_classic()+
  scale_x_continuous("Year", breaks=seq(1994, 2018, 1))+
  scale_y_continuous("Total deployment")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
Plot2



# size of deployment by continent

contdf <- geopko %>% select(Mission, Year, Country, Location, No.troops) %>% 
  mutate(Country2=ifelse(Country=="Ethiopia/Eritrea", "Eritrea", Country)) %>%
  select(-Country)
geopkocountry <- unique(contdf$Country2)
geopkocountry
contdf <- as.data.frame(contdf)
#matching region
contdf$continent <- countrycode(sourcevar = contdf[, "Country2"],
                                origin = "country.name",
                                destination = "continent")
contdf <- contdf %>%mutate(continent=ifelse(Country2=="Kosovo", "Europe", continent))
contdf <- contdf %>% filter(!is.na(Mission))

#debug <- contdf %>% filter(is.na(continent))
#warning

troopcont <- contdf %>% group_by(Mission, Year, Location, continent) %>%
  summarize(ave.per.location=round(mean(No.troops))) %>%
  group_by(Mission, Year, continent) %>%
  summarize(total.per.missionyear=sum(ave.per.location)) %>%
  mutate(total.per.missionyear=ifelse(is.na(total.per.missionyear),0,total.per.missionyear))%>%
  group_by(Year, continent) %>% mutate(total.per.year.cont=sum(total.per.missionyear)) %>%
  select(-Mission, -total.per.missionyear) %>% distinct() %>%ungroup()

troopcont <- troopcont %>% mutate(total.per.year.cont=ifelse(is.na(total.per.year.cont),0,total.per.year.cont))
troopcont$Year <- as.factor(troopcont$Year)
troopcont$continent <- as.factor(troopcont$continent)
troopcont2 <- troopcont %>% complete(Year, continent, fill=list(total.per.year.cont=0))
troopcont2 <- troopcont2 %>% filter(Year!="1993",Year!="2019", Year!="2020")

Plot3 <- ggplot(troopcont2, aes(x=as.factor(Year), y=total.per.year.cont, fill=continent)) + geom_bar(stat="identity")+
  theme_classic()+
  scale_x_discrete("Year", breaks=seq(1994, 2018, 1))+
  scale_fill_discrete("Region")+
  scale_y_continuous("Total UN Peacekeeping Troops")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position="bottom")
Plot3
#creating the stacked plot

library(grid)
grid.newpage()
q <- grid.draw(rbind(ggplotGrob(Plot1), ggplotGrob(Plot3), size = "last"))


ggsave("stacked_plots_2.png", plot=q)

library(gridExtra)
g <- grid.arrange(Plot1, Plot3)

####UNIFIL map####
library(GADMTools)
cbPalette2 <- c("#E69F00","#0072B2", "#CC79A7")

lebanon <- gadm_sf_loadCountries("LBN", level=1)
lebanon2 <- gadm.subset(lebanon, regions=c("South", "Nabatiyeh")) 
lebanon2 <- lebanon2 %>% mutate(No.troops=ifelse(is.na(No.troops), 0, No.troops))

unifilmap <- ggplot() +geom_sf(data=lebanon2$sf, fill="grey90", color="white") + theme_map()+
  geom_point(data=unifil2019, aes(x=Longitude, y=Latitude, 
                                  size=as.numeric(No.troops),
                                  color=as.factor(No.TCC)),
             alpha=.6)+
  scale_size_continuous(range=c(5,15),
                        name="Deployment size", breaks=c(150, 300, 450, 600, 750, 900, 1250, 2050))+
  scale_color_brewer(palette="Set1", breaks=c(0:6), name="No. TCCs")+
  geom_point(data=unifil2019 %>% filter(HQ==3), aes(x=Longitude, y=Latitude),
             shape=4, size =7, color="hotpink")+
  geom_label_repel(data=unifil2019 %>% filter(HQ==3), 
                   aes(x=Longitude, y=Latitude, label=paste("UNIFIL HQ")),
                   box.padding = 1,
                   size = 3, 
                   fill = alpha(c("white"),0.7),
                   nudge_y = c(-.5,0))+
  guides(color=guide_legend(order=1),
         size=guide_legend(order=2, ncol=2))+
  theme(legend.position="right")

unifilmap

ggsave("UNIFIL2019.png", plot=unifilmap)
library(ggrepel)

mybreaks <- c(0.02, 0.04, 0.08, 1, 7)
unifilmap

unifil2019 <- geopko %>% filter(Mission=="UNIFIL", 
                                Source=="Map No. 4144 Rev. 44")

carplot <- ggplot() + geom_sf(data=CAR$sf, color="grey90", fill="grey78") + 
  geom_point(data=cardf, aes(x=Longitude, y=Latitude, size=No.troops, color=as.factor(No.TCC)), alpha=.7)+
  scale_color_brewer(name="No. of TCCs")+
  scale_size_continuous(name="Deployment size", range=c(5,13))+
  new_scale_color()+
  geom_point(data=cardf %>% filter(UNPOL.dummy==1), aes(x=Longitude, y=Latitude, shape="UNPOL", color="UNPOL2"), size=3)+
  geom_point(data=cardf %>% filter(UNMO.dummy==1), aes(x=Longitude, y=Latitude, shape="UNMO", color="UNMO2"), size=3)+
  scale_color_manual(name="", values=c("UNPOL2"="green", "UNMO2"="blue"))+
  scale_shape_manual(name = "", breaks=c("UNPOL2","UNMO2"), values=c("plus", "triangle")) +
  theme_map()+
  theme(legend.position="right")
carplot2

#### unique TCCs number

TCCdf <- geopko %>% select(Source, Year, Mission, Location, 50:83)
TCCno <- TCCdf %>% pivot_longer(c(5:38), names_to=c(".value", "TCC_id"), names_sep="_")
TCCno2 <- TCCno %>% select(-Source, - Location, -TCC_id, -notroopsperTCC) %>% filter(nameofTCC!="NA") %>% distinct()
TCCno3 <- TCCno2 %>% select(-Mission) %>% distinct() %>% arrange(Year) %>% 
  count(Year) %>% filter(Year<2019)

TCCnoplot <- ggplot(data=TCCno3, aes(x=as.factor(Year), y=n, group=1)) + geom_line(linetype="dashed") + geom_point() + 
  theme_classic()+
  scale_x_discrete("Year", breaks=seq(1994, 2018, 1))+
  scale_y_continuous("Troop-contributing Countries")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position="bottom")
TCCnoplot

ggsave("TCCnoplot.png", plot=TCCnoplot)

tcc.by.mission <- TCCdf %>% pivot_longer(c(6:38), names_to=c(".value", "TCC_id"), names_sep="_")
tcc.by.mission2 <- tcc.by.mission %>% filter(nameoftcc!="NA") %>%
  select(-Location, -TCC_id, -notroopspertcc) %>% group_by(Source, Year, Mission) %>% distinct(nameoftcc) %>%
  summarize(no.tcc.mission.year=n(),
            list.tcc=str_c(nameoftcc, collapse=", ")) 

tcc.by.mission.year <- tcc.by.mission2 %>% group_by(Year, Mission) %>% slice(1) %>%ungroup() %>% arrange(no.tcc.mission.year)

####  choropleth ####
world2 <- left_join(world, world_mission2, by=c("admin"="Country"))
world3 <- anti_join(world, world_mission, by=c("admin"="Country"))
cbPalette <- c("#999999", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
world_mission2 <- world_mission %>% ungroup() %>%
  mutate(Country=ifelse(Country=="North Macedonia", "Macedonia",
                        ifelse(Country=="Timor-Leste", "East Timor",
                               ifelse(Country=="DRC",
                                      "Democratic Republic of the Congo", Country))))

#world2 <- world2 %>% mutate(mission.per.country=ifelse(is.na(mission.per.country),0,mission.per.country))


worldmap <- ggplot() + geom_sf(data=world2, aes(fill=as.factor(mission.per.country)),  color="grey90")+
  coord_sf(ylim = c(-50, 90), datum = NA)+
  scale_fill_viridis_d(option="plasma", na.value="grey80", alpha=.7, name="No. of Missions\nper Country")+
  theme_map()+
  theme(legend.position=c(0.1,0.1),
        legend.background=element_rect(fill="transparent"))



casia <- ggplot() +
  geom_sf(data=world2,
          aes(fill=as.factor(mission.per.country)), color="white", alpha=.8)+
  coord_sf(ylim=c(35,48), xlim=c(14,30))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey80", name="Number of Missions", drop=FALSE)+
  geom_sf_label_repel(data=worldwrap %>% filter(admin %in% c("Bosnia and Herzegovina",
                                                          "Croatia",
                                                          "Macedonia", 
                                                          "Kosovo",
                                                          "Montenegro")),
                      aes(label=wrap_lbl, geometry=geometry), fun.geometry=st_centroid,
                      point.padding=.25, box.padding=2,min.segment.length=0,
                      direction="both",
                      fill=alpha(c("white"),0.5))+
  geom_count(data=geopko %>% filter(Country %in% c("Croatia",
                                                   "Bosnia and Herzegovina",
                                                   "Montenegro",
                                                   "Kosovo",
                                                   "North Macedonia")), 
             aes(x=Longitude, y=Latitude, alpha=..n.., group=1), 
             color="blue3",size=2)+
  labs(alpha="Freq. of occurrences\nper location")+
  theme(legend.background=element_blank(),
        legend.position="right",
        legend.text.align = 0)

casia

ggsave("casia3.png", plot=casia)

casia <- ggplot() +
  geom_sf(data=world2,
          aes(fill=as.factor(mission.per.country)), color="white", alpha=.8)+
  coord_sf(ylim=c(36.5,48), xlim=c(13,25))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey80", name="Number of Missions", drop=FALSE)+
  geom_count(data=geopko %>% filter(Country %in% c("Croatia",
                                                   "Bosnia and Herzegovina",
                                                   "Montenegro",
                                                   "Kosovo",
                                                   "North Macedonia",
                                                   "Cyprus")), 
             aes(x=Longitude, y=Latitude, alpha=..n.., group=1), 
             color="blue3",size=2)+
  labs(alpha="Freq. of occurrences")+
  theme(legend.background = element_blank(),
        legend.position=c(0.4,0.1),
        legend.box="horizontal")+
  guides(fill=guide_legend(ncol=2))

casia

ggsave("casia4teaser.png", plot=casia)

#### Plotting unique location ####
camerica <-  ggplot() +
  geom_sf(data=world2,
          aes(geometry=geometry, fill=as.factor(mission.per.country)), color="grey90",alpha=.8) + 
  theme_map() +
  coord_sf(ylim=c(12,23), xlim=c(-96,-68))+
  geom_sf_label_repel(data=world2 %>% filter(admin %in% c("Haiti", "Guatemala")), 
                      aes(label=admin, geometry=geometry),
                      fun.geometry=st_centroid,
                      box.padding=2, point.padding=0.5)+
  scale_fill_brewer(palette="Set1", direction=1, na.value="grey80",
                    labels=c(NA,"1", "2", "3", "4", "5"),  
                    name="Number of Missions", drop=FALSE)+
  geom_point(data=uniquecame, aes(x=Longitude, y=Latitude), 
             color="blue3", shape=21, size=2,stroke=1)


camerica

uniquecame <- geopko %>% filter(Country %in% c("Haiti", "Guatemala"))%>% 
  select(Location, Longitude, Latitude, No.troops) %>%
  group_by(Location, Longitude, Latitude) %>% summarize(mean.size=mean(No.troops))

camerica2 <- camerica + geom_point(data=uniquecame, aes(x=Longitude, y=Latitude), 
                                   color="pink", shape=21, size=2,
                                   alpha=.7)
camerica2

ggsave("worldPKO.png", plot=worldmap)

#mideast

ggplot()+
  geom_sf(data=worldwrap,
          aes(geometry=geometry, fill=as.factor(mission.per.country)), color="grey90",alpha=.8)+
  coord_sf(ylim=c(33,35), xlim=c(33,38))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey80", name="Number of Missions", drop=FALSE)+
  geom_count(data=geopko, 
             aes(x=Longitude, y=Latitude, alpha=..n.., group=1), 
             color="blue3",size=2)+
  labs(alpha="Freq. of occurrences\nper location")


####world wrap for labels

worldwrap <- world2 %>% mutate(wrap_lbl=stringr::str_wrap(list.mission, width=15))

camerica <- ggplot() +
  geom_sf(data=worldwrap,
          aes(fill=as.factor(mission.per.country)), color="grey90", alpha=.8)+
  coord_sf(ylim=c(12,23), xlim=c(-96,-68))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey80", name="Number of Missions", drop=FALSE)+
  geom_sf_label_repel(data=worldwrap %>% filter(admin %in% c("Haiti", "Guatemala")),
                      aes(label=wrap_lbl, 
                          geometry=geometry), fun.geometry=st_centroid,
                      fill=alpha(c("white"),0.5),
                      direction="x",
                      box.padding=1,
                      point.padding=1)+
  geom_count(data=geopko %>% filter(Country %in% c("Haiti", "Guatemala")), 
             aes(x=Longitude, y=Latitude, alpha=..n.., group=1), 
             color="blue3",size=2)+
  labs(alpha="Freq. of occurrences\nper location")+
  guides(fill=guide_legend(nrow=2))+
  theme(legend.position=c(0.6, 0.), legend.box="horizontal",
        legend.background = element_blank())

camerica


ggsave("camerica3.png", plot=camerica)

africa <- ggplot() +
  geom_sf(data=world2, color="white")+
  geom_sf(data=worldwrap %>% filter(admin %in% listaf),
          aes(fill=as.factor(mission.per.country)), color="white", alpha=.8)+
  coord_sf(ylim=c(-35,40), xlim=c(-20,60))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey60", name="Number of Missions", drop=FALSE)+
  geom_point(data=geopko %>% filter(Country %in% listaf), 
             aes(x=Longitude, y=Latitude, size=No.TCC,group=1), 
             color="blue3", size=1)+
  labs(alpha="Freq. of occurrences\nper location")+
  guides(fill=guide_legend(nrow=2))+
  theme(legend.position=c(.1,0), legend.box="vertical",
        legend.background = element_blank())

africa
ggsave("africa3.png", plot=africa)
#a

camerica <- ggplot() +
  geom_sf(data=worldwrap,
          aes(fill=as.factor(mission.per.country)), color="white", alpha=.8)+
  coord_sf(ylim=c(12,23), xlim=c(-96,-68))+
  theme_map()+
  scale_fill_brewer(palette="Set1", direction=-1, 
                    na.value="grey80", name="Number of Missions", drop=FALSE)+
  geom_sf_label_repel(data=worldwrap %>% filter(admin %in% c("Haiti", "Guatemala")),
                      aes(label=wrap_lbl, 
                          geometry=geometry), fun.geometry=st_centroid,
                      fill=alpha(c("white"),0.5),
                      direction="x",
                      box.padding=1,
                      point.padding=1)+
  geom_count(data=geopko %>% filter(Country %in% c("Haiti", "Guatemala")), 
             aes(x=Longitude, y=Latitude, alpha=..n.., group=1), 
             color="blue3",size=2)+
  labs(alpha="Freq. of occurrences\nper location")+
  guides(fill=guide_legend(nrow=2))+
  theme(legend.position=c(0.6, 0.), legend.box="horizontal",
        legend.background = element_blank())

camerica

africadf <- left_join(world_mission2, world, by=c("Country"="admin"))
listafrica <- africadf %>% filter(region_un %in% "Africa") %>% distinct(Country)

#### facet size and year ####
ballist <- c("UNTAES", "UNPROFOR", "UNMOP", "UNCRO", "UNMIK", "UNMIBH", "UNPREDEP")
balkanyr <- geopko %>% filter(Mission %in% ballist) %>% 
  select(Mission, Year, Location, Latitude, Longitude, No.troops, HQ, No.TCC) %>%
  group_by(Mission, Year, Location) %>% 
  mutate(max.troops=as.integer(max(No.troops)),
         max.tcc=max(No.TCC)) %>% select(-No.TCC, -No.troops) %>% distinct()

balkanyr$Year <- as.numeric(balkanyr$Year)
balkanyr2 <- balkanyr %>% filter(Year<2003) %>% group_by()
UNMIBH <- geopko %>% filter(Mission == "UNMIBH")

casia2 <- ggplot() +
  geom_sf(data=world2, fill="grey90", color="white")+
  coord_sf(ylim=c(40,47.5), xlim=c(14,24))+
  theme_map()+
  geom_point(data=balkanyr2, 
             aes(x=Longitude, y=Latitude, size=max.troops, color=Mission), 
             shape=20, alpha=.7)+
  scale_size_continuous(name="Deployment size", 
                        range=c(2,10), breaks=c(0, 100, 300, 500, 1000, 2000, 3000))+
  scale_color_brewer(palette="Set1")+
  guides(color=guide_legend(override.aes = list(size=5)))+
  theme(legend.position="right")+
  facet_wrap(~Year)
casia2

ggsave("faceteurope.png", plot=casia2)


BH <- gadm_sf_loadCountries("BIH", level=2)
library(GADMTools)
UNMIBH$UNPOL.dummy <- as.factor(UNMIBH$UNPOL.dummy)

unmibh <- ggplot()+geom_sf(data=BH$sf, fill="grey90", color="grey50")+theme_map()+
  geom_point(data=UNMIBH %>% filter(Source %in% "Map No. 3946 Rev. 1"), 
             aes(x=Longitude, y=Latitude, shape=UNPOL.dummy), 
             fill="royalblue4", color="blue2", size=3, alpha=1)+
  geom_point(data=UNMIBH %>% filter(Source %in% "Map No. 3946 Rev. 1",
                                    HQ==3),
             aes(x=Longitude, y=Latitude, shape="MHQ"), color="red", size=4)+
  scale_shape_manual(values=c(23,4), labels=c("1"="UNPOL", "MHQ"="Mission HQ"), name="")+
  guides(shape=guide_legend(override.aes = list(size=c(3,4),
                                                color=c("blue2", "red"),
                                                fill=c("royalblue4","red"),
                                                alpha=c(.7,1)
                                                )))
unmibh
ggsave("unmibh.png", plot=unmibh)


geopko %>% filter(UNPOL.dummy==1, UNMO.dummy==1) %>% distinct(Mission)

haitisf <- gadm_sf_loadCountries("HTI", level=2)
haitidf <- geopko %>% filter(Country=="Haiti", 
                             Source=="Map no. 4224 Rev-28")

minustah <- ggplot()+geom_sf(data=haitisf$sf, fill="grey90", color="white")+theme_map()+
  geom_point(data=haitidf, aes(x=Longitude, y=Latitude, 
                                  size=as.numeric(No.troops),
                                  color=as.numeric(No.TCC)),
             alpha=.8)+
  scale_size_continuous(range=c(5,15),
                        name="Deployment size")+
  scale_color_continuous(name="Number of TCC", breaks=c(1, 2, 17),
                         high = "#132B43", low = "#56B1F7")+
  guides(color=guide_legend(order=1, override.aes = c(size=3)),
         size=guide_legend(order=2), override.aes=c(fill="white"))+
  theme(legend.position="right")

minustah  

ggsave("minustah2011.png", minustah)
####


#### missions with UNPOL and UNMO ####
library(GADMTools)
unocimaps <- gadm_sf_loadCountries("CIV", level=1)

unociex <- geopko %>% filter(Mission =="UNOCI", Year==2015, Month=="January")

unoci <- ggplot() + geom_sf(data=unocimaps$sf, fill="grey85", color="white") + 
  theme_map()+
  geom_point(data=unociex, 
             aes(x=Longitude, y=Latitude, size=No.troops, color=as.factor(No.TCC)),
             alpha=.6)+
  scale_size_continuous(range=c(3,10),
                        name="Deployment size")+
  scale_color_brewer(name="Number of TCC", palette="Set1", breaks=c(0:6))+
  geom_point(data=unociex %>% filter(UNMO.dummy==1), aes(x=Longitude, y=Latitude, shape="UNMO"), color="red")+
  geom_point(data=unociex %>% filter(UNPOL.dummy==1), aes(x=Longitude, y=Latitude, shape="UNPOL"), 
             fill="royalblue4", color="blue2")+
  scale_shape_manual(values=c(24,23), 
                     labels=c("UNMO"="UN Military Observers", "UNPOL"="UN Police"), name="")+
  theme(legend.position = "right", 
        legend.key = element_blank())+
  guides(color=guide_legend(ncol=2, override.aes = list(size=3)),
         size=guide_legend(ncol=2), 
         shape=guide_legend(override.aes = list(size=2, color=c("UNMO"="red",
                                                                "UNPOL"="blue2"), 
                                                fill=c("UNMO"="white", "UNPOL"="royalblue4"))))
unoci

ggsave("unoci2015.png", plot=unoci)


#### filling in the missing years ####
df <- tibble( year=c(1994, 1994, 1994, 1996, 1996, 1996, 1996), location=c("Uppsala", "Stockholm", "Gbg", "Uppsala", "Stockholm", "Umea", "Gavle"), value1= 1:7, value2= 7:13 )
map2 <- tibble(name = c('a', 'b', 'c'), from=c(1999, 2000, 1993), to=c(2002, 2005, 1999))
df %>% mutate(year = map2(from, to, seq)) %>% unnest(year)
