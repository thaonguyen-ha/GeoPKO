library(leaflet)
library(tidyverse)

unamsil <- geopko %>% filter(Mission=="UNAMSIL")

unamsil$No.troops <- as.numeric(unamsil$No.troops)

map <- leaflet() %>% setView(lat = 8.681437, lng = -11.761370, zoom=8) %>%
  addTiles () %>%
  addLayersControl(
    overlayGroups = c("Deployment size", "Mission HQ"),
    options=layersControlOptions(collapsed = FALSE)
  ) %>%
  addCircleMarkers(data= unamsil, lat= ~latitude, lng=~longitude, weight=1,
                   radius= ~(No.troops)^(1/3), color="pink",
                   fillOpacity = 0.2, group="Deployment size",
                   label=paste(unamsil$location,unamsil$No.troops),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", color="blue"),
                     textsize = "15px", direction = "auto")
                   ) %>%
  addMarkers(data=unamsil %>% filter(HQ==3), lat=~latitude, lng=~longitude,
                   group="Mission HQ")

map



#### creating new df for 2013 deployment ####
df2013 <- geopko %>% filter(year==2013) %>%
  select(Source, Mission, year, month, location, latitude, longitude, No.troops, RES_No, 13:43, HQ)
df2013$No.troops <- as.numeric(df2013$No.troops)
df2013$RES_No <- as.numeric(df2013$RES_No)

df2013 <- df2013 %>% mutate(NonRES = No.troops - RES_No)
df2013_dep <- df2013 %>% group_by(Mission, year, location, latitude, longitude) %>%
    summarize(ave=round(mean(No.troops)), min.troops=min(No.troops), max.troops=max(No.troops),
           mean.no.RES=mean(NonRES))


#### tcc df
df2013_tcc <- df2013
colnames(df2013_tcc) <- sub("name.of.TCC", "nameofTCC_", colnames(df2013_tcc))
colnames(df2013_tcc) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(df2013_tcc))
df2013_tcc <- df2013_tcc %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character)
df2013_tcc <- df2013_tcc %>% mutate_at(vars(starts_with("nameofTCC")), as.character)


df2013_tcc2 <- df2013_tcc %>% pivot_longer(c(13:39), names_to=c(".value", "TCC_id"), names_sep="_")%>%
  mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
  select(Mission, location, latitude, longitude,No.TCC, nameofTCC, notroopsperTCC) %>%
  filter(!is.na(nameofTCC)) %>%
  mutate(overview=str_c(nameofTCC, notroopsperTCC, sep="-")) %>%
  group_by(Mission, location, latitude, longitude, No.TCC) %>%
  summarise(details=str_c(overview, collapse=", "))


  group_by(Mission, location, nameofTCC)%>%
  summarise(total.tcc=sum(notroopsperTCC)) %>%
  add_count(Source, name="No.TCC") %>%
