install.packages("leaflet")
library(leaflet)
locations<-read.csv("locations.csv")
names(locations)<-c("region","name", "lat", "long", "mdn_age", "below_15", "post_sec")

below_15<-tail(locations[ order(locations[,"below_15"]),], n=7)
post_sec<-head(locations[ order(locations[,"post_sec"], locations[,"name"]),], n=7)
mdn_age <-subset(locations,mdn_age>35 & mdn_age<45)

leaflet(data = locations[c(1:30),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = below_15[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = post_sec[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = mdn_age[c(1:16),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

#Batch geocoder: http://www.mapdevelopers.com/batch_geocode_tool.php
