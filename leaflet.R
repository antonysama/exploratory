install.packages("leaflet")
library(leaflet)
locations<-read.csv("locations.csv", stringsAsFactors=FALSE)
locations<-locations[c(1:10,12)]
names(locations)<-c("region","name", "lat", "long", 
                    "mdn_age", "below_15", 
                    "post_sec", "income", "growth", "buildings",
                    "projects")

na_post_sec<-locations[which(is.na(locations$post_sec)), 1:2]
na_income<-locations[which(is.na(locations$income)), 1:2]


below_15<-tail(locations[ order(locations[,"below_15"]),], n=7)
post_sec<-head(locations[ order(locations[,"post_sec"], 
                                locations[,"name"]),], n=7)
mdn_age <-subset(locations,mdn_age>35 & mdn_age<45)
income<-head(locations[ order(locations[,"income"]),], n=7)
growth<-head(locations[ order(locations[,"growth"]),], n=7)
                              
leaflet(data = locations[c(1:30),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = below_15[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = post_sec[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = mdn_age[c(1:16),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = income[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = growth[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

North<-subset(locations, region=="North")
CS<-subset(locations, region=="CS")
Edmonton<-subset(locations, region=="Edmonton")

boxplot(income~region, data = locations, 
        main="Income data (regional dashboard~2013)",
        xlab="Location", ylab="Median family income($)")

boxplot(growth~region, data = locations, 
        main="Income growth data (regional dashboard~2013)",
        xlab="Location", ylab="Median family rowth(#.##)")

hist(North$income, xlim=c(60000,200000))
hist(Edmonton$income, xlim=c(60000,200000))
hist(CS$income, xlim=c(60000,200000))

boxplot(projects~region, data = locations, 
        main="Value of major projects (IMAP~2013-17)",
        xlab="Location", ylab="Value($)")

boxplot(buildings~region, data = locations, 
        main="Value of building permits (regional dashboard~2014)",
        xlab="Location", ylab="Value of building permits($)")

#Batch geocoder: http://www.mapdevelopers.com/batch_geocode_tool.php
