#Prep, read file & check
getwd()
list.files()
locations<-read.csv("locations_1.1.csv")
locations<-locations[c(1:8,10,12,13,14,15)]  #Take what we need
str(locations)  
head(locations, n=4)

#if needed...renames columns
names(locations)<-c("region","name", "lat", "long", 
                    "age", "children", 
                    "education", "income",  "buildings",
                    "projects", "tax", "ratesprd", "small_biz")
as.numeric("children", "education", "tax",  "small_biz")
head(locations, n=1)

#if needed ...creates an index 
#locations<-locations; seq(along=locations)
#row.names(locations) <- seq_len(nrow(df)) - 1

#selects locatons with na's
na_education<-locations[which(is.na(locations$education)), 1:2] 
na_income<-locations[which(is.na(locations$income)), 1:2]

#orders indicators of interest
children_tail<-tail(locations[ order(locations[,"children"]),], n=7)
children_head<-head(locations[ order(locations[,"children"]),], n=7)
education_head<-head(locations[ order(locations[,"education"], 
                                locations[,"name"]),], n=7)
new_locations<-na.omit(locations[c(1:7)])
education_tail<-tail(new_locations[ order(new_locations[,"education"], 
                                     new_locations[,"name"]),], n=7)
SnstvAge <-subset(locations,age>35 & age<55)
Adptv_age <-subset(locations,age<35 | age>55)

income_head<-head(locations[ order(locations[,"income"]),], n=7)
income_tail<-tail(locations[ order(locations[,"income"]),], n=7) 

growth_head<-head(locations[ order(locations[,"growth"]),], n=7)
growth_tail<-tail(locations[ order(locations[,"growth"]),], n=7)

tax_head<-head(locations[ order(locations[,"tax"]),], n=7)
tax_tail<-tail(locations[ order(locations[,"tax"]),], n=7)

small_biz_head<-head(locations[ order(locations[,"small_biz"]),], n=7)
small_biz_tail<-tail(locations[ order(locations[,"small_biz"]),], n=7)
#Spatial package
install.packages("leaflet")
library(leaflet)

#maps indicators                              
leaflet(data = locations[c(1:30),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = children_head[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = children_tail[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = education_head[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = education_tail[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = age[c(1:16),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = rvs_age[c(1:16),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = income_head[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = income_tail[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = tax_head[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = tax_tail[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = small_biz_head[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))

leaflet(data = small_biz_tail[c(1:7),]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(name))
#subseets regions
BR<-subset(locations, region=="Battle River")
Gns<-subset(locations, region=="Genessee")
Sh<-subset(locations, region=="Sheerness")
Mlnr<-subset(locations, region=="Milner")


#box plots regional income& growth
boxplot(income~region, data = locations, 
        main="Median Family Income (AB Regional dashboard~2013)",
        xlab="Location", ylab="income($)")

boxplot(children~region, data = locations, 
        main="Proportion  of Children  (Statistics Canada~2011)",
        xlab="Location", ylab="Proportion in popln.")
boxplot(children~region, data = locations, 
        main="Proportion  of Children  (Statistics Canada~2011)",
        xlab="Location", ylab="Proportion in popln.")

boxplot(age~region, data = locations, 
        main="Median Age of Population(Statisitcs Canada~2011)",
        xlab="Location", ylab="Median Age(yrs)")

boxplot(education~region, data = locations, 
        main="Proportion w/ PostSec. Education 
        (AB Regional dashboard~2011,2014)",
        xlab="Location", ylab="Proportion in popln.")

boxplot(education~region, data = locations, 
        main="Proportion w/ PostSec. Education 
        (AB Regional dashboard~2011,2014)",
        xlab="Location", ylab="Proportion in popln.")

boxplot(ratesprd~region, data = locations, 
        main="Rate Spread of Property Tax 
        (Municipal Affairs,2015)",
        xlab="Location", ylab="Non Res minus Residential Rate (%)")
boxplot(small_biz~region, data = locations, 
        main="Proportion of  Small Businesses(employees<49)
        (Statisitcs  Canada,2014)",
        xlab="Location", ylab="Proportion of businesses (%)")
#histogram of regional income
hist(North$income, xlim=c(60000,200000))
hist(Edmonton$income, xlim=c(60000,200000))
hist(CS$income, xlim=c(60000,200000))

#box plots regional projects & building permits
boxplot(projects~region, data = locations, 
        main="Value of major projects (IMAP~2013-17)",
        xlab="Location", ylab="Value($)")

boxplot(buildings~region, data = locations, 
        main="Value of building permits (regional dashboard~2014)",
        xlab="Location", ylab="Value of building permits($)")

#Batch geocoder: http://www.mapdevelopers.com/batch_geocode_tool.php
write.csv(children_head, file = "children_head.csv")
write.csv(children_tail, file = "children_tail.csv")
write.csv(education_head, file = "education_head.csv")
write.csv(education_tail, file = "education_tail.csv")
write.csv(age, file = "age.csv")
write.csv(rvs_age, file = "rvs_age.csv")
write.csv(income_head, file = "income_head.csv")
write.csv(income_tail, file = "income_tail.csv")
write.csv(tax_head, file = "tax_head.csv")
write.csv(tax_tail, file = "tax_tail.csv")
write.csv(small_biz_head, file = "small_biz_head.csv")
write.csv(smal_biz_tail, file = "small_biz_tail.csv")
