##next find out what @data is spatial
## find muni shapefile
packages<-c("rio", "rgdal", "maptools", "tmap", "scales", "leaflet")
lapply(packages, require, character.only = TRUE)
#coal<- rio::import("mtcars.xlsx")
coal<-read.csv("mtcars.csv") #contains (geo)code shared with poly.plot below
coal<-coal[,1:10]
str(coal)

#prep for shp file merge
columns.to.keep <- c("PID", "GEONAME", 'GEOCODE')
files <- list.files(pattern="*.shp$", recursive=TRUE, full.names=TRUE) 
uid <-1 
# get polygons from first file
poly.data<- readOGR(files[1], gsub("^.*/(.*).shp$", "\\1", files[1])) 
n <- length(slot(poly.data, "polygons"))
poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1))) 
uid <- uid + n 
poly.data <- poly.data[columns.to.keep]
# combine remaining polygons with first polygon
for (i in 2:length(files)) {
  temp.data <- readOGR(files[i], gsub("^.*/(.*).shp$", "\\1",files[i]))
  n <- length(slot(temp.data, "polygons")) 
  temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1))) 
  temp.data <- temp.data[columns.to.keep]
  uid <- uid + n 
  poly.data <- spRbind(poly.data,temp.data) 
}
#check nature of code in both
str(coal$Code)
str(poly.data@data$GEOCODE)

# Merge data with tmap's append_data function, make unecassary stuff NULL
coalmap <- append_data(poly.data, coal,key.shp="GEOCODE", key.data = "Code")
coalmap@data$GEONAME<-NULL
coalmap@data$GEOCODE<-NULL
plot(coalmap)
dev.off()
#check & change column
str(coalmap@data)
names(coalmap@data)[6:13]<-c("popln","pct.popl","age", "chld", "educ", "incm", "bldngs", "prjcts") 
coalmap2<-coalmap[complete.cases(coalmap$communities), ]
str(coalmap2@data)
dev.off()
identical(coal$Code,coalmap2@data$GEOCODE)

# Create a palette
dev.off()
cPalette <- colorNumeric(palette = "Blues", domain=coalmap@data$Code)
nhpopup <- paste0("<b>city_id: ", coalmap@data$communities)
# Now the interactive map:
leaflet(coalmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .8, 
              popup=nhpopup,
              color= ~cPalette(coalmap@data$Code)
  )%>%
  addLegend(position="bottomleft", 
            colors="#984ea3", labels="communities")
dev.off()

#another map
tm_shape(coalmap) +
  tm_fill("bldngs", title="bldngs of communities", palette = "PRGn")+
  tm_borders(alpha=.5) +
  tm_text("communities", size=0.2) + 
  tm_style_classic()
dev.off()

# Subset just the  data needed
Citygeo@data$C<-c("0003", "0043")#problem is all cities are colored, use another  eqn
qtm(Citygeo, "C")
#Alternative subsetting
C<-c("0003", "0043")
coalgeo <-Citygeo[Citygeo@data$GEOCODE==C,]
# tmap test plot of subset data
head(coalgeo@data)
qtm(Citygeo, C)




