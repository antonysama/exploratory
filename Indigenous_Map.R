#shp we are using:RURl, svillage, city, indian, town, village
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
#GREPL coal indigenous
toMatch<-c("Paul first", "ENOCh", "aLEXIS", "Alexander", "Montana", "Samson", "Louis Bull", 
           "Ermineskin", "Siksika", "O'Chiese", "Sunchild", "Aseniwuche")
poly.data@data$coal_indig<- ifelse(grepl(paste(toMatch, collapse = "|"), 
                                         poly.data@data$GEONAME, ignore.case = T), 1, 0)
#best altntv: poly.data@data$coal_indig<-grepl("paul|enoch", poly.data@data$GEONAME, ignore.case = T)
#case snstv: poly.data@data$coal_indig<-c("Discard", "Keep")[grepl("(paul|ENOCH)", poly.data@data$GEONAME) + 1]

# Another map: Create a palette
dev.off()
cPalette <- colorNumeric(palette = "Blues", domain= poly.data@data$coal_indig)
nhpopup <- paste0("<b>community_id: ",poly.data@data$GEONAME)
# Now the interactive map:
leaflet(poly.data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .8, 
              popup=nhpopup,
              color= ~cPalette(poly.data@data$coal_indig)
  )%>%
  addLegend(position="bottomleft", 
            colors="#984ea3", labels="GEOCODE")
dev.off()

#a map
dev.off()
tm_shape(poly.data) +
  tm_fill("coal_indig", title="bldngs of communities", palette = "PRGn")+
  tm_borders(alpha=.5) +
  #tm_text("", size=0.2) + 
  tm_style_classic()


