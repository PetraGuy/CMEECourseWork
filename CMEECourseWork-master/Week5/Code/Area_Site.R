#Week 5, QGSI - I am using ggmap!Datacamp.
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy ist November 2017

library(ggmap)
library(ggplot2)
Area_Site = read.csv("../Data/Area_Site.csv")
corvallis =  c(lon = -123.260, lats = 44.5646) #specify a location
map_5 = get_map(location = corvallis,zoom=10, scale =1) # get the map, needs location, zoom

ggmap(map_5) #ggmap plots the map

corvallis_map = get_map(location = corvallis, zoom = 13, scale = 1)

ggmap(corvallis_map)

# add something on top of a map. Data will need a mapping. Lets get Uk map and add areas and sites of Woodland Survey
mapImageData3 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 7)
ggmap(mapImageData3)

#create a map of the woodlands

ggmap(mapImageData3)

#need to convert data from eastings and northings

library(rgdal) # rgdal is not available for newer R Studio, therefore spTransform wont work
#I think this might make the whole mapping thing a bit tricky. I gues the data could be loaded
#into qgis and transfomred, but does that take away the whole point of doing it in R?

ggmap(mapImageData3)+
geom_point(aes(Easting,Northing), data = Area_Site)
Area_Site$Easting  <- spTransform(Area_Site$Site, CRS("+proj=longlat +ellps=GRS80"))
