
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)
rm(list = ls())
cat("\014")

woods = read.csv("../Data/EastingNorthing.csv")
colnames(woods) = c("Site", "Easting","Northing","GridRef","Lat","Long")

#just for plotting
data = cbind(woods$Lat, woods$Long)

#d is calucalted in MainProjRichness.Rmd - the simple richness of each wood
wood_rich = cbind(woods,d)

#might need for extent?
lat_range =c(50,59)
lon_range = c(-7,2)

####### This ggplot thing stopped working ###

UK <- map_data(map = "world", region = "UK")

ggplot() + geom_polygon(data = UK, aes(x = long, y = lat),group = group) +
  coord_map()+
  geom_point(data=wood_rich, aes(x = Long,y=Lat)) 

###############################
  
ggplot(UK, aes(x = long, y = lat),group = group)+
  geom_polygon(colour = "black", size = 0.25, fill = "white", aes(group = group)) +
  geom_point(data = wood_rich, aes(x = Long, y = Lat, col = d), size = 3)

######ggmap version,  scaled these by diversity with max richess/min richness ####
#,in_max from MainProjRichness

map<- get_map(location = c(lon = -1.5, lat = 54),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 6) 
  
ggmap(map, extent ='device')+
    geom_point(aes(x = Long, y = Lat, colour = d),data = wood_rich, size = 2, alpha=1)+
    scale_color_gradient(low = "yellow", high="red")+
  geom_point(aes(x = Long, y = Lat, colour = d),data = min_max, size = 4, alpha=1)+
  scale_color_gradient(low = "yellow", high="red")+
  geom_text_repel(data = good_bad, aes(x = Long, y = Lat,label = d), 
            size = 3, colour = "white")

##################################

#select nearby woods

nearby_woods = woods%>%filter(Long> -1.3 & Lat < 51.5  )
nearby_woods

map<- get_map(location = c(lon = 0, lat = 51.25),
              color = "color",
              source = "google",
              maptype = "satellite",
              zoom = 9) 

ggmap(map, extent ='device')+
  geom_point(aes(x = Long, y = Lat),data = nearby_woods, color="red", size=2, alpha=1)
###############################



