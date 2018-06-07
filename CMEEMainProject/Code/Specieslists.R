
### This bit of code will allow species lists for different sites to be pulled out.
###NB ground flors has bryophytes - need to joing with veg_codes to remove these
library(dplyr)
library(gridExtra)
library(ggplot2)
groundflora = read.csv("../Data/GroundCover.csv")
colnames(groundflora) = c("Site","Plot","Nest","Cover","BRC","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species","BRC")

year2groundcover = groundflora%>%filter(Year == 2)
colnames(year2groundcover) = c("Site","Plot","Nest","Cover","BRC","Year")
species_per_plot = inner_join(year2groundcover, veg_codes)

ellenbergs = read.csv("../Data/Ellenbergs.csv")
colnames(ellenbergs) = c("BRC","Taxon name","Light","Moisture","pH","Nitrogen","Silinity")
  
##############################


#png("../Data/site88.png")
species74 =  species_per_plot%>%filter(Site==74)
species74 = unique(species74$Species)
write.csv(species20, "../Data/site20.csv")
grid.table(unique(species76$Species))

dev.off()


#### add the ellenbergs

