
### This bit of code will allow species lists for different sites to be pulled out. 
###will include bryophytes unless joined with veg_codes

groundflora = read.csv("../Data/GroundCover.csv")
colnames(groundflora) = c("Site","Plot","Nest","Cover","BRC","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species","BRC")

year2groundcover = groundflora%>%filter(Yr_2 == 2)
colnames(year2groundcover) = c("Site","Plot","Nest","Cover","BRC","Year")
species_per_plot = inner_join(year2groundcover, veg_codes)

species78_2 = species_per_plot%>%filter(Site==78)%>%filter(Plot==2)
png("../Data/Talk/78_2.png")
grid.table(species78_2)
species78_2
dev.off()

png("../Data/Talk/78_12")
species78_12 = species_per_plot%>%filter(Site==78)%>%filter(Plot==12)
grid.table(species78_12)
dev.off()

species78_14 = species_per_plot%>%filter(Site==78)%>%filter(Plot==14)
png("../Data/Talk/78_14")
grid.table(species78_14)
dev.off()

species83_3 = species_per_plot%>%filter(Site==83)%>%filter(Plot==3)
png("../Data/Talk/83_3")
grid.table(species83_3)
dev.off()

ggplot(data, aes(DBH_class))
  geom_bar()
 


