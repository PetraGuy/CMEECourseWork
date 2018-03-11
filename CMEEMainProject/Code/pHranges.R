
# just small bit of analysis for talk
# import plot level vars and ground cover to look at riches and poorest site

plotcsvfile = read.csv("../Data/AnalysisEnvDataLevelPlot.csv")

groundflora = read.csv("../Data/GroundCover.csv")
colnames(groundflora) = c("Site","Plot","Nest","Cover","BRC","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species","BRC")

year2groundcover = groundflora%>%filter(Yr_2 == 2)
colnames(year2groundcover) = c("Site","Plot","Nest","Cover","BRC","Year")
species_per_plot = inner_join(year2groundcover, veg_codes)

ellenbergs = read.csv("../Data/Ellenbergs.csv")
colnames(ellenbergs) = c("BRC","Taxon name","Light","Moisture","pH","Nitrogen","Silinity")

###########################################


# plot of soil pH range 
site83_plots = plotcsvfile%>%filter(Site == 83)
site78_plots = plotcsvfile%>%filter(Site == 78)
phdata = cbind(site83_plots$pHYr2, site78_plots$pHYr2)
colnames(phdata) =c("site83","site78")
melted = as.data.frame(melt(phdata))
colnames(melted) = c("plot","site","pH")

png("../Data/Talk/soilpH.png")
ggplot(melted)+geom_boxplot(aes_string(x="site", y="pH",na.rm = TRUE))+
  ggtitle("range of soil pH measured")+
  theme(axis.text=element_text(size = 12))

###################################

species78= species_per_plot%>%filter(Site==78)
ellenbergs78 = inner_join(species78,ellenbergs)

species83= species_per_plot%>%filter(Site==83)
ellenbergs83 = inner_join(species83,ellenbergs)

#convert to a format for box plot and facet wrap plot of ellenbergs
site78data = ellenbergs78[,c(1,9,10,11,12)]
site78data$Site = "site78"
meltedsite78data = melt(site78data)

site83data = ellenbergs83[,c(1,9,10,11,12)]
site83data$Site = "site83"
meltedsite83data=melt(site83data)

twosites = rbind(meltedsite78data,meltedsite83data)

png("../Data/Talk/ellenbergs.png")
ggplot(data = twosites, aes_string(x = "variable", y = "value"))+
  facet_wrap(~Site, nrow = 2 )+
  geom_boxplot()+
  theme(axis.text=element_text(size = 12),
        strip.text = element_text(size = 12 ))
dev.off()
