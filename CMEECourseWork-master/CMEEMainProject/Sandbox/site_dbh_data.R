


sitedata = read.csv("../Data/AnalysisEnvdataLevelSite.csv")
ancientwoods = sitedata%>%filter(AW==1|PAWS==1) #= 67 woods

##what is median richness?
medianrichness = median(d) # =73

#select woods with above median richness
above_median = wood_rich%>%filter(d>73)
high = wood_rich%>%filter(d>100)

dbhmeans = read.csv("../Data/dbh_means.csv")
dbhmeans = dbhmeans[-1]
site_mean = dbhmeans[98,]
site_richness = plot_rich[98,]
tmp = rbind(site_mean, site_richness)
tmp = t(tmp)
colnames(tmp) = c("dbhmean","richness")

png("../Data/Talk/richnessvsmean")
ggplot(as.data.frame(tmp), aes(x=dbhmean, y = richness))+
  geom_point(size = 3)+
  ggtitle("Richess with dbhmean for plots in site 98")
dev.off()