

plot(AllSiteVars$meandbh, AllSiteVars$sd_meandbh)
plot(AllSiteVars$meanph, AllSiteVars$sd_pH)
plot(AllSiteVars$meanSOM, AllSiteVars$sd_SOM)


ggplot(AllPlotVars, aes(x = reorder(as.factor(Site), SOMYr2, median), y = SOMYr2))+
  geom_boxplot()+
  xlab("Site")+
  theme(axis.text.x = element_text(angle=90, hjust=1))
  
ggplot(AllPlotVars, aes(x = reorder(as.factor(Site), pHYr2, mean), y = pHYr2))+
  geom_boxplot()+
  xlab("Site")+
  theme(axis.text.x = element_text(angle=90, hjust=1))


ggplot(AllPlotVars, aes(x = reorder(as.factor(Site), mean_dbh, median), y = mean_dbh))+
  geom_boxplot()+
  xlab("Site")+
  theme(axis.text.x = element_text(angle=90, hjust=1))