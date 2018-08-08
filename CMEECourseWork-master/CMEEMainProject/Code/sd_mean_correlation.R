

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





#Aside...

(I cant think why these correlations are so strong?? But you can see them (sort of) in this plot of SOM in each site - I tried to order by median, not sure why only some of them have done that, the rest just plonked themselves on the end.
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
  
  ggplot(AllPlotVars, aes(x = reorder(as.factor(Site), SOMYr2, median), y = SOMYr2))+
    geom_boxplot()+
    xlab("Site")+
    theme(axis.text.x = element_text(angle=90, hjust=1))
  ```
  ```{r}
  g1 = ggplot(AllSiteVars, aes(x = meanSOM, y = sd_SOM))+
    geom_point()+
    geom_smooth(method = loess)
  
  g2 = ggplot(AllSiteVars, aes(x = meandbh, y = sd_meandbh))+
    geom_point()+
    geom_smooth(method = loess)
  g3 = ggplot(AllSiteVars, aes(x = meanph, y = sd_pH))+
    geom_point()+
    geom_smooth(method = loess)
  
  grid.arrange(g1,g2,g3)
  ```