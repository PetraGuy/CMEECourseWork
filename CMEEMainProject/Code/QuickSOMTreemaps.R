library(RColorBrewer)
library(treemap)
library(gridExtra)
library(grid)

site_data =  read.csv("../Data/CompleteSiteLevelVars.csv")
AllPlotsvars = read.csv("../Data/AllPlotsVarsRichness.csv")
site76 = AllPlotsvars%>%filter(Site == 76)
site74 = AllPlotsvars%>%filter(Site == 74)
site102 = AllPlotsvars%>%filter(Site == 102)
site60 = AllPlotsvars%>%filter(Site == 60)
site73 = AllPlotsvars%>%filter(Site == 73)



mapplot = function(site){
fNVC = as.data.frame(table(site$ShortNVC))
fNVC = fNVC[order(fNVC$Freq),]
colnames(fNVC) = c("ShortNVC","frequency")
meanSOM = site%>%group_by(ShortNVC)%>%summarise(meanSOM = round(mean(SOMYr2, na.rm = TRUE),1))


NVC_SOM = inner_join(fNVC,meanSOM)   

title = c(site)
tm1 <- treemap(
  NVC_SOM,
  index="ShortNVC",
  vSize="frequency",
  vColor="meanSOM",
  type="value",
  title.legend = "SOM content",
  title = "Frequency of the different NVC codes and their mean SOM content",
  palette = "Blues",
  range = c(0,100)
)

}

plot76 = mapplot(site76)
plot74 = mapplot(site74)
plot102 = mapplot(site102)
plot60 = mapplot(site60)


