
# Vars collected from various locations and grouped together to make file
# with richness, mean dbh, and tree density added



#INPUT GroundCover, access
#     PlotVars, access
#     veg codes, csv file
#     plot_rich, ExploringNests
#     tree_density, dbh_means,  DBHAnalysis
#OUTPUT AllPlotVarsRichness.csv combines PlotVars with tree density, mean dbh and richness from
#       DBH analysis and ExploringNests
library(ggplot2)
library(dplyr)


Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)
# now using Dat_Yr2_veg means analysis is carried out without bryophytes


data_plots = read.csv("../Data/PlotVars.csv") # this provides pH, livebasal area, SOM

#means come from MainProjDBH
data_dbh = read.csv("../Data/dbh_means.csv")

#tree_density calcuated in MainProjDBH, is sum of all trees over all dbh classes/200m2
#BUT - saplings only counted in 2 quadrants

data_tree_density = read.csv("../Data/tree_density.csv")

#plot_richness come from ExploringNests
data_rich = read.csv("../Data/plot_rich.csv")

#wrangle
# get data into long format of columns Site (with 16 plots), pH, SOM, etc


tmp_plot_vars = data_plots[c(1,2,5,7,9,11,12,14)] 


data_dbh_long = data.frame()
Plot = c(1:16)
for (i in 1:103){
  r = as.vector(t(data_dbh[i, c(2:17)]))
  Site = rep(i,16)
  tmp = as.data.frame(cbind(Site,Plot,r))
  colnames(tmp) = c("Site","Plot","mean_dbh")
  data_dbh_long = rbind(data_dbh_long,tmp)
  }



data_density_long = data.frame()
Plot = c(1:16)
for (i in 1:103){
  r = as.vector(t(data_tree_density[i, c(2:17)]))
  Site = rep(i,16)
  tmp = as.data.frame(cbind(Site,Plot,r))
  colnames(tmp) = c("Site","Plot","tree density")
  data_density_long = rbind(data_density_long,tmp)
}

data_rich_long = data.frame()
Plot = c(1:16)
for (i in 1:103){
  r = as.vector(t(data_rich[i, c(2:17)]))
  Site = rep(i,16)
  tmp = as.data.frame(cbind(Site,Plot,r))
  colnames(tmp) = c("Site","Plot","plot_richness")
  data_rich_long = rbind(data_rich_long,tmp)
}

#join tmp plot vars with data dbh long, data density long and data rich long

tmp_all = inner_join(tmp_plot_vars,data_dbh_long)
tmp_all = inner_join(tmp_all, data_density_long)
all_plot_vars = inner_join(tmp_all, data_rich_long)
      
all_plot_vars$ShortNVC = gsub("[[:lower:]]","",all_plot_vars$Yr2NVC)
write.csv(all_plot_vars,"../Data/AllPlotsVarsRichness.csv")

#plot of all NVC codes and richnesses across all woods, so spread is due to wood.

y = all_plot_vars$plot_richness
x1 = as.factor(all_plot_vars$ShortNVC)
x2= as.factor(round(all_plot_vars$pHYr2, digits = 0))
x3 = as.factor(round(all_plot_vars$SOMYr2, digits = 0))
x4 = as.factor(round(all_plot_vars$LiveBasalAreaYr2, digits = 0))
x5 =  as.factor(round(all_plot_vars$mean_dbh, digits = 0))
x6 =  as.factor(round(all_plot_vars$`tree density`, digits = 1))

#png("../Data/Talk/plot_vars.png")
par(mfrow =c(3,2), mai = c(0.2,0.2,0.2,0.2))

plot(x = x1, y = y, main = "by NVC")

plot(x = x2, y = y, main = "by pH")

plot(x = x3, y = y, main = "by SOM")

plot(x = x4, y = y, main = "by LBA")

plot(x=x5, y=y, main = "by mean DBH")

plot(x = x6, y = y, main = "by tree density")
dev.off()


# do linear model of richness against the vars and see which have a correlation
#playing around with one sit first

#colnames(var_matrix) = c("pHYr2" ,"SOMYr2","LiveBasalAreaYr2", "mean_dbh" ,"tree_density","plot_richness" )
#site83 = all_plot_vars%>%filter(Site == 83)
#var_matrix = site83[c(5,6,8,9,10,11,12)]
#var_matrix[var_matrix==0] = NA

#model_lm = lm(plot_richness~ pHYr2, data = var_matrix)


#################################


data = as.data.frame(cbind(as.factor(x2),y))

ggplot(data, aes(x = x2, y = y))+geom_boxplot()+
  xlab("plot pH")+ylab("plot richness")+
  theme(text = element_text(size = 14, face = "bold"))




