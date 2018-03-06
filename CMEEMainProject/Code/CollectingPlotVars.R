

#open all required files

##run ExploringNests.Rmd to caluculate rchnesses - but plot_rich.csv exists

Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)
# now using Dat_Yr2_veg means analysis is carried out without bryophytes


data_plots = read.csv("../Data/AnalysisEnvDataLevelPlot.csv") # this provides pH, livebasal area, SOM

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

all_plot_vars = data.frame()
all_plot_vars = inner_join(all_plot_vars,data_rich_long)


#Now we have some plot level vars in long format

                           
