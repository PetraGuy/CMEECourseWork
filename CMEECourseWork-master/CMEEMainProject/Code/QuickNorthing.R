
rm(list = ls())
cat("\014")
library(dplyr) # everything

library(ggplot2)
library(gridExtra)

zeta_r = readRDS("Zeta/zeta_r")
# get the data
site_data =  read.csv("../Data/CompleteSiteLevelVars.csv")
site_data = site_data[,-1]




Site = c(1:103)
zeta_r = as.data.frame(cbind(Site,zeta_r))
site_data = inner_join(site_data,zeta_r)

#mean impute the missing PHI
meanPHI = round(mean(site_data$Pos_Hetero_Index, na.rm = TRUE),2)
x = site_data$Pos_Hetero_Index
x[is.na(x)] = meanPHI
site_data$Pos_Hetero_Index = x


subset_all = site_data%>%select("Site","Richness","Area_ha",
                                "Northing", "Pos_Hetero_Index","Buffer3",
                                "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                                "sd_meandbh","sd_treedensity","area_ratio",
                                "meandbh","meanph", "meanSOM","meanLBA",
                                "meantreedensity","zeta_r")



colnames(subset_all) = c("Site","Richness","Area",
                         "Northing", "PHI","Buffer",
                         "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                         "sd_meandbh","sd_TD","area_ratio",
                         "meandbh","meanph", "meanSOM","meanLBA",
                         "meanTD","zeta_r")


largest_area = as.numeric(subset_all%>%filter(Area == max(Area))%>%select(Site))
site_data_outlier = subset_all%>%filter(Site!=largest_area)
site_data_outlier = site_data_outlier[,-3] # remove area column now

g1 = ggplot(site_data_outlier, aes(x=meanTD, y=Northing))+ geom_point()+geom_smooth(method = "lm")+
  xlab("mean tree density")
g2 = ggplot(site_data_outlier, aes(x=Buffer, y=Northing))+ geom_point()+geom_smooth(method = "lm")
g3 =g2 = ggplot(site_data_outlier, aes(x=Buffer, y=Northing))+ geom_point()+geom_smooth(method = "lm")

grid.arrange(g1,g2, ncol = 2)

g1 = ggplot(site_data, aes(x=Buffer1, y=meanSOM))+ geom_point()+geom_smooth(method = "lm")+
  xlab("Buffer")

g2 = ggplot(site_data, aes(x=Buffer1, y=mean_N))+ geom_point()+geom_smooth(method = "lm")+
  xlab("Buffer")

grid.arrange(g1,g2,ncol = 2)
