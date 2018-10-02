

#Modelling z site, z plot, richness site, richness plot, random intercepts

#Collect the variables

rm(list = ls())
cat("\014")
library(dplyr)
library(ggplot2)
library(visreg)
library(car) #for vif
library(GGally) # for ggpairs
library(reshape)
library(gridExtra)
setwd("C:/Users/Petra Guy/Google Drive/Imperial/MainProject/R/CMEECourseWork-master/CMEEMainProject/Code")

# zs from fits to av cf curves from SAC
ave_data_fits = read.csv("../Data/z_ave_fits")
#the ave cf calculated from the min and max slope cf in SAC
ave_cf = read.csv("../Data/ave_cf.csv")

#plot vars if I want data at plot level
AllplotVars =  read.csv("../Data/AllPlotsVarsRichness.csv")

#site vars inc richness
AllSiteVars = read.csv("../Data/CompleteSiteLevelVars.csv")
AllSiteVars = AllSiteVars[-1]
ave_data_fits = ave_data_fits[-1]
ave_data_fits = ave_data_fits[-3]
colnames(ave_data_fits) = c( "Site",  "ave_slope" )

#cumulative richness across each plot
plot_cum_richess = readRDS("CumulateveRichness.RDS")

#for the nest modesl - the standard dev of random intercepts
#from NestFixedEffectsFit, remove unwanted cols and rename
nestfits = readRDS("nest_mixed_model_fits.RDS")
nestfits = nestfits[-4]
colnames(nestfits) = c("Site" , "nest_intercept", "nest_slope" ,"sd_intercept")

#look at correlations for plot richness:
#Select useful columns
plotdata = AllplotVars%>%select(pHYr2, SOMYr2, MSG,LiveBasalAreaYr2,
                                mean_dbh,tree.density,ShortNVC,plot_richness)

# ggpairs(plotdata[-7], axisLabels="none",
#         lower = list(continuous="smooth"),
#         diag="blankDiag",
#         upper = list(corSize=2,axisLabels='show'))+
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         axis.ticks = element_blank(),
#         panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))

# make long df
#make NA = 0, remove missing plots and separate NVC codes.
plotdata = plotdata%>%filter(plot_richness !=0)
NVCdata = plotdata%>%select(ShortNVC,plot_richness)
plotdata[is.na(plotdata)] = 0
#richness = plotdata$plot_richness
#plotdata = plotdata[,-7]
# normplotdata = apply(plotdata,2, function(x) {x/max(x)})
# normplotdata = as.data.frame(cbind(normplotdata,richness))

#melted_plotdata = melt(plotdata, id.vars = "plot_richness")
plotdatapH = plotdata%>%select(pHYr2,plot_richness)%>%filter(pHYr2 !=0)
  g1 = ggplot(plotdatapH, aes(x = pHYr2, y = plot_richness))+
  geom_point()+
    geom_smooth(method = loess)
  
plotdataSOM = plotdata%>%select(SOMYr2,plot_richness)%>%filter(SOMYr2 !=0)
   g2 = ggplot(plotdataSOM, aes(x = SOMYr2, y = plot_richness))+
    geom_point()+
     geom_smooth(method = loess)

    g3 = ggplot(plotdata, aes(x = as.factor(MSG), y = plot_richness))+
    geom_boxplot()
 
   
  g4 = ggplot(plotdata, aes(x = LiveBasalAreaYr2, y = plot_richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  g5 = ggplot(plotdata, aes(x = mean_dbh, y = plot_richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  g6 = ggplot(plotdata, aes(x = tree.density, y = plot_richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  g7 = ggplot(plotdata, aes(x = as.factor(ShortNVC), y = plot_richness))+
    geom_boxplot()


  ggplot(plotdatapH, aes(x = cut(pHYr2, breaks = 10), y = plot_richness))+
    geom_boxplot()
    
  ggplot(plotdataSOM, aes(x = cut(SOMYr2, breaks = 10), y = plot_richness))+
    geom_boxplot()
  
  ggplot(plotdata, aes(x = cut(LiveBasalAreaYr2, breaks = 10), y = plot_richness))+
    geom_boxplot()
  
  
  #########################
  #Richness ~ SiteVars
  
  AllSiteVarsRed = AllSiteVars[-c(1,2,3,4,5,6,7,8,10,11,22,23,24,25,26,27,28,29)]
  outliers = c(78,97)
 
  sitedataPHI = AllSiteVarsRed%>%select(Pos_Hetero_Index,Richness)
  sitevarsPHI = AllSiteVarsRed%>%filter(Pos_Hetero_Index != outliers)

  
  ggplot(sitevarsPHI, aes(x = Pos_Hetero_Index, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = Buffer3, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = no_NVC, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = sd_pH, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = sd_SOM, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = sd_LBA, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = sd_meandbh, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = sd_treedensity, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = no_trees, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = meandbh, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = meanph, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = meanSOM, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = meanLBA, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = meantreedensity, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(AllSiteVarsRed, aes(x = area_ratio, y = Richness))+
    geom_point()+
    geom_smooth(method = loess)
  
  
  
  ########################
  
  # taking the slope from the min/max fits and plotting with variables.
  #not sure what I was trying to do here
  
  SiteVarsSlopes = inner_join(AllSiteVars,ave_data_fits)
  SiteVarsSlopesRed = SiteVarsSlopes[-c(1,2,3,4,5,6,7,8,10,11,22,23,24,25,26,27,28,29)]
  outliers = c(78,97)
  
  sitedataSlopePHI = SiteVarsSlopesRed%>%select(Pos_Hetero_Index,ave_slope)
  sitedataSlopePHI = sitedataSlopePHI%>%filter(Pos_Hetero_Index != outliers)
  
  ggplot(sitedataSlopePHI, aes(x = Pos_Hetero_Index, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = Buffer3, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = no_NVC, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = sd_pH, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = sd_SOM, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = sd_LBA, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = sd_meandbh, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = sd_treedensity, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = no_trees, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meandbh, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanph, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanSOM, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanLBA, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meantreedensity, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = area_ratio, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  
  ###################################
  
  
# z from lme z of intercepts
  
  sitevars_sd = inner_join(AllSiteVars,nestfits)
  sitevars_sdRed = sitevars_sd[-c(1,2,3,4,5,6,7,8,10,11,22,23,24,25,26,27,28,29)]
 
  outliers = c(78,97)
  
  sitedatasdPHI = sitevars_sdRed%>%select(Pos_Hetero_Index,nest_slope)
  sitedatasdPHI = sitedatasdPHI%>%filter(Pos_Hetero_Index != outliers)
  
  ggplot(sitedatasdPHI, aes(x = Pos_Hetero_Index, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = Buffer3, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = no_NVC, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = sd_pH, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = sd_SOM, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = sd_LBA, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = sd_meandbh, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = sd_treedensity, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = no_trees, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(sitevars_sdRed, aes(x = meandbh, y = nest_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanph, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanSOM, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meanLBA, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = meantreedensity, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  
  ggplot(SiteVarsSlopesRed, aes(x = area_ratio, y = ave_slope))+
    geom_point()+
    geom_smooth(method = loess)
  