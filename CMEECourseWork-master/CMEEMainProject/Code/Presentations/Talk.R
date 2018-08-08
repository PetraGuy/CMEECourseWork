####Nest level fits##

setwd("~/Documents/CMEECourseWork/CMEEMainProject/Code")
rm(list = ls())
cat("\014")
library(reshape)
library(nlme)

ave_data_fits = read.csv("../Data/z_ave_fits")
ave_cf = read.csv("../Data/ave_cf.csv")
AllplotVars =  read.csv("../Data/AllPlotsVarsRichness.csv")
AllSiteVars = read.csv("../Data/CompleteSiteLevelVars.csv")
AllSiteVars = AllSiteVars[-1]
ave_data_fits = ave_data_fits[-1]
plot_cum_richess = readRDS("CumulateveRichness.RDS")
plot_cum_richess = readRDS("CumulateveRichness.RDS") # melt this for the nlme
sitevars = read.csv("../Data/CompleteSiteLevelVars.csv")
plotvars = read.csv("../Data/AllPlotsVarsRichness.csv")
sac_max = readRDS("sac_max.RDS")
nest_fit_coefs = readRDS("nest_mixed_model_fits.RDS")


#this wont plot sites with missing plots because y = predct < length x = log(sarea), fix it later.
title = paste("Site", i)
ggplot(melted_cf)+
  geom_point(aes(x=log(area),y=log(value),na.rm =TRUE), colour = "black") +
  geom_line(aes(x=log(area), y = predict(model), group = plot, colour = plot))+
  xlim(lower = 0, upper = 6)+
  ylim(lower = -1, upper = 5)+
  ggtitle(title)


#find some woods that are beech, alder etc 
#sites 81, 7nvc,52 2nvc

#the plots show heterogeneity via deviation of random intercepts











