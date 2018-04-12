###Try to fit the z's from the ave path to some variables
setwd("~/Documents/CMEECourseWork/CMEEMainProject/Code")
rm(list = ls())
cat("\014")
library(dplyr)
library(ggplot2)
library(visreg)


path_min = readRDS("path_min.RDS")
path_max = readRDS("path_max.RDS")
ave_data_fits = read.csv("../Data/z_ave_fits")
ave_cf = read.csv("../Data/ave_cf.csv")
AllplotVars =  read.csv("../Data/AllPlotsVarsRichness.csv")
AllSiteVars = read.csv("../Data/CompleteSiteLevelVars.csv")
AllSiteVars = AllSiteVars[-1]
ave_data_fits = ave_data_fits[-1]
plot_cum_richess = readRDS("CumulateveRichness.RDS")
#############No - not this #############
# ave_path_vars = list()
# for (i in 1:103){
#   site_var_path = data.frame()
#   ave_cf_site = ave_cf%>%filter(Site == i)
#   vars = c("Site", "Plot","SOMYr2","mean_dbh","tree.density")
#   site =  AllplotVars%>%filter(Site == i)%>%select(vars)
#   max_path = path_max[[i]]
#   min_path = path_min[[i]]
#   lmax = length(max_path) # this will always have fewer plots
#   min_path = min_path[1:lmax]  
#   area = 200# now will just use these pair of variables for the fits
#   for (j in 1:lmax) {
#     max = max_path[j]
#     min = min_path[j]
#     SOM = (site$SOMYr2[max]+site$SOMYr2[min])/2
#     dbh = (site$mean_dbh[max]+site$mean_dbh[min])/2
#     TD = (site$tree.density[max]+site$tree.density[min])/2
#     rich = ave_cf_site$ave_cf[j]
#     step = j
#     this_row = c(step, area, SOM, dbh, TD,rich,min,max)
#     site_var_path = rbind(site_var_path, this_row)
#     area = area + 200
#   }
#   colnames(site_var_path) = c("step","area","SOM","dbh","TD","ave_cf_rich","plotmin","plotmax")
#   ave_path_vars[[i]] = site_var_path
# }
##################################

# Use the average z's and plot them against values for each site.

#add slope to the df of site vars and remodel against slope instead of richness

AllSiteVarsSlope = inner_join(AllSiteVars,ave_data_fits)
colnames(AllSiteVarsSlope)
#remove a few variables
vars_slope_model = c("Northing","Pos_Hetero_Index","Buffer3","no_NVC","sd_SOM","sd_pH",
                     "sd_meandbh","no_trees",
                     "sd_treedensity","meandbh","meanph","meanSOM",
                     "meantreedensity","sd_pH","area_ratio","slope")
vars_richness_model = c("Northing","Pos_Hetero_Index","Buffer3","no_NVC",
                        "sd_SOM","sd_pH","sd_meandbh","no_trees",
                        "sd_treedensity","meandbh","meanph","meanSOM",
                        "meantreedensity","sd_pH","area_ratio","Richness")
SlopeVars = AllSiteVarsSlope%>%select(vars_slope_model)
RichnessVars = AllSiteVars%>%select(vars_richness_model)

model_all_slope = lm(slope ~., data = SlopeVars)
model_all_richness = lm(Richness~.,data = RichnessVars ) 

sum_slope = summary(model_all_slope)
sum_richness = summary(model_all_richness)

coef_slope = round(sum_slope$coefficients[,4],digits = 2)
coef_richness = round(sum_richness$coefficients[,4], digits = 2)

coefs1 = cbind(coef_slope,coef_richness)
colnames(coefs1) = c("slope model","richness model")

par(mfrow = c(4,4))
visreg(model_all_slope, main="slope fit")
par(mfrow = c(4,4))
visreg(model_all_richness, main = "richness fit")

#########################################################################

vars_slope_model = c("Pos_Hetero_Index",
                     "no_NVC","sd_SOM","sd_pH","sd_meandbh","no_trees",
                     "sd_treedensity","meandbh","meanph","meanSOM",
                     "meantreedensity","sd_pH","slope")
vars_richness_model = c("Northing","Pos_Hetero_Index","Buffer3","no_NVC",
                        "sd_SOM","sd_pH","sd_meandbh",
                        "sd_treedensity","meandbh","meanSOM",
                        "sd_pH","area_ratio","Richness")
SlopeVars = AllSiteVarsSlope%>%select(vars_slope_model)
RichnessVars = AllSiteVars%>%select(vars_richness_model)

model_all_slope = lm(slope ~., data = SlopeVars)
model_all_richness = lm(Richness~.,data = RichnessVars ) 

sum_slope = summary(model_all_slope)
sum_richness = summary(model_all_richness)

p_slope = round(sum_slope$coefficients[,4],digits = 2)
p_richness = round(sum_richness$coefficients[,4], digits = 2)
tmp =  colnames(vars_slope_model[-13])
p_slope = as.data.frame(cbind(tmp,p_slope))
tmp = colnames(vars_richness_model[-13])
p_richness = as.data.frame(cbind(tmp,p_richness))
ps = as.data.frame(cbind(p_slope,p_richness))


par(mfrow = c(4,4))
visreg(model_all_slope, main="slope ,it")
par(mfrow = c(4,4))
visreg(model_all_richness, main = "richness fit")

########################################


vars_slope_model = c("Pos_Hetero_Index",
                     "sd_meandbh","no_trees",
                     "sd_treedensity","meandbh","meanSOM",
                     "meantreedensity","slope")
vars_richness_model = c("Northing","Pos_Hetero_Index","Buffer3","no_NVC",
                        "sd_meandbh","meandbh","meanSOM",
                       "Richness")
SlopeVars = AllSiteVarsSlope%>%select(vars_slope_model)
RichnessVars = AllSiteVars%>%select(vars_richness_model)

model_all_slope = lm(slope ~., data = SlopeVars)
model_all_richness = lm(Richness~.,data = RichnessVars ) 

sum_slope = summary(model_all_slope)
sum_richness = summary(model_all_richness)

p_slope = round(sum_slope$coefficients[,4],digits = 2)
p_richness = round(sum_richness$coefficients[,4], digits = 2)
tmp =  colnames(vars_slope_model[-13])
p_slope = as.data.frame(cbind(tmp,p_slope))
tmp = colnames(vars_richness_model[-13])
p_richness = as.data.frame(cbind(tmp,p_richness))
ps = as.data.frame(cbind(p_slope,p_richness))


par(mfrow = c(4,4))
visreg(model_all_slope, main="slope ,it")
par(mfrow = c(4,4))
visreg(model_all_richness, main = "richness fit")

########################################
















