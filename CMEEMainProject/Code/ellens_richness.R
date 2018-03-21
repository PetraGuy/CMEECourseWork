
# exploring relationship between richness of a site and various env vars
#using d[site]  which is the site richness of the 16 plots, so all equal area.
#need d[site] from ExploringNests saved to SiteRichness.csv
#need AllPlotVarsRichness from CollectingPlotCars this conatains plot level richness, but also range of pH, dbh etc in each site
#the ranges/sd will be extracted to use as heterogeneity of the site
#Need Site descriptor codes 
#Need Site level vars
#need land cover in buffers


#The data files are at different levels - the higher levels (nest/plot) are used to get range/sd of variables
#The ramges/sd will be added as variable to the lowest level (Site) file along with simple site richness
#Simple site richness = unique richness of the 16 plots no extrapolation

library(dplyr)
library(ggplot2)
library(reshape2)
rm(list = ls())
cat("\014")
setwd("~/Documents/CMEECourseWork/CMEEMainProject/Code")

Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")

# the  bryophytes, lichen etc have already been removed from these, 
#because the veg codes csv files is no bryophytes, so need to join this 
#with ground cover to eliminate bryophytes from counts


colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)

#get the ellenbergs file

ellenbergs = read.csv("../Data/Spp_lib.csv")
ellenbergs  = ellenbergs[c(2,3,5,6,7,8)]
colnames(ellenbergs) = c("BRC_Name","BRC_number","R","N","W","L")
veg_ellens = left_join(Data_Yr2_veg,ellenbergs)
#Now have veg_ellens = site/plot/nest species list and ellenbergs - use this extract
#range of ellenbergs for a Site


#The smple  site richness
SiteRichness = read.csv("../Data/SiteRichness.csv")
colnames(SiteRichness) = c("Site","Richness")
#plot level env vars
AllPlotsvars = read.csv("../Data/AllPlotsVarsRichness.csv")

#positive site desc codes
SiteCodes = read.csv("../Data/PositiveHeterogIndices.csv")

#all other site level vars
SiteLevelVars = read.csv("../Data/AnalysisEnvdataLevelSite.csv")

#land cover in buffers
Buffers = read.csv("../Data/LandCoverIndices.csv")
colnames(Buffers) = c("Site","Buffer1","Buffer2","Buffer3")

#Now add all these vars to site level vars data.frame

#Positive codes, richness and buffers can be added on the end

tmp = inner_join(SiteLevelVars,SiteCodes)
tmp = inner_join(tmp,Buffers)
tmp = inner_join(tmp,SiteRichness)

#remove all the site designations excep Any.Ac and num of designations.
tmp = tmp[-c(7:14, 16,17,19)]

#No add the sd/ranges from the plot level variables.
#First get the sd of pH, dbh etc

#missing plots

missing_sites = c( 3,15,17,38,46,46,46,59,59,  63,74,77, 83,90,98,98,98, 59,79,79,91,99,99,99,48,48,29,29,82,77)
missing_plots = c(14, 1,15, 9, 2, 8,16, 1, 8,  14,16, 1, 16, 2,11,12,14,  1, 2, 3,15,10,13,14, 7,10, 2, 1, 7,16)
missing = as.data.frame(cbind(missing_sites,missing_plots))
colnames(missing) = c("Site","Plot")

#remove missing plots
AllPlotsvars = anti_join(AllPlotsvars, missing, by = c("Site","Plot"))
#now if NA implies zero, not missing, so na.omit etc should be OK
#put zeros in dbh etc, because they are for no trees and we want to 
#count that in sd
AllPlotsvars$LiveBasalAreaYr2[is.na(AllPlotsvars$LiveBasalAreaYr2)]=0
AllPlotsvars$mean_dbh[is.na(AllPlotsvars$mean_dbh)]=0
AllPlotsvars$tree.density[is.na(AllPlotsvars$tree.density)]=0

hetero_vars = data.frame()

for (i in 1:103){
  site = AllPlotsvars%>%filter(Site == i)
  no_NVC = length(unique(site$ShortNVC))
  sd_pH = sd(site$pHYr2, na.rm = TRUE)
  sd_SOM = sd(site$SOMYr2,na.rm = TRUE)
  no_MSG = length(unique(site$MSG))
  sd_LBA = sd(site$LiveBasalAreaYr2,na.rm = TRUE)
  sd_meandbh= sd(site$mean_dbh,na.rm = TRUE)
  sd_treedensity= sd(site$tree.density,na.rm = TRUE)
  no_trees = sum(site$LiveBasalAreaYr2==0)
  vect = c(i,no_NVC, sd_pH, sd_SOM, no_MSG, sd_LBA, sd_meandbh, sd_treedensity,no_trees)
  hetero_vars = rbind(hetero_vars,vect)
 }

colnames(hetero_vars) = c("Site","no_NVC", "sd_pH", "sd_SOM", "no_MSG", "sd_LBA",
                          "sd_meandbh", "sd_treedensity","no_trees")


SiteLevelVars = inner_join(tmp,hetero_vars)
#Now completesitelevelvars has the site level variables 
#plus the sd of the plot level vars in each site, plus richness

# now add the sd of ellenbergs and mean of ellenbergs
#for each site the mean will use all values recorded including repeats
#because we want to include the effecs of the different assemblages across the wood.
#eg, if light loving plants in plot 1 and same ones in plot 5, that information
#must be retained.


ellen_vars = data.frame()

for (i in 1:130){
  site = veg_ellens%>%filter(SITE == i)
  sd_R = sd(site$R, na.rm = TRUE)
  mean_R = mean(site$R, na.rm = TRUE)
  sd_N = sd(site$N, na.rm = TRUE)
  mean_N = mean(site$R, na.rm = TRUE)
  sd_W = sd(site$W, na.rm = TRUE)
  mean_W =mean(site$W,na.rm = TRUE)
  sd_L = sd(site$L, na.rm = TRUE)
  mean_L = mean(site$L, na.rm = TRUE)
  vect = c(i,sd_R,mean_R,sd_N,mean_N,sd_W,mean_W,sd_L,mean_L)
  ellen_vars = rbind(ellen_vars,vect)
}
colnames(ellen_vars) = c("Site","sd_R","mean_R","sd_N","mean_N",
                         "sd_W","mean_W","sd_L","mean_L")
CompleteSiteLevelVars = inner_join(SiteLevelVars,ellen_vars)

#going to add mean dbh, mean pH to the complete set. 
#Then there are 3 sets of models, richnes vs
#1 -physical: alt, area/perim/E-N, no_designations, Any Anc, buffers, ave pH,ave dbh, ave live basal
#2 -hetero:number ANC, no +ve site codes, sd_ph, sd_meandbh, sd_LBA, 
#sd_tree density, no_trees(this is num of plots with no trees), num NVC
#3 -ellenergs - plant based traits that should reflect all of above.


means = data.frame()
for (i in 1:103){
  site = AllPlotsvars%>%filter(Site == i)
  meandbh = mean(site$mean_dbh)
  meanph = mean(site$pHYr2,na.rm = TRUE)
  meanSOM = mean(site$SOMYr2,na.rm = TRUE)
  meanLBA = mean(site$LiveBasalAreaYr2)
  tmp = c(i,meandbh,meanph,meanSOM,meanLBA)
  means = rbind(means,tmp)
}
  colnames(means) = c("Site","meandbh","meanph","meanSOM","meanLBA")

CompleteSiteLevelVars = inner_join(CompleteSiteLevelVars,means)

# add area/perim ratio
CompleteSiteLevelVars$area_ratio = CompleteSiteLevelVars$Area_ha*10000/CompleteSiteLevelVars$Perim_m

write.csv(CompleteSiteLevelVars, "../Data/SiteLevelVars.csv")

#################################################################

#####PHYSICL##########
#Now first consider "physical" variables, select subset of CompleteSiteLevelVars

physical_vars = c("Site","Alt_m", "Area_ha","Perim_m" ,"area_ratio", "Easting" , "Northing","Buffer1","Buffer2" ,
                  "Buffer3", "Richness", "meandbh", "meanph" , "meanSOM" ,"meanLBA" )              
physical = CompleteSiteLevelVars%>%select(physical_vars)



#normalise to 1 so you can plot together
#remove site and richness
cut_physical = physical[-c(1,11)]
norm_physical = apply(cut_physical, 2, function(X) X/max(X))
norm_physical = as.data.frame(cbind(physical$Site, physical$Richness,norm_physical))
colnames(norm_physical) = c("Site","Richness","Alt_m","Area_ha","Perim_m","area_ratio",
                          "Easting", "Northing","Buffer1","Buffer2" ,
                           "Buffer3", "meandbh", "meanph" , "meanSOM" ,"meanLBA" ) 

# melt by hand, melt not doing the right thing
norm_physical_cut = norm_physical[,-c(1,2)]
R = norm_physical$Richness
Richness_rep = rep(norm_physical$Richness,13)
melted = melt(norm_physical_cut)
melted_physical = as.data.frame(cbind(Richness_rep,melted))

###select out subsets to plot otherwise cant see anything
set1 = melted_physical%>%filter(variable == "Alt_m" | variable == "Area_ha" |
                                  variable == "Perim_m" | variable == "area_ratio")

#pdf("../Results/set1.pdf")
ggplot(set1, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

set2 = melted_physical%>%filter(variable == "Easting" | variable == "Northing")

#pdf("../Results/set2.pdf")
ggplot(set2, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

set3 = melted_physical%>%filter(variable == "Buffer1" | variable == "Buffer2" | variable == "Buffer3")

#pdf("../Results/set3.pdf")
ggplot(set3, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

set4 = melted_physical%>%filter(variable == "meandbh" | variable == "meanph" | 
                                  variable == "meanSOM" | variable == "meanLBA")
#pdf("../Results/set4.pdf")
ggplot(set4, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))


#####HETERO #####################
#now do heterogeneity variables,
#have dataframe called hetero_vars before joined to richness, but I'll subract from the complete
heterog_vars = c("Site","Richness","no_NVC", "Pos_Hetero_Index", "sd_pH", "sd_SOM", "no_MSG", "sd_LBA",
  "sd_meandbh", "sd_treedensity","no_trees")

heterog = CompleteSiteLevelVars%>%select(heterog_vars)
# replace NA by zero in Pos_Heter_Index column
heterog[is.na(heterog)] = 0
 #reshape

cut_heterog = heterog[-c(1,2)]
norm_heterog = apply(cut_heterog, 2, function(X) X/max(X))
norm_heterog = as.data.frame(cbind(heterog$Site, heterog$Richness,norm_heterog))
colnames(norm_heterog) = c("Site","Richness","no_NVC", "Pos_Hetero_Index", "sd_pH", "sd_SOM", "no_MSG", "sd_LBA",
                            "sd_meandbh", "sd_treedensity","no_trees") 

# melt by hand, melt not doing the right thing
norm_heterog_cut = norm_heterog[,-c(1,2)]
R = norm_heterog$Richness
Richness_rep = rep(norm_heterog$Richness,9)
melted = melt(norm_heterog_cut)
melted_heterog= as.data.frame(cbind(Richness_rep,melted))

set5 =  melted_heterog%>%filter(variable == "no_NVC" | variable == "no_MSG" | variable == "Pos_Hetero_Index" |
                                  variable == "sd_pH" | variable == "sd_SOM" )
#pdf("../Results/set5.pdf")
ggplot(set5, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

set6 = melted_heterog%>%filter(variable == "sd_meandbh" | variable == "sd_treedensity" |
                                 variable == "sd_LBA" | variable == "no_trees")
#pdf("../Results/set6.pdf")
ggplot(set6, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

########## ELLENBERGS #############################

ellenberg_vars = c("Site","Richness","sd_R","mean_R","sd_N","mean_N",
                         "sd_W","mean_W","sd_L","mean_L")

ellenberg = CompleteSiteLevelVars%>%select(ellenberg_vars)
# replace NA by zero in Pos_Heter_Index column
#ellenberg[is.na(ellenberg)] = 0
#reshape

# melt by hand, melt not doing the right thing
ellenberg_cut = ellenberg[,-c(1,2)]
R = ellenberg$Richness
Richness_rep = rep(R,8)
melted = melt(ellenberg_cut)
melted_ellenberg= as.data.frame(cbind(Richness_rep,melted))


set7 =  melted_ellenberg%>%filter(variable == "sd_R" | variable == "sd_N" | 
                                  variable == "sd_W" | variable == "sd_L" )
#pdf("../Results/set7.pdf")
ggplot(set7, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

set8 = melted_ellenberg%>%filter(variable == "mean_R" | variable == "mean_N" |
                                 variable == "mean_W" | variable == "mean_L")
#pdf("../Results/set8.pdf")
ggplot(set8, aes(value,Richness_rep))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))

#### this function does nice pairs plots
#mydata = norm_physical[-c(1,2)]
#my_fn <- function(data, mapping, ...){
#  p <- ggplot(data = data, mapping = mapping) + 
#    geom_point() + 
#    geom_smooth(method=loess, fill="red", color="red", ...) +
#    geom_smooth(method=lm, fill="blue", color="blue", ...)
#  p
#}

#g = ggpairs(mydata,columns = 1:4, lower = list(continuous = my_fn))
#h = ggpairs(mydata,columns = 5:8, lower = list(continuous = my_fn))
#i = ggpairs(mydata,columns = 9:12, lower = list(continuous = my_fn))
###################


