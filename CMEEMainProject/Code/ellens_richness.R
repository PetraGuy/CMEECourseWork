
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
