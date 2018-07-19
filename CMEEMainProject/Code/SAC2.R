#SAC - second version. Here I will add values to end of shortest average.
#otherwise there are only as many value as the shorted cf vector.
#Plus will store plot numbers






# Calculating SAC using maxmum and Minimum slope methods
#INPUTS plot_rich.csv (ExploringNests)
#       GroundCover.csv, access
#       VegCodes.csv, csv file
#OUTPUTS creates long_sac_max and min - a long df of cf obtained
#     by maximum and minimum paths through the plots. The values are extrapolated to 
#     3200m2 using linear extrap. The average is then fixed so that it
#     it also finishes on max_cf value. Paths area stored in path_max[[site]]
#     and path_min[[site]]. The plot numbers in these can be used to access
#     the variables for each plot through the path.
#     path_min, path_max RDS, for the plot order, ave_cf.csv for the richness on the ave fit 
#     and ave_fits.csv for the slopse


rm(list = ls())
cat("\014")
library(glmm)
library(dplyr)
library(ggplot2)
library(nlme)
library(gridExtra)
library(reshape)
library(MuMIn)
#plot level SAC


plotrichness = read.csv("../Data/plot_rich.csv")
#plot richness includes the missing plots as 0's, col 1 in random row numbers
plotrichness = plotrichness[, -1]

#Input all the ground flora data
Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data %>% filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT", "NEST", "Cover", "BRC_number", "Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")

# the  bryophytes, lichen etc have already been removed from veg codes csv so join this with ground cover to eliminate bryophytes from counts


colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2 %>% inner_join(veg_codes)
# now using Dat_Yr2_veg means analysis is carried out without bryophytes


# calculate SAC starting from least rich plot
#select plot with least amount of species
#remove missing plots from Data_Yr2_veg

sac_max = list()
path_max = list()

for (i in 1:103){
  path = vector()
  f = vector()
  plotvect = c(1:16)
  sitevector = plotrichness[i,] # vector of richnesses of each plot for site i
  # remove zero values of missing plots
  #sitevector = sitevector[sitevector != 0]
  sitevector[sitevector == 0] = NA
  start = which.min(sitevector) # take plot with min richness
  path[1] = start
  siteveg = Data_Yr2_veg %>% filter(SITE == i) #get species data for site i
  startplot = siteveg %>% filter(PLOT == start) # get species data for min plot
  f[1] = length(unique(startplot$Species)) # first frequency  is just richness of min plot
  restofplots = siteveg %>% filter(PLOT != start) # take out the min plot
  plotvect = plotvect[-start] # take out the plot number of min plot
  j = 1
  while (length(plotvect != 0)) {
    #browser()
    j = j + 1 # counter for position in f vector
    #inner joins of all other plots to min plot, tmp 2 lists all species in
    #next plot that were not in min plot.
    tmp2 = restofplots %>% group_by(PLOT) %>% anti_join(., startplot, by = "Species")
    if (length(tmp2$SITE) == 0) {
      break
    } else {
      # this just gives the numbers of new species
      nos = as.data.frame(tmp2 %>% group_by(PLOT) %>% summarise(length(unique(Species))))
      #find location i.e.plot numer of biggest new no. of species
      position = which.max(nos[, 2])
      nextplot = nos[, 1][position]
      path= c(path,nextplot)
      # make the next frequency the biggest jump
      f[j] = max(nos[, 2]) #next f along, the extra species only
      #remove that plot number
      plotvect = plotvect[which(plotvect != nextplot)]
      # remove that plot species list
      restofplots = filter(siteveg, PLOT %in% plotvect)
      nextplotveg = siteveg %>% filter(PLOT == nextplot)
      # add the species for the removed plot to the start plot so only additional
      #new species are counted
      startplot = rbind(startplot, nextplotveg)
    }
  }
  path_max[[i]] = path
  sac_max[[i]] = cumsum(f)
}
saveRDS(sac_max,"sac_max.RDS")

# We have the cumsums in sac_max for the maximum gradient.
# Repeat for the minimum gradient

sac_min = list()
path_min = list()

for (i in 1:103){
  path = vector()
  f = vector()
  plotvect = c(1:16)
  sitevector = plotrichness[i,] # vector of richnesses of each plot for site i
  # remove zero values of missing plots
  #make NA to keep plot position
  #sitevector = sitevector[sitevector != 0]
  sitevector[sitevector == 0] = NA
  start = which.max(sitevector) # take plot with max richness
  path[1] = start
  siteveg = Data_Yr2_veg %>% filter(SITE == i) #get species data for site i
  startplot = siteveg %>% filter(PLOT == start) # get species data for min plot
  f[1] = length(unique(startplot$Species)) # first frequency  is just richness of min plot
  restofplots = siteveg %>% filter(PLOT != start) # take out the min plot
  plotvect = plotvect[-start] # take out the plot number of min plot
  j = 1
  while (length(plotvect != 0)) {
    #browser()
    j = j + 1 # counter for position in f vector
    #inner joins of all other plots to min plot, tmp 2 lists all species in
    #next plot that were not in min plot.
    tmp2 = restofplots %>% group_by(PLOT) %>% anti_join(., startplot, by = "Species")
    if (length(tmp2$SITE) == 0) {
      break
    } else {
      # this just gives the numbers of new species
      nos = as.data.frame(tmp2 %>% group_by(PLOT) %>% summarise(length(unique(Species))))
      #find location i.e.plot numer of biggest new no. of species
      position = which.min(nos[, 2])
      nextplot = nos[, 1][position]
      path = c(path,nextplot)
      # make the next frequency the biggest jump
      f[j] = min(nos[, 2]) #next f along, the extra species only
      #remove that plot number
      plotvect = plotvect[which(plotvect != nextplot)]
      # remove that plot species list
      restofplots = filter(siteveg, PLOT %in% plotvect)
      nextplotveg = siteveg %>% filter(PLOT == nextplot)
      # add the species for the removed plot to the start plot so only additional
      #new species are counted
      startplot = rbind(startplot, nextplotveg)
    }
  }
  path_min[[i]] = path
  sac_min[[i]] = cumsum(f)
}


areas = seq(from = 200, to = 3200, by = 200)

# make a long df from the min cumsums, but continue to area 3200

long_sac_max = data.frame()

for (i in 1:103){
  y = as.numeric(sac_max[[i]])
  l = length(y)
  diff = 16 - l
  grad = y[l] - y[(l-1)]
  final_val = y[l]
  pad_cf = vector()
  for (j in 1:diff){
    final_val = final_val + grad
    pad_cf[j] = final_val
  }
  y = c(y,pad_cf)
  sitecol =  rep(i,16)
  cols = as.data.frame(cbind(sitecol,areas,y))
  colnames(cols) = c("Site","area","max_cf")
  long_sac_max = rbind(long_sac_max,cols)
}


long_sac_min = data.frame()

for (i in 1:103){
  y = as.numeric(sac_min[[i]])
  l = length(y)
  cfmax = long_sac_max%>%filter(Site == i)
  final_valmin = y[l]
  final_valmax = cfmax$max_cf[16]
  diff = 16 - l
  sitecol =  rep(i,16)
  if (diff > 0) {
  grad = (final_valmax - final_valmin)/diff
  pad_cf = vector()
  for (j in 1:diff){
    final_valmin = final_valmin + grad
    pad_cf[j] = final_valmin
  }
  y = c(y,pad_cf)
  }
  cols = as.data.frame(cbind(sitecol,areas,y))
  colnames(cols) = c("Site","area","min_cf")
  long_sac_min = rbind(long_sac_min,cols)
  
}


##########AVERAGE############################################


ave_data = data.frame()
for (i in 1:103){
  site_max = long_sac_max%>%filter(Site == i)
  site_min = long_sac_min%>%filter(Site == i)
  x1 = site_max$max_cf
  x2 = site_min$min_cf
  ave_cf= (x1 + x2)/2 # calculate average
  Site = rep(i,16)
  #make a dataframe
  aves = as.data.frame(cbind(Site,areas,ave_cf))
  ave_data = rbind(ave_data,aves)
}
write.csv(ave_data,"../Data/ave_cf.csv")

#######################################################

#look at some plots

sac_plot =  function(sitenumber){
  min = long_sac_min%>%filter(Site == sitenumber)
  max = long_sac_max%>%filter(Site == sitenumber)
  ave = ave_data%>%filter(Site == sitenumber)
  cf_min = min$min_cf
  cf_max = max$max_cf
  cf_ave = ave$ave_cf
  data = as.data.frame(cbind(areas,cf_min,cf_ave,cf_max))
  melted = melt(data = data, id.vars = "areas")
  title = paste("Site", sitenumber)
  ggplot(melted, aes(x = areas, y = value, colour = variable))+
    geom_point(size = 5)+
    labs(title = title)
}

#################################

#get the z's for these averages

ave_data_fits = data.frame()
for(i in 1:103){
  #browser()
  site = ave_data%>%filter(Site == i)
  site$logarea = log(site$areas)
  site$logave_cf = log(site$ave_cf)
  model_ave = lm(site$logave_cf ~ site$logarea)
  s = summary(model_ave)
  slope = model_ave$coefficients[2]
  r2 = s$r.squared
  this_row = as.data.frame(cbind(i,slope,r2))
  ave_data_fits= rbind(ave_data_fits, this_row)
}
colnames(ave_data_fits) = c("Site","slope","R2")
row.names(ave_data_fits) = c(1:103)

###Some plots

avehist = ggplot(ave_data_fits, aes(slope))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of slopes in average of two methods")
aveR2 = ggplot(ave_data_fits, aes(R2))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of R2 in average of two methods")

######
#ave_data_fits now has the slopes for each site. So I can now model these

write.csv(ave_data_fits, "../Data/z_ave_fits.csv")
saveRDS(path_max, "path_max.RDS")
saveRDS(path_min,"path_min.RDS")

#########glmm of the average fits###


model = lme(log(ave_cf)~log(areas),random = ~1|Site, data = ave_data, na.action = na.omit)
fit = predict(model)
ave_data$fit = exp(fit)


model_glmm = lmer(log(ave_cf)~log(areas)+(1|Site), data = ave_data, na.action = na.omit)

fit_glmm = predict(model)
ave_data$fit_glmm = exp(fit_glmm)

model_glmm2 = glmer(log(ave_cf)~log(areas)+(1|Site), data = ave_data, 
                    family = "inverse",na.action = na.omit)
fit_glmm = predict(model)

ave_data$fit_glmm = exp(fit_glmm)

model_glmm3 = glmer(ave_cf~log(areas)+(1|Site), data = ave_data, 
                    family = gaussian(link ="log"),na.action = na.omit)
fit_glmm3 = predict(model_glmm3)
r.squaredGLMM(model_glmm3)


model_glmm4 = glmer(log(ave_cf)~log(areas)+(1|Site), data = ave_data, 
                    family = gaussian,na.action = na.omit)
fit_glmm4 = predict(model_glmm4)


fit_glmm = predict(model)
ave_data$fit_glmm = exp(fit_glmm)


#model plot for best wood
ggplot(ave_data)+
  geom_point(aes(x=log(areas),y=log(ave_cf),na.rm =TRUE), colour = "black") +
  geom_line(aes(x=log(areas), y = log(fit), group = Site), colour = "red")

  
