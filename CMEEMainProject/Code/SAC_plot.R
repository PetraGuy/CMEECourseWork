rm(list = ls())
cat("\014")
library(dplyr)
library(ggplot2)
library(nlme)
library(gridExtra)
#plot level SAC


plotrichness = read.csv("../Data/plot_rich.csv")
#plot richness includes the missing plots as 0's
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


missing_sites = c(15, 17, 38, 46, 46, 46, 59, 59, 63,
                  74, 77, 83, 90, 98, 98, 98, 59, 79,
                  79, 91, 99, 99, 99, 48, 48, 29, 29,
                  82, 77)
missing_plots = c(1,15, 9, 2, 8, 16, 1, 8, 14, 16, 1,
                  16, 2,11,12,14,1, 2, 3, 15, 10, 13,
                  14, 7, 10, 2, 1, 7, 16)
missing = as.data.frame(cbind(missing_sites, missing_plots))
colnames(missing) = c("Site", "Plot")

# calculate SAC starting from least rich plot
#select plot with least amount of species
#remove missing plots from Data_Yr2_veg



sac_max = list()

for (i in 1:103){
  f = vector()
  plotvect = c(1:16)
  sitevector = plotrichness[i,] # vector of richnesses of each plot for site i
  # remove zero values of missing plots
  #sitevector = sitevector[sitevector != 0]
  sitevector[sitevector == 0] = NA
  start = which.min(sitevector) # take plot with min richness
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
  sac_max[[i]] = cumsum(f)
}
  

# We have the cumsums in sac_max for the maximum gradient.
# Repeat for the minimum gradient

sac_min = list()

for (i in 1:103){
  f = vector()
  plotvect = c(1:16)
  sitevector = plotrichness[i,] # vector of richnesses of each plot for site i
  # remove zero values of missing plots
  #make NA to keep plot position
  #sitevector = sitevector[sitevector != 0]
  sitevector[sitevector == 0] = NA
  start = which.max(sitevector) # take plot with max richness
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
  sac_min[[i]] = cumsum(f)
}


areas = seq(from = 200, to = 3200, by = 200)

# make a long df from the min cumsums
long_sac_min = data.frame()

for (i in 1:103){
  y = as.numeric(sac_min[[i]])
  l = length(y)
  x = as.numeric(areas[1:l])
  sitecol =  rep(i,l)
  cols = as.data.frame(cbind(sitecol,x,y))
  colnames(cols) = c("Site","area","min_cf")
  long_sac_min = rbind(long_sac_min,cols)
}


#make a long df from the max cumsums
  
long_sac_max = data.frame()

for (i in 1:103){
  y = as.numeric(sac_max[[i]])
  l = length(y)
  x = as.numeric(areas[1:l])
  sitecol =  rep(i,l)
  cols = as.data.frame(cbind(sitecol,x,y))
  colnames(cols) = c("Site","area","max_cf")
  long_sac_max = rbind(long_sac_max,cols)
}

#Now have long df of cf for each site by max and min method.
#Want to create an average of the two

######################################################
#For example of the issue, consider site 1

site1_max = long_sac_max%>%filter(Site == 1)
site1_min = long_sac_min%>%filter(Site == 1)

longest =  vector()
shortest = vector()

x1 = site1_max$max_cf
x2 = site1_min$min_cf

if (length(x1)>length(x2)){
  longest = x1
  shortest = x2
}else {
  longest = x2
  shortest = x1
}

diff = length(longest)  - length(shortest)
longest_cut = longest[1:length(shortest)] # take off the end cf's on longest

ave = (shortest + longest_cut)/2 # calcualte average



if (diff > 1){
index_replace = c((length(shortest)+1) : (length(shortest)+diff))
}else{
  index_replace = (length(shortest)+1)
}

replace = longest[index_replace] # now add the cut of bits back on
ave_full = c(ave,replace)

#make new dataframe of average values and areas, add log columns

ave_data =  as.data.frame(cbind(areas,ave_full))
ave_data$logarea = log(ave_data$areas)
ave_data$logavecf = log(ave_data$ave_full)

#check out the log/log for the max and min data
site1_max$logarea = log(site1_max$area)
site1_max$logcf = log(site1_max$max_cf)

site1_min$logarea = log(site1_min$area)
site1_min$logcf = log(site1_min$min_cf)

allmethods = ggplot()+
  geom_point(data = site1_max, aes(x = area, y = max_cf), color="red") +
  geom_point(data = site1_min, aes(x = area, y = min_cf),color="blue")+
  geom_point(data = ave_data, aes(x = areas, y = ave_full),color="black")+
  labs(title = "min, max and average cf methods")


avefit = ggplot(data = ave_data, aes(x = logarea, y = logavecf))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "average")

maxfit = ggplot(data = site1_max, aes(x = logarea, y = logcf))+
  geom_point()+
  geom_smooth(method = "lm")+
labs(title = "max method")

minfit = ggplot(data = site1_min, aes(x = logarea, y = logcf))+
  geom_point()+
  geom_smooth(method = "lm")+
 labs(title = "min method")

pdf("../Results/Site1SAC.pdf")
grid.arrange(allmethods,avefit,maxfit,minfit, nrow = 2)
dev.off()
#####################################################

## run a linear model for every wood using min/max and average and see what's what.
# first create the average dataframe

ave_data = data.frame()

for (i in 1:103){
  
  site_max = long_sac_max%>%filter(Site == i)
  site_min = long_sac_min%>%filter(Site == i)
  
  longest =  vector()
  shortest = vector()
  
  x1 = site_max$max_cf
  x2 = site_min$min_cf
  
  if (length(x1)>length(x2)){
    longest = x1
    shortest = x2
  }else {
    longest = x2
    shortest = x1
  }
  
  diff = length(longest)  - length(shortest)
  longest_cut = longest[1:length(shortest)] # take off the end cf's on longest
  
  ave = (shortest + longest_cut)/2 # calcualte average
  
  
  
  if (diff > 1){
    index_replace = c((length(shortest)+1) : (length(shortest)+diff))
  }else{
    index_replace = (length(shortest)+1)
  }
  
  replace = longest[index_replace] # now add the cut of bits back on
  ave_full = c(ave,replace)
  
  areas = seq(from = 200, to = 3200, by = 200) # make the correct length
  areas_cut = areas[1:length(ave_full)]
  Site = rep(i,length(ave_full))
  #make a dataframe
  aves=  as.data.frame(cbind(Site,areas_cut,ave_full))
  ave_data = rbind(ave_data,aves)
}

#######################################################



#Now look at R2 etc for the min/max and average

ave_data$logarea = log(ave_data$area)
ave_data$logave = log(ave_data$ave_full)

ave_data_fits = data.frame()
for(i in 1:103){
  #browser()
  site = ave_data%>%filter(Site == i)
  model_ave = lm(site$logave ~ site$logarea)
  s = summary(model_ave)
  slope = model_ave$coefficients[2]
  r2 = s$r.squared
  this_row = as.data.frame(cbind(i,slope,r2))
  ave_data_fits= rbind(ave_data_fits, this_row)
}
colnames(ave_data_fits) = c("Site","slope","R2")
row.names(ave_data_fits) = c(1:103)
##########################################


long_sac_max$logarea = log(long_sac_max$area)
long_sac_max$log_cf_max = log(long_sac_max$max_cf)

max_data_fits = data.frame()
for(i in 1:103){
  site = long_sac_max%>%filter(Site ==i)
  model_max= lm(site$log_cf_max ~ site$logarea)
  s = summary(model_max)
  slope = model_max$coefficients[2]
  r2 = s$r.squared
  this_row = as.data.frame(cbind(i,slope,r2))
  max_data_fits = rbind(max_data_fits, this_row)
}
colnames(max_data_fits) = c("Site","slope","R2")
row.names(max_data_fits) = c(1:103)
#######################################

long_sac_min$logarea = log(long_sac_min$area)
long_sac_min$log_cf_min = log(long_sac_min$min_cf)

min_data_fits = data.frame()
for(i in 1:103){
  site = long_sac_min%>%filter(Site ==i)
  model_min= lm(site$log_cf_min ~ site$logarea)
  s = summary(model_min)
  slope = model_min$coefficients[2]
  r2 = s$r.squared
  this_row = as.data.frame(cbind(i,slope,r2))
  min_data_fits = rbind(min_data_fits, this_row)
}
colnames(min_data_fits) = c("Site","slope","R2")
row.names(min_data_fits) = c(1:103)

#########################################

#look at slopes and r2 from the three dataframes

avehist = ggplot(ave_data_fits, aes(slope))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of slopes in average of two methods")
aveR2 = ggplot(ave_data_fits, aes(R2))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of R2 in average of two methods")

minhist = ggplot(min_data_fits, aes(slope))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of slopes from minimum method")
minR2 = ggplot(min_data_fits, aes(R2))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of R2 using minimum method")

maxhist =ggplot(max_data_fits, aes(slope))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of slopes from maximum method")
maxR2=ggplot(max_data_fits, aes(R2))+
  geom_histogram(binwidth = 0.05, col = "black", fill = "grey")+
  ggtitle("Distribution of R2 using maximum method")

pdf("../Results/AcrossPlotSAC.pdf")
grid.arrange(avehist,aveR2,minhist,minR2,maxhist,maxR2, nrow = 3)
dev.off()


