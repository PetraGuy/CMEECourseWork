rm(list = ls())
cat("\014")
#plot level SAC


plotrichness = read.csv("../Data/plot_rich.csv")
#plot richness includes the missing plots as 0's
plotrichness = plotrichness[,-1]

#Input all the ground flora data
Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")

# the  bryophytes, lichen etc have already been removed from veg codes csv so join this with ground cover to eliminate bryophytes from counts


colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)
# now using Dat_Yr2_veg means analysis is carried out without bryophytes


missing_sites = c(15,17,38,46,46,46,59,59,  63,74,77, 83,90,98,98,98, 59,79,79,91,99,99,99,48,48,29,29,82,77)
missing_plots = c( 1,15, 9, 2, 8,16, 1, 8,  14,16, 1, 16, 2,11,12,14,  1, 2, 3,15,10,13,14, 7,10, 2, 1, 7,16)
missing = as.data.frame(cbind(missing_sites,missing_plots))
colnames(missing) = c("Site","Plot")

# calculate SAC starting from least rich plot
#select plot with least amount of species
plotvect = c(1:16)
f = vector()

for (i in 1:103)
  sitevector = plotrichness[i,] # vector of richnesses of each plot for site i
  start = which(sitevector==min(sitevector)) # take plot with min richness
  siteveg = Data_Yr2_veg%>%filter(SITE == i) #get species data for site i
  startplot = siteveg%>%filter(PLOT == start) # get species data for min plot
  f[1] = length(unique(startplot$Species)) # first frequency  is just richness of min plot
  restofplots = siteveg%>%filter(PLOT != start) # take out the min plot
  plotvect = plotvect[-start] # take out the plot number of min plot
  j = 1
  while (length(plotvect != 0)){
    #browser()
    j = j+1
      #inner joins of all other plots to min plot, tmp 2 lists all species in
     #next plot that were not in min plot.
      tmp2 = restofplots%>%group_by(PLOT)%>% inner_join(.,startplot, by = "Species") 
      # this just gives the numbers of new species
      nos = as.data.frame(tmp2%>%group_by(PLOT.x)%>%summarise(length(unique(Species))))
      #find location i.e.plot numer of biggest new no. of species
      position = which.max(nos[,2])
      nextplot = nos[,1][position]
      # make the next frequency the biggest jump
      f[j] = max(nos[,2]) #next f along, the extra species only
      #remove that plot number  
      plotvect = plotvect[which(plotvect!=nextplot)]
      # remove that plot species list
      restofplots = filter(siteveg, PLOT %in% plotvect)
    }
  
