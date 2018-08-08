rm(list = ls())
cat("\014")

library(dplyr)

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

get_sacs = function(s){
  site = Data_Yr2_veg%>%filter(SITE == s)
  num_plots = max(site$PLOT)
  rand_f = data.frame()
  #browser()
  for (k in 1:1000){
    path = sample(1:num_plots,num_plots, replace = FALSE)
    f = vector()
    this_plot = unique(site%>%filter(PLOT == path[1])%>%select(BRC_number))
    for (j in 1:(num_plots-1)){
      #browser()
      next_plot = unique(site%>%filter(PLOT == path[j+1])%>%select(BRC_number))
      f[j] = nrow(this_plot)
      combo_list = full_join(this_plot,next_plot)
      f[j+1] = nrow(combo_list)
      this_plot = combo_list
      }
    rand_f = rbind(rand_f,f)
  }
  return(rand_f)
}

get_aves = function(sacs_df){
  aves = colMeans(sacs_df)
  return(aves)
}

get_z = function(a){
  l = length(a)
  end = 200*l
  areas = seq(200,end,200)
  df = as.data.frame(cbind(log(areas),log(a)))
  colnames(df) = c("logarea","logfreq")
  rownames(df) = c()
  model = lm(logfreq~logarea,df)
  z = model$coefficients[2][[1]]
  return(z)
}

rand_zs = vector()

for (i in 1:103){
  m = get_sacs(i)
  a = get_aves(m)
  z = get_z(a)[[1]]
  rand_zs = c(rand_zs,z)
}
 
write.csv(rand_zs, "../Data/rand_zs.csv")  
