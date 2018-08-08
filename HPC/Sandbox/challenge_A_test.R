
#Calculate species ricchness at each time interval and store the values.
#Repeat.
#Take average richness at each time step.

t = 200
v = 0.1
repeats = 100

#get two starting communities
comm_max = initialise_max(100)
comm_min = initialise_min(100)

#initialise richness vector
richness_max_vect = vector(length = t)
richness_min_vect = vector(length = t)

#initialise df for storing output for laer calculations
richness_df_max = data.frame()
richness_df_min = data.frame()


for (i in 1:repeats) {
  
#get times series of richness for them both - repeat this and store values for each repeat
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)

  richness_df_max = rbind(richness_df_max,rich_max)
  richness_df_min = rbind(richness_df_min,rich_min)
 
  
}
#can be useful to name cols - otherwise first row can become a header
cols = c(1:t)
colnames(richness_df_max) = cols
colnames(richness_df_min) = cols
#calcualte the standard deviations and averages using the dataframe of stored richnesses
#because each run is a row the cols are used for sd and ave.
sd_max = sqrt(apply(richness_df_max,2,var))
sd_min = sqrt(apply(richness_df_min,2,var))
avg_max = colSums(richness_df_max)/repeats
avg_min = colSums(richness_df_min)/repeats

#for 97.2% CI we need 98.6th percentile = z of 2.2
CI_upper_max = avg_max + 2.2*sd_max
CI_lower_max = avg_max - 2.2*sd_max

CI_upper_min = avg_min + 2.2*sd_min
CI_lower_min = avg_min - 2.2*sd_min

#plot the graphs

x = (1:t)
titles = c("Maximum initial richness", "Minimum initial richness")
y =  cbind(avg_max, avg_min)

matplot (x, y, pch = 20, col = 1:2, xlim = c(0,t), ylim = c(0,100),
         xlab = "time", ylab = "Average species richness",
         main = "Average species richness for 500 repeats of neutral_time_series_speciation, initial community size 100")
legend(50,
       100,
       legend = titles,
       col = 1:2,
       lty = 1:4)
      
lines(CI_upper_max)
lines(CI_lower_max)
lines(CI_upper_min, col = "red")
lines(CI_lower_min, col = "red")










                      