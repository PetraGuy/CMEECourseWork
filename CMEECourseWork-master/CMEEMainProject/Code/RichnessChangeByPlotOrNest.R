

library(reshape)
# get the change across nests and across plots - dS/dA and dS/dP
cum_rich_all = readRDS("CumulateveRichness.RDS")
CompleteSiteLevelvars = read.csv("../Data/CompleteSiteLevelVars.csv")

range_df = data.frame()
for(i in 1:103){
  #browser()
  site = cum_rich_all[[i]]
  l = nrow(site)
  nestrange = vector()
  for ( j in 1:l){
    nestrange[j] = site[j,5] - site[j,1]
  }
  maxnest = max(nestrange)
  plotRange = max(site[,5])-min(site[,5])
  next_row = cbind(maxnest,plotRange)
  range_df = rbind(range_df,next_row)
}
colnames(range_df) = c("nest_range", "plot_range")


ggplot(melt(range_df), aes(x = variable, y = value,fill = variable))+
  geom_boxplot(width = 0.4 )+
  ylab("Richness")+
  xlab("Change in Richness")


#ggplot(range_df, aes(nest_range))+
# geom_histogram(binwidth = 1, col = "black", fill = "grey")  

#ggplot(range_df, aes(plot_range))+geom_histogram(binwidth = 1, col = "black", fill = "grey")
melted =  melt(range_df)
ggplot(melted, aes(value, fill = variable) )+ 
  geom_histogram(position = "identity", alpha = 0.2)+
  ggtitle("Change in richness due to area and plot")

######


dSdA = vector()
for (i in 1:103){
  site = cum_rich_all[[i]]
  dSdA[i] = max(site[,5]) - min(site[,5])
}

dSdH = CompleteSiteLevelvars$Richness

data = as.data.frame(cbind(dSdA,dSdH))

ggplot(melt(data), aes(x = variable, y = value,fill = variable))+
  geom_boxplot(width = 0.4 )+
  ylab("Richness")+
  xlab("Change in Richness")

