

library(reshape)
#dont need this - just a beta, all redone in nestmodels
##########################

rich = spec_rich[[1]]
colnames(rich) = c("nest1","nest2","nest3","nest4","nest5")
melted_r = melt(rich)
cum_rich = cum_rich_all[[1]]
colnames(cum_rich) = c("nest1","nest2","nest3","nest4","nest5")
melted_cr = melt(cum_rich) 


ggplot(melted_r, (aes_string(x='variable', y='value')) )+
  geom_boxplot()+
 geom_point(aes(x=variable,y=value), data = melted_cr) 

#########################

