# Practical Chapter 9

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 27th October 2017

# Trying loops again

library(ggplot2)
library(plyr)
rm(list = ls())

Mydf = as.data.frame(read.csv("../Data/EcolArchives-E089-51-D1.csv"))

pdf("../Results/PP_Regress.pdf",  11.7, 8.3)
plot = ggplot(Mydf, aes(x = log(Prey.mass), y = log(Predator.mass) ) )+
  geom_point(aes(col = Predator.lifestage), shape = 3)+
  stat_smooth(method = "lm", aes(col = Predator.lifestage), fullrange = TRUE) +
  facet_grid(Type.of.feeding.interaction ~ .)
print(plot)
dev.off()

# convert to g mg
l = length(Mydf$Prey.mass)
for (i in 1:l){
  if (Mydf$Prey.mass.unit[i] == "mg") {
    Mydf$Prey.mass[i] = Mydf$Prey.mass[i] * 1e-3
  }
}

#Subset by Feeding interaction.
keyFeedGroups = as.character(unique(Mydf$Type.of.feeding.interaction))
keyPredLifeStage = as.character(unique(Mydf$Predator.lifestage))
numsFeedgroups = as.numeric(unique(Mydf$Type.of.feeding.interaction))
key_to_Feedlevels = rbind(keyFeedGroups, numsFeedgroups) #so i know what i is!


Mydfs_FeedGroups =  vector(mode = "list")
for (i in 1:length(keyFeedGroups)){
Mydfs_FeedGroups[[i]]= subset(Mydf, Mydf$Type.of.feeding.interaction == keyFeedGroups[i])
}

stages =  vector(mode = "list")
#How many Life stages in each Feed group?
for (i in 1:length(Mydfs_FeedGroups)){
  stages[i] = sum(count(unique(Mydfs_FeedGroups[[i]]$Predator.lifestage))$freq)
}


get_LifeStages = function(dataframe){
  LifeStages = as.character(unique(dataframe$Predator.lifestage))
}
get_numberLifeStages = function(dataframe){
  No_Stages = sum(count(unique(dataframe$Predator.lifestage))$freq)
}

Lms= vector(mode = "list")
coefs = 0

for (i in 1:length(Mydfs_FeedGroups)){
  #browser()
  n = get_numberLifeStages(Mydfs_FeedGroups[[i]])
  
  St = get_LifeStages(Mydfs_FeedGroups[[i]])
 
  for (j in 1:n){
    
    Model = lm(Mydfs_FeedGroups[[i]]$Predator.mass ~ Mydfs_FeedGroups[[i]]$Prey.mass, subset = (Mydfs_FeedGroups[[i]]$Predator.lifestage==St[j]))
    coefs = cbind(c(Model[[1]][[1]], Model[[2]][[1]]))
    print(coefs)
    Lms = rbind(Lms, c(coefs[[1]],coefs[[2]]))
    print(Model)
  }
  
 }
##the above loop works, but Lms is only the intercept and gradient. Would need summary and more subsetting to
#get R2, F and p. Summary itself produces horrid output, so need to get at the elements of it, but it was tricky to work out what they were
#Partly because the Model seemed excessively nested. The above for loop shows where I get confuesd with indexing in R - I seem to turn everything
#into a nested list of lists and I can work out how to get at the data anymore.
#I cant work out how to rbind new summaries in a sensible way.
#Combine the file output function of PP_Rgress4 with this loop??
