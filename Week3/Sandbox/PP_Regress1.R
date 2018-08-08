# Practical Chapter 9
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 20th October 2017

#The program reads in data with predator and prey masses, type of feeding, eg insectivorous, piscivororous, and predator life stage, eg, adult, larva etc. 
# The last two are in two columns of factors with 5 and 6 levels respectively. The task is to produce linear models FOR EACH predator lifestage, BY Feeding interaction.
# I.e., subset by Type.of.feeding.interaction. The subset this by Predator.lifestae. Then plot a linear model for each. Potentially 30, but not every lifestage is present in
#each feeding interaction

#Got desperate to have something to upload for coursework, so wrote everything out in full!!!


library(ggplot2)
#library(dplyr)
#library(magrittr)
#library(tidyr)
#library(sqldf)
rm(list = ls())

Mydf = as.data.frame(read.csv("../Data/EcolArchives-E089-51-D1.csv"))

pdf("../Results/PP_Regress.pdf",  11.7, 8.3)
plot = ggplot(Mydf, aes(x = log(Prey.mass), y = log(Predator.mass) ) )+
     geom_point(aes(col = Predator.lifestage), shape = 3)+
    stat_smooth(method = "lm", aes(col = Predator.lifestage), fullrange = TRUE) +
    facet_grid(Type.of.feeding.interaction ~ .)
print(plot)
dev.off()


# convert to g and log everything 
l = length(Mydf$Prey.mass)
for (i in 1:l){
  if (Mydf$Prey.mass.unit[i] == "mg") {
    Mydf$Prey.mass[i] = Mydf$Prey.mass[i] * 1e-3
  }
}


Feed1 = subset(Mysmallerdf, Mysmallerdf$Type.of.feeding.interaction == 1)
Feed2 = subset(Mysmallerdf, Mysmallerdf$Type.of.feeding.interaction ==2)
Feed3 = subset(Mysmallerdf, Mysmallerdf$Type.of.feeding.interaction == 3)
Feed4 = subset(Mysmallerdf, Mysmallerdf$Type.of.feeding.interaction == 4)
Feed5 = subset(Mysmallerdf, Mysmallerdf$Type.of.feeding.interaction == 5)

ModelFeed1Life4 = lm(Feed1$Predator.mass ~ Feed1$Prey.mass)

###
Feed2Life1 = subset(Feed2, Feed2$Predator.lifestage ==1)
Feed2Life2 = subset(Feed2, Feed2$Predator.lifestage == 2)
Feed2Life4 = subset(Feed2, Feed2$Predator.lifestage == 4)
Feed2Life5 = subset(Feed2, Feed2$Predator.lifestage == 5)
Feed2Life6 = subset(Feed2, Feed2$Predator.lifestage == 6)

ModelFeed2Life1 = lm(Feed2Life1$Predator.mass~Feed2Life1$Prey.mass)
ModelFeed2Life2 = lm(Feed2Life2$Predator.mass~Feed2Life2$Prey.mass)
ModelFeed2Life4 = lm(Feed2Life4$Predator.mass~Feed2Life4$Prey.mass)
ModelFeed2Life5 = lm(Feed2Life5$Predator.mass~Feed2Life5$Prey.mass)
ModelFeed2Life6 = lm(Feed2Life6$Predator.mass~Feed2Life6$Prey.mass)

####
Feed3Life1 = subset(Feed3, Feed3$Predator.lifestage ==1)
Feed3Life2 = subset(Feed3, Feed3$Predator.lifestage == 2)
Feed3Life3 = subset(Feed3, Feed3$Predator.lifestage == 3)
Feed3Life4 = subset(Feed3, Feed3$Predator.lifestage == 4)
Feed3Life6 = subset(Feed3, Feed3$Predator.lifestage == 6)

ModelFeed3Life1 = lm(Feed3Life1$Predator.mass~Feed3Life1$Prey.mass)
ModelFeed3Life2 = lm(Feed3Life2$Predator.mass~Feed3Life2$Prey.mass)
ModelFeed3Life3 = lm(Feed3Life3$Predator.mass~Feed3Life3$Prey.mass)
ModelFeed3Life4 = lm(Feed3Life4$Predator.mass~Feed3Life4$Prey.mass)
ModelFeed3Life6 = lm(Feed3Life6$Predator.mass~Feed3Life6$Prey.mass)

###

Feed4Life1 = subset(Feed4, Feed4$Predator.lifestage ==1)
Feed4Life2 = subset(Feed4, Feed4$Predator.lifestage == 2)
Feed4Life3 = subset(Feed4, Feed4$Predator.lifestage == 3)
Feed4Life4 = subset(Feed4, Feed4$Predator.lifestage == 4)
Feed4Life5 = subset(Feed4, Feed4$Predator.lifestage == 5)
Feed4Life6 = subset(Feed4, Feed4$Predator.lifestage == 6)

ModelFeed4Life1 = lm(Feed4Life1$Predator.mass~Feed4Life1$Prey.mass)
ModelFeed4Life2 = lm(Feed4Life2$Predator.mass~Feed4Life2$Prey.mass)
ModelFeed4Life3 = lm(Feed4Life3$Predator.mass~Feed4Life3$Prey.mass)
ModelFeed4Life4 = lm(Feed4Life4$Predator.mass~Feed4Life4$Prey.mass)
ModelFeed4Life5 = lm(Feed4Life5$Predator.mass~Feed4Life5$Prey.mass)
ModelFeed4Life6 = lm(Feed4Life6$Predator.mass~Feed4Life6$Prey.mass)

###

Model5Feed1 = lm(Feed5$Predator.mass~Feed5$Prey.mass)

############################



