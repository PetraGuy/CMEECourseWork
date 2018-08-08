# Practical Chapter 9

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 27th October 2017

#The program reads in data with predator and prey masses, type of feeding, eg insectivorous, piscivororous, and predator life stage, eg, adult, larva etc. 
# The last two are in two columns of factors with 5 and 6 levels respectively. The task is to produce linear models FOR EACH predator lifestage, BY Feeding interaction.
# I.e., subset by Type.of.feeding.interaction. The subset this by Predator.lifestae. Then plot a linear model for each. Potentially 30, but not every lifestage is present in
#each feeding interaction

library(dplyr) # need for filter
library(ggplot2)
library(plyr) # need for count

rm(list = ls())

Mydf = as.data.frame(read.csv("../Data/EcolArchives-E089-51-D1.csv"))

#This plot needed for coursework. 
pdf("../Results/PP_Regress.pdf",  11.7, 8.3)
plot = ggplot(Mydf, aes(x = log(Prey.mass), y = log(Predator.mass) ) )+
  geom_point(aes(col = Predator.lifestage), shape = 3)+
  stat_smooth(method = "lm", aes(col = Predator.lifestage), fullrange = TRUE) +
  facet_grid(Type.of.feeding.interaction ~ .)
print(plot)
dev.off()

# convert to mg to g - a few of the data are in mg.
l = length(Mydf$Prey.mass)
for (i in 1:l){
  if (Mydf$Prey.mass.unit[i] == "mg") {
    Mydf$Prey.mass[i] = Mydf$Prey.mass[i] * 1e-3
  }
}

# take logs of the two mass columns which will be used in the linear model and makes later argument bit neater

logsPredMass = log(Mydf$Predator.mass)
Mydf[["Predator.mass"]] = logsPredMass
logsPreyMass = log(Mydf$Prey.mass)
Mydf[["Prey.mass"]] = logsPreyMass

# using dplyr is fine - but need to work out how to then specify what data I have ended up with in the model. Eg, need to pass the Feeding.interaction and Predator.lifestage
#info through as well. Didn't work that bit out.

#models = Mydf %>%  filter(Type.of.feeding.interaction == "insectivorous") %>%  group_by(Predator.lifestage) %>%  do(lm(log(Predator.mass) ~ log(Prey.mass)))
#means = Mydf %>% group_by(Type.of.feeding.interaction) %>% group_by(Predator.lifestage, add = TRUE) %>%   summarize(m = mean(Predator.mass)) #%>%  assign("bar", .)
#Modelspiped = Mydf %>% group_by(Type.of.feeding.interaction) %>% group_by(Predator.lifestage, add = TRUE) %>%  do(fits = lm(log(Predator.mass) ~ log(Prey.mass), data = . ))


#Start again using filters. Part of the issue is that I am trying to automate everything, as if I dont know what each grpup will throw out, so the 
#program generic. Difficulty is that I then need to introduce separate functions to assess each subset is created. Not all subgroups are the same.

# this makes output of summary(linearmodel) look ok, otherwise nonsense when written to csv, but still looks pretty nasty.
convertoutput <-function(x){ 
  res<-c(paste(as.character(summary(x)$call),collapse=" "),
         x$coefficients[1],
         x$coefficients[2],
         length(x$model),
         summary(x)$coefficients[2,2],
         summary(x)$r.squared,
         summary(x)$adj.r.squared,
         summary(x)$fstatistic,
         pf(summary(x)$fstatistic[1],summary(x)$fstatistic[2],summary(x)$fstatistic[3],lower.tail=FALSE))
  names(res)<-c("call","intercept","slope","n","slope.SE","r.squared","Adj. r.squared",
                "F-statistic","numdf","dendf","p.value")
  return(res)} 

# Just to make a list of whats in those columns because I didnt know what the factor where, and I hoped I could
#use this to reference some FOR loops

keyFeedGroups = as.character(unique(Mydf$Type.of.feeding.interaction))
keyPredLifeStage = as.character(unique(Mydf$Predator.lifestage))

#Losing the will to live, I have just filtered to get the 5 feeding groups. Would rather have made
#FOR loops than hard coded for each type

insectivors = filter(Mydf, Type.of.feeding.interaction == "insectivorous")
pred_pisc = filter(Mydf, Type.of.feeding.interaction ==  "predacious/piscivorous")
piscivorous =  filter(Mydf, Type.of.feeding.interaction ==  "piscivorous")
planktivorous = filter(Mydf, Type.of.feeding.interaction ==  "planktivorous")
predacious = filter(Mydf, Type.of.feeding.interaction ==  "predacious")

#how many lifestages in each subset above and what are they
#Thought I might use this in Loops

get_LifeStages = function(dataframe){
  LifeStages = as.character(unique(dataframe$Predator.lifestage))
}
get_numberLifeStages = function(dataframe){
  No_Stages = sum(count(unique(dataframe$Predator.lifestage))$freq)
}
### how many predator life stages in each feed interaction
insectivorstages = get_LifeStages(insectivors)
pred_piscstages = get_LifeStages(pred_pisc)
piscivorousstages =  get_LifeStages(piscivorous)
planktivorousstages = get_LifeStages(planktivorous)
predaciousstages = get_LifeStages(predacious)
# need to loop for the number of stages times, over the stages to get the required models.

# This whole section should be one loop, FOR stages in keyFeedGroups. 
# The write.table/append gives very nasty output


n = length(piscivorousstages)
for (i in 1:n){
  #browser()
    model = lm(piscivorous$Predator.mass ~ piscivorous$Prey.mass, 
            subset = (piscivorous$Predator.lifestage == piscivorousstages[i]))
    title = c("predator life stage ", piscivorousstages[i], "piscivorous")
    output = convertoutput(model)
    fullout = rbind(title, output)
    write.table( fullout,  
                 file="../Results/PP_Regress.csv", 
                 append = T, 
                 sep=',', 
                 row.names=T, 
                 col.names=T )
  
}


n = length(insectivors)
for (i in 1:n){
  #browser()
  model = lm(insectivors$Predator.mass ~ insectivors$Prey.mass, 
             subset = (insectivors$Predator.lifestage == insectivorstages[i]))
  title = c("predator life stage ", insectivorstages[i], "insectivorous")
  output = convertoutput(model)
  fullout = rbind(title, output)
  write.table( fullout,  
               file="../Results/PP_Regress.csv", 
               append = T, 
               sep=',', 
               row.names=T, 
               col.names=T )
  
}


n = length(pred_piscstages)
for (i in 1:n){
  #browser()
  model = lm(pred_pisc$Predator.mass ~ pred_pisc$Prey.mass, 
             subset = (pred_pisc$Predator.lifestage == pred_piscstages[i]))
  title = c("predator life stage ", pred_piscstages[i], "pred_piscivorous")
  output = convertoutput(model)
  fullout = rbind(title, output)
  write.table( fullout,  
               file="../Results/PP_Regress.csv", 
               append = T, 
               sep=',', 
               row.names=T, 
               col.names=T )

}

n = length(predaciousstages)
for (i in 1:n){
  #browser()
  model = lm(predacious$Predator.mass ~ predacious$Prey.mass, 
             subset = (predacious$Predator.lifestage == predaciousstages[i]))
  title = c("predator life stage ", predaciousstages[i], "pred_piscivorous")
  output = convertoutput(model)
  fullout = rbind(title, output)
  write.table( fullout,  
               file="../Results/PP_Regress.csv", 
               append = T, 
               sep=',', 
               row.names=T, 
               col.names=T )
  
}



