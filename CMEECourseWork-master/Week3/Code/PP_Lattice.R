#Chapter 9, 9.1.2
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 26th October 2017

rm(list = ls())

Mydf = read.csv("../Data/EcolArchives-E089-51-D1.csv")

library(lattice)
# plot graphs and output to pdfs
pdf("../Results/Prey_Lattice.pdf", # Open blank pdf page
    11.7, 8.3)
densityplot(~log(Prey.mass)| Type.of.feeding.interaction, data = Mydf)
dev.off()

pdf("../Results/Predator_Lattice.pdf", # Open blank pdf page
    11.7, 8.3)
densityplot(~log(Predator.mass)| Type.of.feeding.interaction, data = Mydf)
dev.off()
pdf("../Results/SizeRatio_Lattice.pdf", # Open blank pdf page
    11.7, 8.3)

densityplot(~log(Prey.mass/Predator.mass)| Type.of.feeding.interaction, data = Mydf)
dev.off()


# are all masses in same units?
masses_not_g =  subset(Mydf, Prey.mass.unit != "g")
length(unique(masses_not_g$Type.of.feeding.interaction)) # = 3, so there are three groups for mass of mg, that makes above calcs wrong.

#if prey mass unit = mg, x prey mass by 1E-3
l = length(Mydf$Prey.mass)
for (i in 1:l){
  if (Mydf$Prey.mass.unit[i] == "mg") {
    Mydf$Prey.mass[i] = Mydf$Prey.mass[i] * 1e-3
  }
}
#tapplys to get means and medians across feeding interaction
# ps, log = ln, natural log
groups = as.factor(Mydf$Type.of.feeding.interaction)

Prey.mass.means = tapply(log(Mydf$Prey.mass), groups, mean)

Predator.mass.means = tapply(log(Mydf$Predator.mass), groups, mean)

PredPrey.mass.means = tapply(log(Mydf$Predator.mass/Mydf$Prey.mass), groups, mean)

Prey.mass.median = tapply(log(Mydf$Prey.mass), groups, median)

Predator.mass.median = tapply(log(Mydf$Prey.mass), groups, median)

PredPrey.mass.median = tapply(log(Mydf$Predator.mass/Mydf$Prey.mass), groups, mean)
  
stats = rbind(Prey.mass.means, Predator.mass.means, PredPrey.mass.means, Prey.mass.median, Predator.mass.median, PredPrey.mass.median)

#write.csv(stats, "../Results/PP_Results.csv")
#another output with headers from StackO

write.table_with_header <- function(x, file, header, ...){
  cat(header, '\n',  file = file)
  write.table(x, file, append = T, ...)
}

#Note that append is ignored in a write.csv call, so you simply need to call
header = "All values are natural log"
write.table_with_header(stats,"../Results/PP_Results.csv",header,sep=',') 



