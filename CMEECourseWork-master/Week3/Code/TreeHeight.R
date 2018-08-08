#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 23th October 2017

#This function evaluates heights of trees from the angle of elevation and distance from the base
#height is distance x tan(angle)

#arguments: angle of elevation
#distance : distance to tree base
#Output: height of the tree, TreeHts.csv


MyData = read.csv("../Data/trees.csv")

TreeHeight = function(distance, angle) {
  radians =  angle*pi/180
  height = distance * tan(radians)
  print(height)
}


Tree.Heights.m = mapply(TreeHeight, MyData$Distance.m, MyData$Angle.degrees)
tmp = cbind(MyData,Tree.Heights.m)
write.csv(tmp, "../Results/TreeHts.csv")

