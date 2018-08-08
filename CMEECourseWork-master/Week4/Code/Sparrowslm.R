

#Sparrows Linear models

library(ggplot2)

SparrowsData = as.data.frame(read.table("../Data/SparrowSize.txt", header = TRUE))

SparrowsData2= subset(SparrowsData, SparrowsData$Tarsus !="NA")
SparrowsData3<-subset(SparrowsData2,SparrowsData2$Mass !="NA")


plot1 = ggplot(SparrowsDataNoNA, aes(x = Tarsus, y = Mass))+
  geom_point() +
  geom_line(aes(y = predict(lm_Tarsus_Mass)))

lm_Tarsus_Mass = lm(SparrowsData$Tarsus~SparrowsData$Mass, SparrowsData3)
y = predict(lm_Tarsus_Mass, !is.na())

plot(SparrowsData3$Tarsus,SparrowsData3$Mass)
lines(y,SparrowsData3$Mass)


length(y)
length(SparrowsDataNoNA$Mass)

plot1 = ggplot(SparrowsData, aes(x = Tarsus, y = Mass), !is.na(Mass))
  geom_point() +
  geom_line(aes(y = predict(lm_Tarsus_Mass)))





